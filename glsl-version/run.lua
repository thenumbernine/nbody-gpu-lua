#!/usr/bin/env luajit
local ffi = require 'ffi'
local sdl = require 'ffi.req' 'sdl'
local math = require 'ext.math'
local table = require 'ext.table'
local vector = require 'ffi.cpp.vector'
local template = require 'template'
local GLTex1D = require 'gl.tex2d'
local GLTex2D = require 'gl.tex2d'
local GLPingPong = require 'gl.pingpong'
local GLProgram = require 'gl.program'
local GLArrayBuffer = require 'gl.arraybuffer'
local GLAttribute = require 'gl.attribute'
local GLKernelProgram = require 'gl.kernelprogram'
local glreport = require 'gl.report'
local gl = require 'gl'
local clnumber = require 'cl.obj.number'	-- TODO since gl needs this too, and cl depends on gl for interop, how about move this to gl?
local ig = require 'imgui'
local vec2i = require 'vec-ffi.vec2i'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'

local matrix_ffi = require 'matrix.ffi'
matrix_ffi.real = 'float'	-- default matrix_ffi type

local App = require 'imguiapp.withorbit'()

local fieldDim = 64
local count = fieldDim * fieldDim
local update = true

local channelsPerFormat = {
	[gl.GL_RGB] = 3,
	[gl.GL_RGBA] = 4,
}

local function dataFromLambda(width, height, format, gltype, callback)
	local ctype = assert(GLTex2D.ctypeForGLType[gltype], "couldn't determine size of type "..tostring(gltype))
	local numChannels = assert(channelsPerFormat[format], "couldn't determine channels for format "..tostring(format))
	local ptr = ffi.new(ctype..'[?]', width * height * numChannels)
	for j=0,height-1 do
		for i=0,width-1 do
			local e = i + width * j
			local chs = table.pack(callback(i, j))
			for ch=0,numChannels-1 do
				ptr[ch + numChannels * e] = chs[ch+1] or 0
			end
		end
	end
	return ptr
end

local function createFieldPingPong(args)
	args = args or {}
	local width = fieldDim
	local height = fieldDim
	local format = args.format or gl.GL_RGBA
	local gltype = args.type or gl.GL_FLOAT
	local internalFormat = args.internalFormat or gl.GL_RGBA32F
	local data = args.data
	if type(data) == 'function' then
		data = dataFromLambda(width, height, format, gltype)
	end
	return GLPingPong{
		width = width,
		height = height,
		internalFormat = internalFormat,
		type = gltype,
		format = format,
		data = data,
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_NEAREST,
	}
end


local uvAttr
local posTexs
local velTexs

--[[
ok for some real world values ...
G = 6.67384e-11 m^3 / (kg s^2)

using new units ...
1 lyr = 9.4607304725808e+15 m
1 ux = 1e+5 lyr = 9.4607304725808e+20 m		-- diameter of the milky way
1 day = 60 * 60 * 24 s = 8.64e+4 s
1 year = 365.25 days = 3.15576e+7 s
1 ut = 2.3e+8 years = 7.258248e+15 s 		-- time for the solar system to orbit the galaxy
1 um = 1 solar mass = 1.989e+30 kg
G = 8.2584809222832e-12 ux^3 / (um ut^2)	-- using units of sun orbit distance, sun orbit time, and sun mass ... same.  I bet Kepler has a law about this.

what if we use the sun's orbit distance also 
1 ux = 2.58e+4 lyr = 2.4408684619258e+20 m		-- sun orbit distance in milky way ... https://www.universetoday.com/148997/a-new-measurement-puts-the-sun-2000-light-years-closer-to-the-center-of-the-milky-way/
G = 4.8088480226311e-10 ux^3 / (um ut^2)	-- it gets closer to G
sun velocity = 2.51e+5 m/s = 7.4638198510813 ux / ut

how about Earth parameters?
1 ux = 1 au = 1.495978707e+11 m				-- average distance of sun to earth
1 ut = 1 year = 3.15576e+7 s				-- average time of earth orbit
1 um = 5.9722e+24 kg						-- weight of Earth
G = 1.18560994016190149433383205224146195178036577999592e-04 ux^3 / (um ut^2)	-- G in Earth orbit units

how about the moon?
1 ux = 1 ld = 3.84399e+8 m
1 ut = 1 month = 27.321662 days = 2360591.5968 s
1 um = 7.349e+22 kg
G = 4.81170511702339376292059114348376169800758361816406e-1

so our "gravitation parameter" G_orbit = G * mass_in_kg * orbit_time_in_s^2 / orbit_dist_in_m^3
--]]
dt = 1
--gravConst = 4.8088480226311e-10	-- in sun units
gravConst = 1e-10
--gravConst = 1.185609940161901e-4	-- Earth orbit units
--gravConst = .5	-- Moon orbit units

displayScale = 1000
pointSize = 2
gravDistEpsilon = 1e-7

r0min = 1
r0max = 2

local function reset()
	do
		local width = fieldDim
		local height = fieldDim
		local format = gl.GL_RGBA
		local gltype = gl.GL_FLOAT
		uvCPUMem = dataFromLambda(width, height, format, gltype, function(i,j)
			return (i+.5)/width, (j+.5)/height
		end)
		-- [[
		uvBuf = GLArrayBuffer{
			size = count * ffi.sizeof'vec2f_t',
			data = uvCPUMem,
			usage = gl.GL_STATIC_DRAW,
		}
		uvAttr = GLAttribute{
			buffer = uvBuv,
			size = 2,
			type = gl.GL_FLOAT,
			stride = ffi.sizeof'vec2f_t',
			offset = 0,
		}
		--]]
	end

	local posData = vector('vec4f_t', count)
	local velData = vector('vec4f_t', count)

	local totalMass = 0
	local posv = ffi.cast('vec4f_t*', posData.v)	-- TODO cast within vector:begin() ?
	local velv = ffi.cast('vec4f_t*', velData.v)	-- TODO cast within vector:begin() ?
	for i=0,count-1 do
--[[
		return (u+.5)/fieldDim*2-1,(v+.5)/fieldDim*2-1, 0, 0
--]]
--[[
		local v = {math.random()*2-1, math.random()*2-1, math.random()*2-1}
		local l = v[1]*v[1] + v[2]*v[2] + v[3]*v[3]
		if l >= 1 then 
			v[1] = v[1] / l
			v[2] = v[2] / l
			v[3] = v[3] / l
		end
		return v[1], v[2], v[3], 1
--]]
-- [[
		local r = math.random() * (r0max - r0min) + r0min
		local phi = math.random() * 2 * math.pi
		local z = .1 * (math.random() * 2 - 1) * math.exp(-r*r)
		posv[0].x = math.cos(phi) * r
		posv[0].y = math.sin(phi) * r
		posv[0].z = z
		local mass = 10^(math.random() * 2 - 1)
		posv[0].w = mass
		totalMass = totalMass + mass
--]]
		
		posv = posv + 1
		velv = velv + 1
	end
	
	local posv = ffi.cast('vec4f_t*', posData.v)	-- TODO cast within vector:begin() ?
	local velv = ffi.cast('vec4f_t*', velData.v)	-- TODO cast within vector:begin() ?
	for i=0,count-1 do
		-- Kepler's 1-2-3 law
		-- G m_sum = omega^2 r^3
		local r = math.sqrt(posv[0].x * posv[0].x + posv[0].y * posv[0].y)
		
		-- P^2 = (2 pi)^2 / (G (m1 + m2)) a^3
		-- (P / 2 pi)^2 = a^3 / (G (m1 + m2))
		-- G (m1 + m2) = (2 pi / P)^2 * a^3
		-- pos = r cis (t 2 pi / P) = r cis (omega t) for omega = 2 pi / P
		-- G (m1 + m2) = omega^2 * a^3
		-- vel = i r omega cis (omega t) = i omega pos
		-- by the 1-2-3 law: G (m1 + m2) = omega^2 * a^3 => omega = sqrt(G (m1 + m2) / a^3)
		local omega = math.sqrt(gravConst * totalMass / (r * r * r))
		velv[0].x = -omega * posv[0].y
		velv[0].y = omega * posv[0].x
		velv[0].z = 0
		velv[0].w = 0
	
		posv = posv + 1
		velv = velv + 1
	end
	posTexs = createFieldPingPong{data = posData.v}
	velTexs = createFieldPingPong{data = velData.v}
end

App.viewDist = 1

messageCallback = function(
	source,		-- GLenum
	gltype,		-- GLenum
	id,			-- GLuint
	severity,	-- GLenum
	length,		-- GLsizei
	msg,		-- GLchar const *
	userParam	-- void const * 
)
	print('gl error', source, gltype, id, severity, ffi.string(msg, length))
end
messageCallbackClosure = ffi.cast('GLDEBUGPROC', messageCallback)

function App:initGL(...)
	App.super.initGL(self, ...)

--[[ crashing
	gl.glDebugMessageCallback(messageCallbackClosure, nil)
	gl.glEnable(gl.GL_DEBUG_OUTPUT)
--]]

	self.view.znear = .1
	self.view.zfar = 100

	reset()

	gradTex = require 'gl.gradienttex'(256, 
--[[ rainbow or heatmap or whatever
		{
			{0,0,0,0},
			{1,0,0,1/6},
			{1,1,0,2/6},
			{0,1,1,3/6},
			{0,0,1,4/6},
			{1,0,1,5/6},
			{0,0,0,6/6},
		},
--]]
-- [[ sunset pic from https://blog.graphiq.com/finding-the-right-color-palettes-for-data-visualizations-fcd4e707a283#.inyxk2q43
		table{
			vec4f(22,31,86,255),
			vec4f(34,54,152,255),
			vec4f(87,49,108,255),
			vec4f(156,48,72,255),
			vec4f(220,60,57,255),
			vec4f(254,96,50,255),
			vec4f(255,188,46,255),
			vec4f(255,255,55,255),
		}:map(function(c,i)
			return {(c/255):unpack()}
		end),
--]]
		false
	)

	simPosShader = GLKernelProgram{
		texs = {'postex', 'veltex'};
		code = [[
uniform float dt;
void main() {
	vec4 vpos = texture2D(postex, pos);
	vec4 vvel = texture2D(veltex, pos);
	vpos.xyz += vvel.xyz * dt;
	// preserve vpos.w == mass
	gl_FragColor = vpos;
}
]],
	}

	simVelShader = GLKernelProgram{
		texs = {'postex','veltex'};
		code = template([[
uniform float dt, gravConst, gravDistEpsilon;
void main() {
	vec4 vpos = texture2D(postex, pos);
	vec4 vvel = texture2D(veltex, pos);
	vec3 accumforce = vec3(0.);
	vec2 otc;
	for (otc.x = .5/<?=fieldDim?>; otc.x < 1.; otc.x += 1./<?=fieldDim?>) {
		for (otc.y = .5/<?=fieldDim?>; otc.y < 1.; otc.y += 1./<?=fieldDim?>) {
			vec4 otherpos = texture2D(postex, otc);
			float othermass = otherpos.w;
			vec3 del = otherpos.xyz - vpos.xyz;
			float len = length(del);
			len = max(len, gravDistEpsilon);
			float invlen = 1. / len;
			del *= invlen;
			del *= invlen;
			del *= invlen;
			del *= othermass;
			del *= gravConst;
			accumforce += del;
		}
	}
	vvel.xyz += accumforce * dt;
	gl_FragColor = vvel;
}
]],		{
			fieldDim = clnumber(fieldDim),
		}),
	}

	displayShader = GLProgram{
		vertexCode = [[
#version 460
uniform sampler2D posTex, velTex;
uniform mat4 modelViewProjectionMatrix;
in vec2 uv;
out float magn;
void main() {
	vec4 pos4 = texture(posTex, uv);
	vec4 vel = texture(velTex, uv);
	vec4 pos = vec4(pos4.xyz, 1.);
	magn = length(vel.xyz);//length(pos.xyz);
	gl_Position = modelViewProjectionMatrix * pos;
}
]],
		fragmentCode = [[
#version 460
uniform float displayScale;
uniform sampler1D gradTex;
in float magn;
out vec4 color;
void main() {
	color = texture(gradTex, magn * displayScale);
}
]],
		uniforms = {
			posTex = 0,
			velTex = 1,
			gradTex = 2,
		},
		-- [[
		attrs = {
			uv = uvAttr,
		},
		--]]
		--[[
		attrLocs = {
			uv = 0,
		},
		--]]
	}
	displayShader:useNone()

	gl.glDisable(gl.GL_DEPTH_TEST)
	gl.glDisable(gl.GL_CULL_FACE)
end

local viewport = vec4f()

local modelViewMatrix = matrix_ffi.zeros{4,4}
local projectionMatrix = matrix_ffi.zeros{4,4}
local modelViewProjectionMatrix = matrix_ffi.zeros{4,4}

function App:update(...)
	gl.glClear(gl.GL_COLOR_BUFFER_BIT)


--	gl.glEnable(gl.GL_TEXTURE_2D)
	
	gl.glGetFloatv(gl.GL_VIEWPORT, viewport.s)
	gl.glViewport(0,0,fieldDim,fieldDim)
	gl.glMatrixMode(gl.GL_PROJECTION)
	gl.glPushMatrix()
	gl.glLoadIdentity()
	gl.glOrtho(0,1,0,1,-1,1)
	gl.glMatrixMode(gl.GL_MODELVIEW)
	gl.glPushMatrix()
	gl.glLoadIdentity()
	if update then
		-- TODO instead of swapping color attachments
		-- how about binding the current pos and vel at the same time?
		-- and combining their update shaders?
		velTexs:swap()
		velTexs:draw{
			shader=simVelShader,
			texs={posTexs:cur(), velTexs:prev()},
			uniforms={dt=dt, gravConst=gravConst, gravDistEpsilon=gravDistEpsilon},
		}
		posTexs:swap()
		posTexs:draw{
			shader=simPosShader,
			texs={posTexs:prev(), velTexs:prev()},
			uniforms={dt=dt},
		}
	end
	gl.glViewport(viewport:unpack())
	gl.glMatrixMode(gl.GL_PROJECTION)
	gl.glPopMatrix()
	gl.glMatrixMode(gl.GL_MODELVIEW)
	gl.glPopMatrix()

--	gl.glDisable(gl.GL_TEXTURE_2D)

--[[
	velTexs:cur():bind()
	drawScreenQuad()
	velTexs:cur():unbind()
--]]
--[[
	gl.glPushMatrix()
	gl.glTranslate(-1,0,0)
	posTexs:cur():bind()
	drawScreenQuad()
	posTexs:cur():unbind()
	gl.glPopMatrix()
--]]


	self.view:setup(self.width / self.height)
	gl.glGetFloatv(gl.GL_MODELVIEW_MATRIX, modelViewMatrix.ptr)
	gl.glGetFloatv(gl.GL_PROJECTION_MATRIX, projectionMatrix.ptr)
	modelViewProjectionMatrix:mul(projectionMatrix, modelViewMatrix)

	gl.glPointSize(pointSize)

	displayShader:use()
	gl.glUniformMatrix4fv(displayShader.uniforms.modelViewProjectionMatrix.loc, 1, false, modelViewProjectionMatrix.ptr)
	gl.glUniform1f(displayShader.uniforms.displayScale.loc, displayScale)
	
	posTexs:cur():bind(0)
--	velTexs:cur():bind(1)
--	gradTex:bind(2)

--[[
	uvBuf:bind()
	gl.glEnableVertexAttribArray(0)
	--displayShader.vao:bind()
	gl.glDrawArrays(gl.GL_POINTS, 0, count)
	gl.glDisableVertexAttribArray(0)
	uvBuf:unbind()
--]]
-- [[
	uvBuf:bind()
	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
--	displayShader.vao:bind()
--	gl.glVertexPointer(2, gl.GL_FLOAT, ffi.sizeof'vec2f_t', nil)
	gl.glDrawArrays(gl.GL_POINTS, 0, count)
--	displayShader.vao:unbind()
--	gl.glDisableClientState(gl.GL_VERTEX_ARRAY)
	uvBuf:unbind()
--]]
--[[
	gl.glBegin(gl.GL_POINTS)
	for i=0,count-1 do
		gl.glVertex2f(uvCPUMem[0 + 2 * i], uvCPUMem[1 + 2 * i])
	end
	gl.glEnd()
--]]

	GLTex1D:unbind(2)
	GLTex2D:unbind(1)
--	GLTex2D:unbind(0)
--
	displayShader:useNone()
	gl.glPointSize(1)
glreport'here'	



	--[=[ grid
	local graphSize = 20
	gl.glEnable(gl.GL_BLEND)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE)
	for i=0,2 do
		local e = vec3f() e.s[i] = 1
		local e2 = vec3f() e2.s[(i+1)%3] = 1
		local e3 = vec3f() e3.s[(i+2)%3] = 1
		
		gl.glColor3d(e:unpack())
		gl.glBegin(gl.GL_LINES)
		gl.glVertex3d(0,0,0)
		gl.glVertex3d((e*2):unpack())
		gl.glEnd()
	
		-- [[
		gl.glColor3d(.1,.1,.1)
		for j=-graphSize,graphSize do
			gl.glBegin(gl.GL_LINE_STRIP)
			for k=-graphSize,graphSize do
				gl.glVertex3d((e*j+e2*k):unpack())
			end
		end
		gl.glEnd()
		for j=-graphSize,graphSize do
			gl.glBegin(gl.GL_LINE_STRIP)
			for k=-graphSize,graphSize do
				gl.glVertex3d((e*j+e3*k):unpack())
			end
			gl.glEnd()
		end
		--]]
	end
	gl.glDisable(gl.GL_BLEND)
	--]=]

	-- super does the view setup.  I've already done it earlier.
	--App.super.update(self, ...)
	-- so instead I'll call super.super
	App.super.super.update(self, ...)

	-- fps counter
	self.frame = (self.frame or 0) + 1
	local thisTime = os.time()
	if thisTime ~= self.lastTime then
		if self.lastTime then
			print(self.frame / (thisTime - self.lastTime))
		end
		self.lastTime = thisTime
		self.frame = 0
	end
end

function App:event(event, ...)
	App.super.event(self, event, ...)
	if event.type == sdl.SDL_KEYDOWN then
		if event.key.keysym.sym == ('r'):byte() then
			reset()
		end
	end	
end

function App:updateGUI()
	ig.luatableInputFloatAsText('dt', _G, 'dt')
	ig.luatableInputFloatAsText('G', _G, 'gravConst')
	ig.luatableInputFloatAsText('displayScale', _G, 'displayScale')	-- TODO could auto determine if I wanted to add some reduce kernels ... migth do reduce kernels just for the gravitation COM ...
	ig.luatableInputFloatAsText('pointSize', _G, 'pointSize')
	ig.luatableInputFloatAsText('r0min', _G, 'r0min')
	ig.luatableInputFloatAsText('r0max', _G, 'r0max')
	ig.luatableInputFloatAsText('gravDistEpsilon', _G, 'gravDistEpsilon')
	ig.luatableInputFloatAsText('znear', self.view, 'znear')
	ig.luatableInputFloatAsText('zfar', self.view, 'zfar')
end

return App():run()

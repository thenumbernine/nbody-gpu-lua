#!/usr/bin/env luajit
local cmdline = require 'ext.cmdline'(...)
local ffi = require 'ffi'
local sdl = require 'sdl'
local op = require 'ext.op'
local math = require 'ext.math'
local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local vector = require 'ffi.cpp.vector-lua'
local template = require 'template'
local gl = require 'gl.setup'(cmdline.gl or 'OpenGL')
local GLTypes = require 'gl.types'
local GLGradientTex2D = require 'gl.gradienttex2d'
local GLFramebuffer = require 'gl.framebuffer'
local GLPingPong = require 'gl.pingpong'
local GLGeometry = require 'gl.geometry'
local GLSceneObject = require 'gl.sceneobject'
local glreport = require 'gl.report'
local glnumber = require 'gl.number'	-- TODO since gl needs this too, and cl depends on gl for interop, how about move this to gl?
local ig = require 'imgui'
local vec2i = require 'vec-ffi.vec2i'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'
local vec3d = require 'vec-ffi.vec3d'

local App = require 'imgui.appwithorbit'()

--local fieldDim = 128
--local fieldDim = cmdline.n or 64
--local fieldDim = cmdline.n or 256
local fieldDim = cmdline.n or 512
local count = fieldDim * fieldDim
update = true	-- _G for gui
useBlend = false
displayAlpha = .2
local pairUpdatesPerFrame = 1024	-- dont update all pairs of particles, just this many per frame

local channelsPerFormat = {
	[gl.GL_RGB] = 3,
	[gl.GL_RGBA] = 4,
}

local function dataFromLambda(width, height, format, gltype, callback)
	local ctype = assert.index(GLTypes.ctypeForGLType, gltype, "couldn't determine size of type")
	local numChannels = assert.index(channelsPerFormat, format, "couldn't determine channels for format")
	local arrayType = ffi.typeof('$['..(width * height * numChannels)..']', ctype)
	local ptr = ffi.new(arrayType)
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
	local data = args.data
	if type(data) == 'function' then
		data = dataFromLambda(width, height, format, gltype)
	end
	local pingpong = GLPingPong{
		fbo = args.fbo,
		width = width,
		height = height,
		internalFormat = args.internalFormat or gl.GL_RGBA32F,
		type = gltype,
		format = format,
		data = data,
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_LINEAR,--gl.GL_NEAREST,
	}

	return pingpong
end


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
gravConst = 4.8088480226311e-10	-- in sun units
--gravConst = 1.185609940161901e-4	-- Earth orbit units
--gravConst = .5	-- Moon orbit units

displayScale = 100
pointSize = 2
--gravDistEpsilon = 1e-7
--gravDistEpsilon = 1e-3
gravDistEpsilon = 1e-1

r0min = .1
r0max = 10
m0min = .1
m0max = 1
--m0max = 3
--m0max = 10
rbins = range(1024):mapi(function() return 0 end)	-- bin integrate particles here

math.randomseed(os.time())

local function reset()
	local posData = vector('vec4f', count)
	local velData = vector('vec4f', count)

	local log10m0min = math.log(m0min, 10)
	local log10m0max = math.log(m0max, 10)

	local totalMass = 0
	local com = vec3d()
	local posv = posData.v+0
	local velv = velData.v+0
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
		posv[0].x = math.cos(phi) * r
		posv[0].y = math.sin(phi) * r
		posv[0].z = (math.random() * 2 - 1) * math.exp(-r*r)
		local mass = 10^(math.random() * (log10m0max - log10m0min) + log10m0min)	-- TODO distribution?
		posv[0].w = mass
		totalMass = totalMass + mass
		com = com + mass * vec3d(posv[0].x, posv[0].y, posv[0].z)

		local bin = math.floor((1 - 1e-9) * (r - r0min) / (r0max - r0min) * #rbins) + 1
		rbins[bin] = rbins[bin] + mass
--]]

		posv = posv + 1
		velv = velv + 1
	end
	com = com * (1 / totalMass)

	local posv = posData.v+0
	local velv = velData.v+0
	for i=0,count-1 do
		-- Kepler's 1-2-3 law
		-- G m_sum = omega^2 r^3
		--local rvec = vec3d(posv[0].x, posv[0].y, posv[0].z) - com
		--local r = rvec:length()
		local rvec = vec3d(posv[0].x, posv[0].y, posv[0].z)	-- just assume origin is center
		local r = rvec:length()

		-- P^2 = (2 pi)^2 / (G (m1 + m2)) a^3
		-- (P / 2 pi)^2 = a^3 / (G (m1 + m2))
		-- G (m1 + m2) = (2 pi / P)^2 * a^3
		-- pos = r cis (t 2 pi / P) = r cis (omega t) for omega = 2 pi / P
		-- G (m1 + m2) = omega^2 * a^3
		-- vel = i r omega cis (omega t) = i omega pos
		-- by the 1-2-3 law: G (m1 + m2) = omega^2 * a^3 => omega = sqrt(G (m1 + m2) / a^3)
		-- yes but
		-- the galaxy rotation curve doesn't follow the 1-2-3 law.
		-- it does up to some inner radius and the it falls back down to zero
		-- totalMass, bodyMass or mass within radius?
		local bin = math.floor((1 - 1e-9) * (r - r0min) / (r0max - r0min) * #rbins) + 1
		local massSum = 0
		--for i=1,bin do
		for i=1,bin-1 do
			massSum = massSum + rbins[bin]
		end
		--local omega = math.sqrt(gravConst * posv[0].w / r) / r
		--local omega = math.sqrt(gravConst * totalMass / r) / r
		local omega = math.sqrt(gravConst * massSum / r) / r
omega = omega * .5
		velv[0].x = -omega * rvec.y
		velv[0].y = omega * rvec.x
		velv[0].z = 0	-- -rvec.z		-- ... omega or something?
		velv[0].w = 0

		posv = posv + 1
		velv = velv + 1
	end

	fbo = GLFramebuffer()
		:drawBuffers(
			gl.GL_COLOR_ATTACHMENT0,
			gl.GL_COLOR_ATTACHMENT1)
		:unbind()

	posTexs = createFieldPingPong{data = posData.v, fbo=fbo}
	velTexs = createFieldPingPong{data = velData.v, fbo=fbo}
end

App.viewDist = 2

--[[ crashing
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
--]]

function App:initGL(...)
	App.super.initGL(self, ...)

--[[ crashing
	gl.glDebugMessageCallback(messageCallbackClosure, nil)
	gl.glEnable(gl.GL_DEBUG_OUTPUT)
--]]

	self.view.znear = 1
	self.view.zfar = 1000

	reset()

	gradTex = GLGradientTex2D(256,
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
	):unbind()

	self.quadGeom = GLGeometry{
		mode = gl.GL_TRIANGLE_STRIP,
		vertexes = {
			data = 	{
				0, 0,
				1, 0,
				0, 1,
				1, 1,
			},
			dim = 2,
		},
	}

	simObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[

// TODO replce with gl_VertexID / fixed array
in vec2 vertex;

void main() {
	gl_Position = vec4(vertex * 2. - 1., 0., 1.);
}
]],
			fragmentCode = template([[

layout(location=0) out vec4 posFragColor;
layout(location=1) out vec4 velFragColor;

uniform sampler2D postex, veltex;
uniform float dt, gravConst, gravDistEpsilon;

//#define LOOP_ALL
#define LOOP_FEW

#ifdef LOOP_FEW
uniform int updateStart, updateEnd, fieldDim;
#endif

void main() {
	ivec2 iFragCoord = ivec2(gl_FragCoord);
	vec4 vpos = texelFetch(postex, iFragCoord, 0);
	vec4 vvel = texelFetch(veltex, iFragCoord, 0);

	// forward-Euler step the position
	posFragColor = vec4(vpos.xyz + vvel.xyz * dt, vpos.w);

	// accumulate a subset of all force pairs as a force into the velocity
	vec3 accumforce = vec3(0.);
#ifdef LOOP_ALL
	vec2 otc;
	for (otc.x = .5/<?=fieldDim?>; otc.x < 1.; otc.x += 1./<?=fieldDim?>) {
		for (otc.y = .5/<?=fieldDim?>; otc.y < 1.; otc.y += 1./<?=fieldDim?>) {
			vec4 otherpos = texture(postex, otc);
#endif
#ifdef LOOP_FEW
	{
		for (int i = updateStart; i < updateEnd; ++i) {
			ivec2 itc = ivec2(
				i % fieldDim,
				i / fieldDim
			);
			// going from texture() to texelFetch(): 130 fps -> 160 fps
			vec4 otherpos = texelFetch(postex, itc, 0);
#endif
			float othermass = otherpos.w;
			vec3 del = otherpos.xyz - vpos.xyz;
			float len = length(del);
			float invlen = 1. / (len + gravDistEpsilon);
			del *= invlen;
			del *= invlen;
			del *= invlen;
			del *= othermass;
			del *= gravConst;
			accumforce += del;
		}
	}
	vvel.xyz += accumforce * dt;
	velFragColor = vvel;
}
]],			{
				fieldDim = glnumber(fieldDim),
			}),
			uniforms = {
				postex = 0,
				veltex = 1,
				fieldDim = fieldDim,
			},
		},
		geometry = self.quadGeom,
	}

	local uvCPUMem = dataFromLambda(fieldDim, fieldDim, gl.GL_RGBA, gl.GL_FLOAT, function(i,j)
		return (i+.5)/fieldDim, (j+.5)/fieldDim
	end)
	displayObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
uniform sampler2D posTex, velTex;
uniform mat4 mvProjMat;
uniform int fieldDim;
uniform float pointSize;

#define USE_VERTEX

#ifdef USE_VERTEX
in vec2 vertex;
#endif

out float magn;
void main() {
#ifndef USE_VERTEX
	vec2 vertex = vec2(
		(float(gl_VertexID % fieldDim) + .5) / float(fieldDim),
		(float(gl_VertexID / fieldDim) + .5) / float(fieldDim)
	);
#endif

	gl_PointSize = pointSize;

	vec4 pos4 = texture(posTex, vertex);
	vec4 vel = texture(velTex, vertex);
	vec4 pos = vec4(pos4.xyz, 1.);
	magn = length(vel.xyz);//length(pos.xyz);
	gl_Position = mvProjMat * pos;
}
]],
			fragmentCode = [[
in float magn;
out vec4 fragColor;
uniform sampler2D gradTex;
uniform float displayScale;
uniform float alpha;
void main() {
	fragColor = vec4(
		texture(gradTex, vec2(magn * displayScale, .5)).xyz,
		alpha	// TODO alpha for mass? size for mass?  both for mass?
	);
}
]],
			uniforms = {
				posTex = 0,
				velTex = 1,
				gradTex = 2,
			},
		},
		geometry = {
			mode = gl.GL_POINTS,
			count = count,
		},
		vertexes = {
			dim = 2,
			size = ffi.sizeof'vec2f' * count,
			count = count,
			usage = gl.GL_STATIC_DRAW,
			data = uvCPUMem,
		},
	}

	gl.glDisable(gl.GL_DEPTH_TEST)
	gl.glDisable(gl.GL_CULL_FACE)
end

-- I'll just throw glPointSize and gl_PointSize at it and one is bound to work
local hasGLPointSize = op.safeindex(gl, 'glPointSize')

local updateStart = 0
function App:update(...)
	gl.glClear(gl.GL_COLOR_BUFFER_BIT)
glreport'here'

	if update then
		gl.glViewport(0, 0, fieldDim, fieldDim)

		posTexs:swap()
		velTexs:swap()

		fbo:bind()
		fbo:setColorAttachmentTex2D(posTexs:cur().id, 0)
		fbo:setColorAttachmentTex2D(velTexs:cur().id, 1)
		GLFramebuffer:check()
		simObj.texs[1] = posTexs:prev()
		simObj.texs[2] = velTexs:prev()
		simObj.uniforms.dt = dt
		simObj.uniforms.gravConst = gravConst
			-- scale up by how many times less than typical we are applying forces
			* (count / pairUpdatesPerFrame)
		simObj.uniforms.gravDistEpsilon = gravDistEpsilon
		simObj.uniforms.updateStart = updateStart

		local updateEnd = updateStart + pairUpdatesPerFrame
		local looped
		if updateEnd >= count then
			updateEnd = count
			looped = true
		end
		simObj.uniforms.updateEnd = updateEnd

		simObj:draw()
		if looped then
			updateStart = 0
		else
			updateStart = updateEnd
		end

		GLFramebuffer:unbind()
	end

	gl.glViewport(0, 0, self.width, self.height)

	self.view:setup(self.width / self.height)

	if useBlend then
		gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE)
		gl.glEnable(gl.GL_BLEND)
	end

	-- how to know when this function is there ...
	if hasGLPointSize then
		gl.glPointSize(pointSize)
	end

	displayObj.uniforms.mvProjMat = self.view.mvProjMat.ptr
	displayObj.uniforms.fieldDim = fieldDim
	displayObj.uniforms.displayScale = displayScale
	displayObj.uniforms.pointSize = pointSize
	displayObj.uniforms.alpha = displayAlpha
	displayObj.texs[1] = posTexs:cur()
	displayObj.texs[2] = velTexs:cur()
	displayObj.texs[3] = gradTex
	displayObj:draw()

	-- how to know when this function is there ...
	if hasGLPointSize then
		gl.glPointSize(1)
	end

	gl.glDisable(gl.GL_BLEND)

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
	if event[0].type == sdl.SDL_EVENT_KEY_DOWN then
		if event[0].key.key == ('r'):byte() then
			reset()
		end
	end
end

function App:updateGUI()
	if ig.igButton'reset' then reset() end
	ig.igSameLine()
	ig.luatableCheckbox('update', _G, 'update')
	ig.igSameLine()
	ig.luatableCheckbox('useBlend', _G, 'useBlend')
	ig.luatableInputFloatAsText('displayAlpha', _G, 'displayAlpha')
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

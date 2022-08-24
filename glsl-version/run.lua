#!/usr/bin/env luajit
local ffi = require 'ffi'
local sdl = require 'ffi.sdl'
local math = require 'ext.math'
local table = require 'ext.table'
local class = require 'ext.class'
local vector = require 'ffi.cpp.vector'
local template = require 'template'
local GLTex2D = require 'gl.tex2d'
local GLPixelPackBuffer = require 'gl.pixelpackbuffer'
local GLPingPong = require 'gl.pingpong'
local GLProgram = require 'gl.program'
local GLKernelProgram = require 'gl.kernelprogram'
local gl = require 'gl'
local vec2i = require 'vec-ffi.vec2i'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'

local App = class(require 'glapp.orbit'(require 'imguiapp'))

local fieldDim = 128
local count = fieldDim * fieldDim
local update = true

-- TODO goes in gl.tex and needs to be public
local ctypePerGLType = {
	[gl.GL_UNSIGNED_BYTE] = 'uint8_t',
	[gl.GL_BYTE] = 'int8_t',
	[gl.GL_UNSIGNED_SHORT] = 'uint16_t',
	[gl.GL_SHORT] = 'int16_t',
	[gl.GL_UNSIGNED_INT] = 'uint32_t',
	[gl.GL_INT] = 'int32_t',
	[gl.GL_FLOAT] = 'float',
}
local channelsPerFormat = {
	[gl.GL_RGB] = 3,
	[gl.GL_RGBA] = 4,
}


local function createFieldPingPong(args)
	args = args or {}
	local width = fieldDim
	local height = fieldDim
	local format = args.format or gl.GL_RGBA
	local gltype = args.type or gl.GL_FLOAT
	local internalFormat = args.internalFormat or gl.GL_RGBA32F
	local data = args.data
	if type(data) == 'function' then
		local ctype = assert(ctypePerGLType[gltype], "couldn't determine size of type "..tostring(gltype))
		local numChannels = assert(channelsPerFormat[format], "couldn't determine channels for format "..tostring(format))
		local ptr = ffi.new(ctype..'[?]', width * height * numChannels)
		for j=0,height-1 do
			for i=0,width-1 do
				local e = i + width * j
				local chs = table.pack(data(i, j))
				assert(#chs == numChannels)
				for ch=0,numChannels-1 do
					ptr[ch + numChannels * e] = chs[ch+1] or 0
				end
			end
		end
		data = ptr
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


local posTexs
local velTexs
local vbo

local dt = .001

local function reset()
	local posData = vector('vec4f_t', count)
	local velData = vector('vec4f_t', count)
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
		local r = math.random()
		local phi = math.random() * 2 * math.pi
		local z = math.exp(-r*r)
		posData.v[i].x = math.cos(phi) * r
		posData.v[i].y = math.sin(phi) * r
		posData.v[i].z = z
		posData.v[i].w = 1
--]]

		velData.v[i].x = 0
		velData.v[i].y = 0
		velData.v[i].z = 0
		velData.v[i].w = 0
	end
	posTexs = createFieldPingPong{data = posData.v}
	velTexs = createFieldPingPong{data = velData.v}
end

App.viewDist = 1

function App:initGL(...)
	App.super.initGL(self, ...)
	
	self.view.znear = .01
	self.view.zfar = 10

	reset()

	hsvtex = require 'gl.gradienttex'(256, 
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

	-- [[
	vbo = GLPixelPackBuffer{size = fieldDim * fieldDim * 4 * 4, usage = gl.GL_DYNAMIC_DRAW}
	--]]

	simPosShader = GLKernelProgram{
		texs={'postex', 'veltex'};
		code = [[
void main() {
	vec4 vpos = texture2D(postex, pos);
	vec4 vvel = texture2D(veltex, pos);
	const float dt = ]]..dt..[[;
	vpos.xyz += vvel.xyz * dt;
	vpos.w = length(vvel);
	gl_FragColor = vpos;
}
]];
	}

	simVelShader = GLKernelProgram{
		texs={'postex','veltex'};
		code = template([[
void main() {
	const float dt = <?=dt?>;
	const float gravConst = .1;
	vec4 vpos = texture2D(postex, pos);
	vec4 vvel = texture2D(veltex, pos);
	vec3 accumforce = vec3(0.);
	vec2 otc;
	for (otc.x = .5/<?=fieldDim?>; otc.x < 1.; otc.x += 1./<?=fieldDim?>) {
		for (otc.y = .5/<?=fieldDim?>; otc.y < 1.; otc.y += 1./<?=fieldDim?>) {
			vec4 otherpos = texture2D(postex, otc);
			vec3 del = otherpos.xyz - vpos.xyz;
			float len = length(del);
			len = max(len, .001);
			del *= (gravConst / len*len*len);
			accumforce += del;
		}
	}
	vvel.xyz += accumforce * dt;
	gl_FragColor = vvel;
}
]],		{
			dt = dt,
			fieldDim = require 'cl.obj.number'(fieldDim),
		}),
	}

	displayShader = GLProgram{
		uniforms={
			tex=0,
		};
		vertexCode = [[
varying float magn;
void main() {
	vec4 v = vec4(gl_Vertex.xyz, 1.);
	magn = gl_Vertex.w;
	gl_Position = gl_ModelViewProjectionMatrix * v;
}
]];
		fragmentCode = [[
#define	magnscale	.01
varying float magn;
uniform sampler1D tex;
void main() {
	gl_FragColor = texture1D(tex, magn * magnscale);
}
]];
	}

	gl.glDisable(gl.GL_DEPTH_TEST)
	gl.glDisable(gl.GL_CULL_FACE)
end

local viewport = vec4f()
function App:update(...)
	gl.glClear(gl.GL_COLOR_BUFFER_BIT)


	gl.glEnable(gl.GL_TEXTURE_2D)
	
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
		velTexs:swap()
		velTexs:draw{
			shader=simVelShader;
			texs={posTexs:cur(), velTexs:prev()};
		}
		posTexs:swap()
		posTexs:draw{
			shader=simPosShader;
			texs={posTexs:prev(), velTexs:prev()};
		}
	end
	gl.glViewport(viewport:unpack())
	gl.glMatrixMode(gl.GL_PROJECTION)
	gl.glPopMatrix()
	gl.glMatrixMode(gl.GL_MODELVIEW)
	gl.glPopMatrix()

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


-- [[
	-- copy from texture to vbo
	gl.glBindBuffer(gl.GL_PIXEL_PACK_BUFFER, vbo.id)
	gl.glBindTexture(gl.GL_TEXTURE_2D, posTexs:cur().id)
	gl.glGetTexImage(gl.GL_TEXTURE_2D, 0, gl.GL_RGBA, gl.GL_FLOAT, nil)
	gl.glBindTexture(gl.GL_TEXTURE_2D, 0)
	gl.glBindBuffer(gl.GL_PIXEL_PACK_BUFFER, 0)

	gl.glPointSize(2)
	displayShader:use()

	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
	gl.glBindBuffer(gl.GL_ARRAY_BUFFER, vbo.id)
	gl.glVertexPointer(4, gl.GL_FLOAT, 0, nil)
	gl.glBindBuffer(gl.GL_ARRAY_BUFFER, 0)
	gl.glDrawArrays(gl.GL_POINTS, 0, fieldDim*fieldDim)
	gl.glDisableClientState(gl.GL_COLOR_ARRAY)

	gl.glUseProgram(0)
	gl.glPointSize(1)
--]]



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

	gl.glDisable(gl.GL_TEXTURE_2D)

	-- gui update & view update
	App.super.update(self, ...)

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

App():run()

#!/usr/bin/env luajit

local ffi = require 'ffi'
local vector = require 'ffi.cpp.vector-lua'
local table = require 'ext.table'
local path = require 'ext.path'
local GLProgram = require 'gl.program'
local GLTex2D = require 'gl.tex2d'
local GLArrayBuffer = require 'gl.arraybuffer'
local glreport = require 'gl.report'
local gl = require 'gl'
local CLEnv = require 'cl.obj.env'
local vec2i = require 'vec-ffi.vec2i'
local vec3f = require 'vec-ffi.vec3f'
local vec4f = require 'vec-ffi.vec4f'

local App = require 'imguiapp.withorbit'()
App.viewUseGLMatrixMode = true
--local count = 16
local count = 2048

App.viewDist = 1


--require 'cl-cpu'.clcpu_build = 'debug'	--fixes problems? 

function App:initGL(...)
	App.super.initGL(self, ...)

	self.view.znear = .01
	self.view.zfar = 10

	self.posCPUMem = vector'vec4f_t'

	self.env = CLEnv{
		precision = 'float',
		size = {count},
		verbose = true,
	}

	self.positionVBO = GLArrayBuffer{
		size = ffi.sizeof'vec4f_t' * count,
		usage = gl.GL_DYNAMIC_DRAW,
	}:unbind()

	if self.env.useGLSharing then
		error"haven't got gl sharing yet"
--		self.posMem = cl::BufferGL(clCommon->context, CL_MEM_WRITE_ONLY, self.positionVBO.id)
	else
		self.posMem = self.env:buffer{type='vec4f_t', count=count}
		self.posCPUMem:resize(count)
	end

	local headerCode = table{
		[[
typedef vec3f_t real3;

typedef struct body_t {
	real3 pos;
	real3 vel;
	real mass;
} body_t;
]],
	}:concat'\n'

	ffi.cdef(headerCode)

	self.objsMem = self.env:buffer{type='body_t', count=count}
	self.objsMemPrev = self.env:buffer{type='body_t', count=count}

	self.program = self.env:program{
		code = table{
			vec3f.code,
			headerCode,
			'#define COUNT '..count,
			'#define INITIAL_RADIUS 50000',
			assert(path'nbody.cl':read()),
		}:concat'\n',
	}
	self.program:compile()

	self.randBuffer = vector('real', count)
	for i=0,count-1 do self.randBuffer.v[i] = math.random() end
	local randMem = self.env:buffer{type='real', count=count, data=self.randBuffer.v}

	local initDataKernel = self.program:kernel('initData', self.objsMem, randMem)
	initDataKernel()
	self.env.cmds[1]:finish()

--[[ debugging ... works by here
	local objsCPUMem = vector('body_t', count)
	self.objsMem:toCPU(objsCPUMem.v)
	print'init objsMem:'
	for i=0,count-1 do
		print(objsCPUMem.v[i].pos)
	end
--]]

	self.updateKernel = self.program:kernel'update'

	self.copyToGLKernel = self.program:kernel'copyToGL'
	self.copyToGLKernel.obj:setArg(0, self.posMem)

	--[[ for fbo post-processing 
	local screenBufferSize = vec2i(1024, 1024)
	self.postTex = GLTex2D{
		minFilter = gl.GL_LINEAR,
		magFilter = gl.GL_LINEAR,
		format = gl.GL_RGBA,
		internalFormat = gl.GL_RGBA,
		type = gl.GL_UNSIGNED_BYTE,
		width = screenBufferSize.x,
		height = screenBufferSize.y, 
	}
	self.postTex:unbind()
	--]]

	local particleTexSize = vec2i(256,256)
	local particleTexData = vector('uint8_t', particleTexSize.x * particleTexSize.y * 4)
	do
		local ptr = ffi.cast('uint8_t*', particleTexData.v)
		for y = 0, tonumber(particleTexSize.y)-1 do
			for x = 0, tonumber(particleTexSize.x)-1 do
				local dx = (x + .5) / tonumber(particleTexSize.x) - .5
				local dy = (y + .5) / tonumber(particleTexSize.y) - .5
				local dr2 = dx*dx + dy*dy
				local lum = math.exp(-100. * dr2)
				ptr[0] = 255 * math.min(.5, lum)
				ptr[1] = 255 * math.min(.5, lum)
				ptr[2] = 255 * math.min(.5, lum)
				ptr[3] = 255
				ptr = ptr + 4
			end
		end
	end
	self.particleTex = GLTex2D{
		minFilter = gl.GL_LINEAR,
		magFilter = gl.GL_LINEAR,
		width = tonumber(particleTexSize.x),
		height = tonumber(particleTexSize.y),
		format = gl.GL_RGBA,
		internalFormat = gl.GL_RGBA,
		type = gl.GL_UNSIGNED_BYTE,
		data = particleTexData.v,
		generateMipmap = true,
	}
		:unbind()

--	local gradientTex = require 'gl.gradienttex'(256)
--		:unbind()

	self.particleShader = GLProgram{
		vertexCode = [[
varying float mass;
varying float distSq;

void main() {
	const float spriteWidth = 10.;
	mass = gl_Vertex.w;
	vec4 vertex = vec4(gl_Vertex.xyz, 1.);
	vec4 eyeVertex = gl_ModelViewMatrix * vertex;
	distSq = dot(eyeVertex.xyz, eyeVertex.xyz);
	gl_Position = gl_ProjectionMatrix * eyeVertex;
	gl_PointSize = spriteWidth / gl_Position.w;
}
]],
		fragmentCode = [[
varying float mass;
varying float distSq;

uniform sampler2D tex;
#define LOG_10 2.302585092994
void main() {
	//float lumForMass = (log(mass) / LOG_10 + 1.) / 5.;	//mass is 10^0 to 10^4
	//float lum = lumForMass * .3 / distSq;

	float lum = 1.;//mass * .0001;// / distSq;

	gl_FragColor = texture2D(tex, gl_TexCoord[0].xy) * lum;
}
]],
		uniforms = {
			tex = 0,
		},
	}:useNone()
end

function App:update(...)
	App.super.update(self, ...)

--[[ debugging ... 
	local objsCPUMem = vector('body_t', count)
	self.objsMem:toCPU(objsCPUMem.v)
	print'update objsMem:'
	for i=0,count-1 do
		print(objsCPUMem.v[i].pos)
	end
--]]


	if self.env.useGLSharing then
		error"haven't got gl sharing yet"
		self.copyToGLKernel.obj:setArg(1, self.objsMem)
	else
self.copyToGLKernel.obj:setArg(0, self.posMem)
		self.copyToGLKernel.obj:setArg(1, self.objsMem)

-- ok printing this in the kernel makes it retain and therefore not crash
--print('#allPtrs', #require 'cl-cpu'.clcpu_allPtrs)
--print('#allMems', #require 'cl-cpu'.clcpu_allMems)
--print('posMem (dst)', self.posMem, self.posMem.ptr)
--print('objsMem (srcObjs)', self.objsMem, self.objsMem.ptr)

		self.copyToGLKernel(
self.posMem,
self.objsMem
		)
		self.posMem:toCPU(self.posCPUMem.v)
		self.env.cmds[1]:finish()

--[[ debugging ... 
	print'update posMem:'
	for i=0,count-1 do
		print(self.posCPUMem.v[i])
	end
--]]


		self.positionVBO
			:bind()
			:updateData(0, ffi.sizeof'vec4f_t' * count, self.posCPUMem.v)
			:unbind()
		gl.glFinish()
	end

	self.updateKernel(self.objsMemPrev, self.objsMem)
	self.objsMem, self.objsMemPrev = self.objsMemPrev, self.objsMem

	gl.glClear(gl.GL_COLOR_BUFFER_BIT)
	gl.glPointSize(20)
	gl.glHint(gl.GL_POINT_SMOOTH_HINT, gl.GL_NICEST)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE)
	gl.glPointParameterfv(gl.GL_POINT_DISTANCE_ATTENUATION, vec3f(0, 1, 0).s)
	gl.glPointParameterf(gl.GL_POINT_SIZE_MIN, 1)
	gl.glPointParameterf(gl.GL_POINT_SIZE_MAX, 128)

	gl.glEnable(gl.GL_POINT_SMOOTH)
	gl.glEnable(gl.GL_BLEND)
	self.particleTex:bind()
	gl.glEnable(gl.GL_POINT_SPRITE)
--	gl.glEnable(gl.GL_VERTEX_PROGRAM_POINT_SIZE)
	gl.glTexEnvi(gl.GL_POINT_SPRITE, gl.GL_COORD_REPLACE, gl.GL_TRUE)

	self.particleShader:use()
	-- [[ using primitives
	gl.glBegin(gl.GL_POINTS)
	do
		for i=0,count-1 do
			gl.glVertex4f(self.posCPUMem.v[i]:unpack())
		end
	end
	gl.glEnd()
	--]]
	--[[ using VBO
	self.positionVBO:bind()
	gl.glVertexPointer(4, gl.GL_FLOAT, 0, nil)
	gl.glEnableClientState(gl.GL_VERTEX_ARRAY)
	gl.glDrawArrays(gl.GL_POINTS, 0, count)
	gl.glDisableClientState(gl.GL_VERTEX_ARRAY)
	self.positionVBO:unbind()
	--]]
	self.particleShader:useNone()
	
	gl.glDisable(gl.GL_BLEND)
--	gl.glDisable(gl.GL_VERTEX_PROGRAM_POINT_SIZE)
	gl.glDisable(gl.GL_POINT_SPRITE)

	--[[ if we're using fbo post-processing ... TODO needs gradient too ... and blur filter
	self.postTex
		:bind()
		:generateMipmap()
		:unbind()
	--]]

--self:requestExit()

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

return App():run()

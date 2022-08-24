#!/usr/bin/env luajit

local ffi = require 'ffi'
local vector = require 'ffi.cpp.vector'
local class = require 'ext.class'
local table = require 'ext.table'
local io = require 'ext.io'
local GLProgram = require 'gl.program'
local GLArrayBuffer = require 'gl.arraybuffer'
local gl = require 'gl'
local CLEnv = require 'cl.obj.env'
local vec4f = require 'vec-ffi.vec4f'

local App = class(require 'glapp.orbit'(require 'imguiapp'))

local count = 2048

function App:initGL(...)
	App.super.initGL(self, ...)

	posCPUMem = vector'vec4f_t'

	self.env = CLEnv{
		size = {count},
		verbose = true,
	}

	positionVBO = GLArrayBuffer{size = ffi.sizeof'vec4f_t' * count, usage = gl.GL_DYNAMIC_DRAW}

--	if self.env.useGLSharing then
--		posMem = cl::BufferGL(clCommon->context, CL_MEM_WRITE_ONLY, positionVBO);
--	else
		posCPUMem:resize(count)
--	end

	local headerCode = [[
typedef struct {
	real x, y, z;
} real3;

typedef struct {
	real3 pos;
	real3 vel;
	real mass;
} Object;
]]

	ffi.cdef(headerCode)

	objsMem = self.env:buffer{type='Object', count=count}
	objsMemPrev = self.env:buffer{type='Object', count=count}

	local program = self.env:program{
		code = table{
			headerCode,
			'#define COUNT '..count,
			'#define INITIAL_RADIUS 50000',
			assert(io.readfile'nbody.cl'),
		}:concat'\n',
	}
	program:compile()

	randMem = self.env:buffer{type='real', count=count}

	local initDataKernel = program:kernel('initData', objsMem, randMem)
	initDataKernel()
	self.env.cmds[1]:finish()

	self.particleShader = GLProgram {
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
	}
end

return App():run()

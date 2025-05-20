//#include "nbody.h" ... now appended at the top

//http://www.artcompsci.org/kali/vol/plummer/volume9.pdf

/*
x'_i = v_i
v'_i = a_i = F_i / m_i = sum_j~=i G m_j * (x_j - x_i) / |x_j - x_i|^3
... versus G [sum_j m_j] [[sum_j (m_j x_j) / sum_j m_j] - x_i] / [[sum_j (m_j x_j) / sum_j m_j] - x_i|^3
*/

/*
const real gravityConstantInM3PerKgS2 = 6.67384e-11;	//G in terms of m^e / kg s^2
const real lyrPerM = 1.0570234110814e-16;				//light years per meter
const real sPerYr = 3.15569e7;							//seconds per year
const real yrPerGyr = 2.25e8;							//galactic years per year
const real sPerGyr = sPerYr * yrPerGyr;				//seconds per galactic year
const real kgPerSm = 1.9891e30;						//kilograms per solar mass
const real gravityConstantInLyr3PerSmGyr2 = gravityConstantInM3PerKgS2 * lyrPerM * lyrPerM * lyrPerM * sPerGyr * sPerGyr * kgPerSm;
*/
#define GRAVITY_CONSTANT	7903.8725760201			//in case the above is beyond the precision of compile-time evaluation
#define DT	.1										//in galactic years
#define EPS	1e+8										//in light years

/*
m/M = r^3 / (r^2 + a^2)^3/2		<- m/M is our 0-1 random number
(r^2 + a^2)^3/2 = M/m r^3
r^2 + a^2 = (M/m)^2/3 r^2
r^2 (1 - (M/m)^2/3) = -a^2
r^2 = a^2 / ((M/m)^2/3 - 1)
r = a / ((M/m)^2/3 - 1)^1/2
*/
kernel void initData(
	global body_t * objs,
	global real * randBuffer)
{
	int i = get_global_id(0);
	if (i >= COUNT) return;
	global body_t * obj = objs + i;
	
	int j = i;
#define FRAND()		(randBuffer[j=(j+104729)%COUNT])
#define CRAND()		(FRAND() * 2. - 1.)
#ifndef M_PI
#define M_PI		3.1415926535898
#endif
#define AVERAGE_MASS	2171.2552622753	//average mass = .5 * (10^4 - 10^0) / log(10)
#define TOTAL_MASS		(COUNT * AVERAGE_MASS)
	obj->mass = (real)mix((real)100., (real)10000., (real)FRAND());//pow(10., FRAND() * 4.);
	real density = .5 * sqrt(-log(1. - FRAND()));
	real cbrtMm = cbrt(TOTAL_MASS / FRAND());
	real a = 100. * INITIAL_RADIUS / sqrt(sqrt(2.) - 1.);	//mystery: why scale by 100?
	real radius = a / sqrt(cbrtMm * cbrtMm - 1.);
	{
		real phi = 2. * M_PI * FRAND();
		real theta = acos(2. * FRAND() - 1.);
		
		//real3 dir = _real3(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
		real3 dir;
		dir.x = cos(phi) * sin(theta);
		dir.y = sin(phi) * sin(theta);
		dir.z = cos(theta);
		
		//obj->pos = dir * radius;
		obj->pos.x = dir.x * radius;
		obj->pos.y = dir.y * radius;
		obj->pos.z = dir.z * radius;
	}

	{
		real x = 0.;
		real y = .1;
		while (y > x * x * pow(1. - x * x, 3.5)) {
			x = FRAND();
			y = .1 * FRAND();
		}
		real velocity = x * sqrt(sqrt(4. / (1. + radius * radius)));
		real phi = 2. * M_PI * FRAND();
		real theta = acos(2. * FRAND() - 1.);
		
		//real3 dir = (real3)(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
		real3 dir;
		dir.x = cos(phi) * sin(theta);
		dir.y = sin(phi) * sin(theta);
		dir.z = cos(theta);
		
		//obj->vel = dir * (velocity * INITIAL_RADIUS * 5.);
		obj->vel.x = dir.x * (velocity * INITIAL_RADIUS * 5.);
		obj->vel.y = dir.y * (velocity * INITIAL_RADIUS * 5.);
		obj->vel.z = dir.z * (velocity * INITIAL_RADIUS * 5.);
	}
#undef FRAND
}
/*
F1 = G m1 m2 / r^2 = m1 a1
a1 = G m2 / r^2

for orbits
x = (r cos wt, r sin wt)
x' = (-rw sin wt, rw cos wt)
x'' = (-rw^2 cos wt, -rw^2 sin wt) = -w^2 x
|v| = |x'| = rw
|a| = w^2 |x| = w^2 r = G M / r^2		<- for 'M' the mass of all other bodies
G M = w^2 r^3							<- 1-2-3 law
w = sqrt(G M / r^3)
|v| = r w = sqrt(G M / r)
*/

kernel void update(
	global body_t *newObjs,
	global const body_t *oldObjs)
{
	int i = get_global_id(0);
	if (i >= COUNT) return;

	global body_t *newObj = newObjs + i;
	*newObj = oldObjs[i];

	//newObj->pos += newObj->vel * DT;
	newObj->pos.x += newObj->vel.x * DT;
	newObj->pos.y += newObj->vel.y * DT;
	newObj->pos.z += newObj->vel.z * DT;
	
	global const body_t *oldObj = oldObjs;
	for (int j = 0; j < COUNT; ++j, ++oldObj) {
		
		//real3 dx = newObj->pos - oldObj->pos;
		real3 dx;
		dx.x = newObj->pos.x - oldObj->pos.x;
		dx.y = newObj->pos.y - oldObj->pos.y;
		dx.z = newObj->pos.z - oldObj->pos.z;
		
		//real invLen = rsqrt(dot(dx,dx) + EPS);
		real invLen = rsqrt(dx.x * dx.x + dx.y * dx.y + dx.z * dx.z + EPS);
		real invLen3 = invLen * invLen * invLen;
		
		//real3 gravity = -dx * invLen3;
		real3 gravity;
		gravity.x = -dx.x * invLen3;
		gravity.y = -dx.y * invLen3;
		gravity.z = -dx.z * invLen3;
		
		//newObj->vel += gravity * (oldObj->mass * GRAVITY_CONSTANT * DT);
		newObj->vel.x += gravity.x * (oldObj->mass * GRAVITY_CONSTANT * DT);
		newObj->vel.y += gravity.y * (oldObj->mass * GRAVITY_CONSTANT * DT);
		newObj->vel.z += gravity.z * (oldObj->mass * GRAVITY_CONSTANT * DT);
	}
}

//#include <stdio.h>

kernel void copyToGL(
	global real4 *dsts,
	global const body_t *srcObjs)
{
	int i = get_global_id(0);
	if (i >= COUNT) return;
//printf("copyToGL %lu\n", i);
//printf("dsts %p\n", dsts);
//printf("srcObjs %p\n", srcObjs);
	global real4 *dst = dsts + i;
	global const body_t *srcObj = srcObjs + i;
	//dst->xyz = srcObj->pos.xyz / INITIAL_RADIUS;
	dst->x = srcObj->pos.x / INITIAL_RADIUS;
	dst->y = srcObj->pos.y / INITIAL_RADIUS;
	dst->z = srcObj->pos.z / INITIAL_RADIUS;
	dst->w = srcObj->mass;
}

//#include "nbody.h" ... now appended at the top

//http://www.artcompsci.org/kali/vol/plummer/volume9.pdf

/*
x'_i = v_i
v'_i = a_i = F_i / m_i = sum_j~=i G m_j * (x_j - x_i) / |x_j - x_i|^3
... versus G [sum_j m_j] [[sum_j (m_j x_j) / sum_j m_j] - x_i] / [[sum_j (m_j x_j) / sum_j m_j] - x_i|^3
*/

/*
const float gravityConstantInM3PerKgS2 = 6.67384e-11;	//G in terms of m^e / kg s^2
const float lyrPerM = 1.0570234110814e-16;				//light years per meter
const float sPerYr = 3.15569e7;							//seconds per year
const float yrPerGyr = 2.25e8;							//galactic years per year
const float sPerGyr = sPerYr * yrPerGyr;				//seconds per galactic year
const float kgPerSm = 1.9891e30;						//kilograms per solar mass
const float gravityConstantInLyr3PerSmGyr2 = gravityConstantInM3PerKgS2 * lyrPerM * lyrPerM * lyrPerM * sPerGyr * sPerGyr * kgPerSm;
*/
#define GRAVITY_CONSTANT	7903.8725760201f			//in case the above is beyond the precision of compile-time evaluation
#define DT	.1f										//in galactic years
#define EPS	1e+8f										//in light years

/*
m/M = r^3 / (r^2 + a^2)^3/2		<- m/M is our 0-1 random number
(r^2 + a^2)^3/2 = M/m r^3
r^2 + a^2 = (M/m)^2/3 r^2
r^2 (1 - (M/m)^2/3) = -a^2
r^2 = a^2 / ((M/m)^2/3 - 1)
r = a / ((M/m)^2/3 - 1)^1/2
*/
kernel void initData(
	global Object *objs,
	global float *randBuffer)
{
	int i = get_global_id(0);
	if (i >= COUNT) return;
	global Object *obj = objs + i;
	
	int j = i;
#define FRAND()		(randBuffer[j=(j+104729)%COUNT])
#define CRAND()		(FRAND() * 2. - 1.)
#ifndef M_PI
#define M_PI		3.1415926535898f
#endif
#define AVERAGE_MASS	2171.2552622753f	//average mass = .5 * (10^4 - 10^0) / log(10)
#define TOTAL_MASS		(COUNT * AVERAGE_MASS)
	obj->mass = mix(100.f, 10000.f, FRAND());//pow(10., FRAND() * 4.);
	float density = .5f * sqrt(-log(1.f - FRAND()));
	float cbrtMm = cbrt(TOTAL_MASS / FRAND());
	float a = 100.f * INITIAL_RADIUS / sqrt(sqrt(2.f) - 1.f);	//mystery: why scale by 100?
	float radius = a / sqrt(cbrtMm * cbrtMm - 1.f);
	{
		float phi = 2.f * M_PI * FRAND();
		float theta = acos(2.f * FRAND() - 1.f);
		
		//float3 dir = _float3(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
		float3 dir;
		dir.x = cos(phi) * sin(theta);
		dir.y = sin(phi) * sin(theta);
		dir.z = cos(theta);
		
		//obj->pos = dir * radius;
		obj->pos.x = dir.x * radius;
		obj->pos.y = dir.y * radius;
		obj->pos.z = dir.z * radius;
	}

	{
		real x = 0.f;
		real y = .1f;
		while (y > x * x * pow(1.f - x * x, 3.5f)) {
			x = FRAND();
			y = .1f * FRAND();
		}
		float velocity = x * sqrt(sqrt(4.f / (1.f + radius * radius)));
		float phi = 2.f * M_PI * FRAND();
		float theta = acos(2.f * FRAND() - 1.f);
		
		//float3 dir = (float3)(cos(phi) * sin(theta), sin(phi) * sin(theta), cos(theta));
		float3 dir;
		dir.x = cos(phi) * sin(theta);
		dir.y = sin(phi) * sin(theta);
		dir.z = cos(theta);
		
		//obj->vel = dir * (velocity * INITIAL_RADIUS * 5.f);
		obj->vel.x = dir.x * (velocity * INITIAL_RADIUS * 5.f);
		obj->vel.y = dir.y * (velocity * INITIAL_RADIUS * 5.f);
		obj->vel.z = dir.z * (velocity * INITIAL_RADIUS * 5.f);
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
	global Object *newObjs,
	global const Object *oldObjs)
{
	int i = get_global_id(0);
	if (i >= COUNT) return;

	global Object *newObj = newObjs + i;
	*newObj = oldObjs[i];

	//newObj->pos += newObj->vel * DT;
	newObj->pos.x += newObj->vel.x * DT;
	newObj->pos.y += newObj->vel.y * DT;
	newObj->pos.z += newObj->vel.z * DT;
	
	global const Object *oldObj = oldObjs;
	for (int j = 0; j < COUNT; ++j, ++oldObj) {
		
		//real3 dx = newObj->pos - oldObj->pos;
		real3 dx;
		dx.x = newObj->pos.x - oldObj->pos.x;
		dx.y = newObj->pos.y - oldObj->pos.y;
		dx.z = newObj->pos.z - oldObj->pos.z;
		
		real invLen = rsqrt(dot(dx,dx) + EPS);
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

kernel void copyToGL(
	global float4 *dsts,
	global const Object *srcObjs)
{
	int i = get_global_id(0);
	if (i >= COUNT) return;

	global float4 *dst = dsts + i;
	global const Object *srcObj = srcObjs + i;
	//dst->xyz = srcObj->pos.xyz / INITIAL_RADIUS;
	dst->x = srcObj->pos.x / INITIAL_RADIUS;
	dst->y = srcObj->pos.y / INITIAL_RADIUS;
	dst->z = srcObj->pos.z / INITIAL_RADIUS;
	dst->w = srcObj->mass;
}

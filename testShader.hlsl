
cbuffer SceneConstants
{
    float3 Eye : TEXCOORD0;

    float3 Light : TEXCOORD1;

    float3 AmbientLight : TEXCOORD2;

    float LightRangeSquared : TEXCOORD3;

};

cbuffer ObjectConstants
{
    row_major matrix WorldViewProjection : TEXCOORD0;

    row_major matrix World : TEXCOORD1;

};

cbuffer MaterialConstants
{
    float Ambient : TEXCOORD0;

    float Diffuse : TEXCOORD1;

    float Specular : TEXCOORD2;

    float Shine : TEXCOORD3;

};

struct VSInput
{
    float4 Position : POSITION;

    float3 Normal : NORMAL;

};

struct PSInput
{
    float4 PositionHS : SV_POSITION;

    float3 PositionWS : TEXCOORD0;

    float3 PositionOS : TEXCOORD1;

    float3 Normal : NORMAL;

};
Texture2D permutation : register(t0);
Texture2D gradients : register(t1);
SamplerState pointSampler : register(s0);
PSInput vertex(VSInput input){ float4 worldPos = mul((input).Position,World);

    PSInput o;
    o.PositionHS = mul((input).Position,WorldViewProjection);
o.PositionWS = (worldPos).xyz;
o.PositionOS = ((input).Position).xyz;
o.Normal = mul((input).Normal,(float3x3)(World));
    return o; };
float4 pixel(PSInput input) : SV_TARGET{ float3 localPos = (input).PositionOS;
float3 norm;
                    {
                        float3 _normal = (input).Normal;
float f0;
                    {
                        float _t;
                    {
                        float W = 640.0f;
float t = -0.5f;
float f = 1.0f;

    for (int i=1; i <= 7; i++)
    {
        float n;
                    {
                        float3 _pos = (localPos) * (f);
float3 _P = (fmod(floor(_pos),256.0f)) / (256.0f);
float3 _p = (_pos) - (floor(_pos));
float _one = (1.0f) / (256.0f);
float3 _f = (((_p) * (_p)) * (_p)) * (((_p) * (((_p) * (6.0f)) - (15.0f))) + (10.0f));
float4 _AA = (permutation.Sample(pointSampler,(_P).xy)) + ((_P).z);
float _A;
                    {
                        float ___t = (_AA).x;
float4 ___grad = gradients.Sample(pointSampler,___t);
_A = dot((___grad).xyz,_p);
                    }
float _B;
                    {
                        float ___t = (_AA).z;
float3 ___p = (_p) + (float3(-1.0f,0.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_B = dot((___grad).xyz,___p);
                    }
float _C;
                    {
                        float ___t = (_AA).y;
float3 ___p = (_p) + (float3(0.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_C = dot((___grad).xyz,___p);
                    }
float _D;
                    {
                        float ___t = (_AA).w;
float3 ___p = (_p) + (float3(-1.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_D = dot((___grad).xyz,___p);
                    }
float _E;
                    {
                        float ___t = ((_AA).x) + (_one);
float3 ___p = (_p) + (float3(0.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_E = dot((___grad).xyz,___p);
                    }
float _F;
                    {
                        float ___t = ((_AA).z) + (_one);
float3 ___p = (_p) + (float3(-1.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_F = dot((___grad).xyz,___p);
                    }
float _G;
                    {
                        float ___t = ((_AA).y) + (_one);
float3 ___p = (_p) + (float3(0.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_G = dot((___grad).xyz,___p);
                    }
float _H;
                    {
                        float ___t = ((_AA).w) + (_one);
float3 ___p = (_p) + (float3(-1.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_H = dot((___grad).xyz,___p);
                    }
n = lerp(lerp(lerp(_A,_B,(_f).x),lerp(_C,_D,(_f).x),(_f).y),lerp(lerp(_E,_F,(_f).x),lerp(_G,_H,(_f).x),(_f).y),(_f).z);
                    }
t =  (t) + ((n) / (f));;
 f =  (f) * (2.0f);;
;
    };_t = t;
                    }
f0 = (-0.1f) * (_t);
                    }
float epsilon = 0.0001f;
float3 dx = float3(epsilon,0.0f,0.0f);
float3 dy = float3(0.0f,epsilon,0.0f);
float3 dz = float3(0.0f,0.0f,epsilon);
float fx;
                    {
                        float _t;
                    {
                        float W = 640.0f;
float t = -0.5f;
float f = 1.0f;

    for (int i=1; i <= 7; i++)
    {
        float n;
                    {
                        float3 _pos = ((localPos) + (dx)) * (f);
float3 _P = (fmod(floor(_pos),256.0f)) / (256.0f);
float3 _p = (_pos) - (floor(_pos));
float _one = (1.0f) / (256.0f);
float3 _f = (((_p) * (_p)) * (_p)) * (((_p) * (((_p) * (6.0f)) - (15.0f))) + (10.0f));
float4 _AA = (permutation.Sample(pointSampler,(_P).xy)) + ((_P).z);
float _A;
                    {
                        float ___t = (_AA).x;
float4 ___grad = gradients.Sample(pointSampler,___t);
_A = dot((___grad).xyz,_p);
                    }
float _B;
                    {
                        float ___t = (_AA).z;
float3 ___p = (_p) + (float3(-1.0f,0.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_B = dot((___grad).xyz,___p);
                    }
float _C;
                    {
                        float ___t = (_AA).y;
float3 ___p = (_p) + (float3(0.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_C = dot((___grad).xyz,___p);
                    }
float _D;
                    {
                        float ___t = (_AA).w;
float3 ___p = (_p) + (float3(-1.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_D = dot((___grad).xyz,___p);
                    }
float _E;
                    {
                        float ___t = ((_AA).x) + (_one);
float3 ___p = (_p) + (float3(0.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_E = dot((___grad).xyz,___p);
                    }
float _F;
                    {
                        float ___t = ((_AA).z) + (_one);
float3 ___p = (_p) + (float3(-1.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_F = dot((___grad).xyz,___p);
                    }
float _G;
                    {
                        float ___t = ((_AA).y) + (_one);
float3 ___p = (_p) + (float3(0.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_G = dot((___grad).xyz,___p);
                    }
float _H;
                    {
                        float ___t = ((_AA).w) + (_one);
float3 ___p = (_p) + (float3(-1.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_H = dot((___grad).xyz,___p);
                    }
n = lerp(lerp(lerp(_A,_B,(_f).x),lerp(_C,_D,(_f).x),(_f).y),lerp(lerp(_E,_F,(_f).x),lerp(_G,_H,(_f).x),(_f).y),(_f).z);
                    }
t =  (t) + ((n) / (f));;
 f =  (f) * (2.0f);;
;
    };_t = t;
                    }
fx = (-0.1f) * (_t);
                    }
float fy;
                    {
                        float _t;
                    {
                        float W = 640.0f;
float t = -0.5f;
float f = 1.0f;

    for (int i=1; i <= 7; i++)
    {
        float n;
                    {
                        float3 _pos = ((localPos) + (dy)) * (f);
float3 _P = (fmod(floor(_pos),256.0f)) / (256.0f);
float3 _p = (_pos) - (floor(_pos));
float _one = (1.0f) / (256.0f);
float3 _f = (((_p) * (_p)) * (_p)) * (((_p) * (((_p) * (6.0f)) - (15.0f))) + (10.0f));
float4 _AA = (permutation.Sample(pointSampler,(_P).xy)) + ((_P).z);
float _A;
                    {
                        float ___t = (_AA).x;
float4 ___grad = gradients.Sample(pointSampler,___t);
_A = dot((___grad).xyz,_p);
                    }
float _B;
                    {
                        float ___t = (_AA).z;
float3 ___p = (_p) + (float3(-1.0f,0.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_B = dot((___grad).xyz,___p);
                    }
float _C;
                    {
                        float ___t = (_AA).y;
float3 ___p = (_p) + (float3(0.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_C = dot((___grad).xyz,___p);
                    }
float _D;
                    {
                        float ___t = (_AA).w;
float3 ___p = (_p) + (float3(-1.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_D = dot((___grad).xyz,___p);
                    }
float _E;
                    {
                        float ___t = ((_AA).x) + (_one);
float3 ___p = (_p) + (float3(0.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_E = dot((___grad).xyz,___p);
                    }
float _F;
                    {
                        float ___t = ((_AA).z) + (_one);
float3 ___p = (_p) + (float3(-1.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_F = dot((___grad).xyz,___p);
                    }
float _G;
                    {
                        float ___t = ((_AA).y) + (_one);
float3 ___p = (_p) + (float3(0.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_G = dot((___grad).xyz,___p);
                    }
float _H;
                    {
                        float ___t = ((_AA).w) + (_one);
float3 ___p = (_p) + (float3(-1.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_H = dot((___grad).xyz,___p);
                    }
n = lerp(lerp(lerp(_A,_B,(_f).x),lerp(_C,_D,(_f).x),(_f).y),lerp(lerp(_E,_F,(_f).x),lerp(_G,_H,(_f).x),(_f).y),(_f).z);
                    }
t =  (t) + ((n) / (f));;
 f =  (f) * (2.0f);;
;
    };_t = t;
                    }
fy = (-0.1f) * (_t);
                    }
float fz;
                    {
                        float _t;
                    {
                        float W = 640.0f;
float t = -0.5f;
float f = 1.0f;

    for (int i=1; i <= 7; i++)
    {
        float n;
                    {
                        float3 _pos = ((localPos) + (dz)) * (f);
float3 _P = (fmod(floor(_pos),256.0f)) / (256.0f);
float3 _p = (_pos) - (floor(_pos));
float _one = (1.0f) / (256.0f);
float3 _f = (((_p) * (_p)) * (_p)) * (((_p) * (((_p) * (6.0f)) - (15.0f))) + (10.0f));
float4 _AA = (permutation.Sample(pointSampler,(_P).xy)) + ((_P).z);
float _A;
                    {
                        float ___t = (_AA).x;
float4 ___grad = gradients.Sample(pointSampler,___t);
_A = dot((___grad).xyz,_p);
                    }
float _B;
                    {
                        float ___t = (_AA).z;
float3 ___p = (_p) + (float3(-1.0f,0.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_B = dot((___grad).xyz,___p);
                    }
float _C;
                    {
                        float ___t = (_AA).y;
float3 ___p = (_p) + (float3(0.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_C = dot((___grad).xyz,___p);
                    }
float _D;
                    {
                        float ___t = (_AA).w;
float3 ___p = (_p) + (float3(-1.0f,-1.0f,0.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_D = dot((___grad).xyz,___p);
                    }
float _E;
                    {
                        float ___t = ((_AA).x) + (_one);
float3 ___p = (_p) + (float3(0.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_E = dot((___grad).xyz,___p);
                    }
float _F;
                    {
                        float ___t = ((_AA).z) + (_one);
float3 ___p = (_p) + (float3(-1.0f,0.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_F = dot((___grad).xyz,___p);
                    }
float _G;
                    {
                        float ___t = ((_AA).y) + (_one);
float3 ___p = (_p) + (float3(0.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_G = dot((___grad).xyz,___p);
                    }
float _H;
                    {
                        float ___t = ((_AA).w) + (_one);
float3 ___p = (_p) + (float3(-1.0f,-1.0f,-1.0f));
float4 ___grad = gradients.Sample(pointSampler,___t);
_H = dot((___grad).xyz,___p);
                    }
n = lerp(lerp(lerp(_A,_B,(_f).x),lerp(_C,_D,(_f).x),(_f).y),lerp(lerp(_E,_F,(_f).x),lerp(_G,_H,(_f).x),(_f).y),(_f).z);
                    }
t =  (t) + ((n) / (f));;
 f =  (f) * (2.0f);;
;
    };_t = t;
                    }
fz = (-0.1f) * (_t);
                    }
float3 dF = (float3((fx) - (f0),(fy) - (f0),(fz) - (f0))) / (epsilon);
norm = normalize((_normal) - (dF));
                    }
float3 intensity;
                    {
                        float3 _worldPos = (input).PositionWS;
float3 _lightVec = (_worldPos) - (Light);
float3 _lightDir = normalize(_lightVec);
float _diffuse;
                    {
                        float __lightFallOff;
                    {
                        float ___lightVecSquared = dot(_lightVec,_lightVec);
__lightFallOff = saturate((LightRangeSquared) / (___lightVecSquared));
                    }
_diffuse = saturate(mul(__lightFallOff,mul(Diffuse,dot(-(_lightDir),norm))));
                    }
float _specular = saturate(mul(Specular,pow(Shine,saturate(dot(norm,normalize((_lightDir) - (normalize((Eye) - (_worldPos)))))))));
intensity = saturate(((AmbientLight) + (_diffuse)) + (_specular));
                    }
return float4(intensity,1.0f); };
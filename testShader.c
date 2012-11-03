'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_64\mscorlib\v4.0_4.0.0.0__b77a5c561934e089\mscorlib.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\PerlinNoise.exe', Symbols loaded.
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\System.Windows.Forms\v4.0_4.0.0.0__b77a5c561934e089\System.Windows.Forms.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\System.Drawing\v4.0_4.0.0.0__b03f5f7f11d50a3a\System.Drawing.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\System\v4.0_4.0.0.0__b77a5c561934e089\System.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\FSharp.Core\v4.0_4.3.0.0__b03f5f7f11d50a3a\FSharp.Core.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\System.Core\v4.0_4.0.0.0__b77a5c561934e089\System.Core.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\Shaders.dll', Symbols loaded.
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\SharpShaders.dll', Symbols loaded.
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\SharpDX.Direct3D11.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\SharpDX.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\SharpDX.DXGI.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\MiniRender.dll', Symbols loaded.
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\AssimpNet.dll', Symbols loaded.
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\System.Configuration\v4.0_4.0.0.0__b03f5f7f11d50a3a\System.Configuration.dll'
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\Microsoft.Net\assembly\GAC_MSIL\System.Xml\v4.0_4.0.0.0__b77a5c561934e089\System.Xml.dll'

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
float3 normal;
{
	float3 _normal = normalize((input).Normal);
	float _f0;
	{
		float __temp_y;
		{
			float ___x;
			{
				float ____temp_y;
				{
					float _____temp_y;
					{
						float ______W = 800.0f;
						float ______t = -0.5f;
						float ______f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float ______p;
							{
								float3 ________pos = (localPos) * (______f);
								float3 ________P = (fmod(floor(________pos),256.0f)) / (256.0f);
								float3 ________p = (________pos) - (floor(________pos));
								float ________one = (1.0f) / (256.0f);
								float3 ________f = (((________p) * (________p)) * (________p)) * (((________p) * (((________p) * (6.0f)) - (15.0f))) + (10.0f));
								float4 ________AA = (permutation.Sample(pointSampler,(________P).xy)) + ((________P).z);
								float4 ________left;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).x);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).x) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).y);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).y) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,1.0f)));
									}
									________left = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________right;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).z);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).z) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).w);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).w) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,1.0f)));
									}
									________right = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________topDown = lerp(________left,________right,(________f).x);
								float2 ________frontBack = lerp((________topDown).xy,(________topDown).zw,(________f).y);
								______p = lerp((________frontBack).x,(________frontBack).y,(________f).z);
							}
							float ______n = abs(______p);
							______t = float ______temp_y;
							{
								float _______temp_x;
								{
									float ________temp_value;
									{
										float3 _________P = (fmod(floor((localPos) * (______f)),256.0f)) / (256.0f);
										float3 _________p = ((localPos) * (______f)) - (floor((localPos) * (______f)));
										float _________one = (1.0f) / (256.0f);
										float3 _________f = (((_________p) * (_________p)) * (_________p)) * (((_________p) * (((_________p) * (6.0f)) - (15.0f))) + (10.0f));
										float4 _________AA = (permutation.Sample(pointSampler,(_________P).xy)) + ((_________P).z);
										float4 _________left;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).x);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).x) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).y);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).y) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,1.0f)));
											}
											_________left = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________right;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).z);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).z) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).w);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).w) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,1.0f)));
											}
											_________right = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________topDown = lerp(_________left,_________right,(_________f).x);
										float2 _________frontBack = lerp((_________topDown).xy,(_________topDown).zw,(_________f).y);
										________temp_value = lerp((_________frontBack).x,(_________frontBack).y,(_________f).z);
									}
									_______temp_x = abs(________temp_value);
								}
								______temp_y = (_______temp_x) / (______f);
							}
							(______t) + (______temp_y);;
							______f =  (______f) * (2.0f);;
							;
						};_____temp_y = ______t;
					}
					____temp_y = (2.0f) * (_____temp_y);
				}
				___x = ((localPos).x) + (____temp_y);
			}
			float ___f = 1.6f;
			float ___PI = 3.141593f;
			float ___t = (0.5f) + ((0.5f) * (sin((((___f) * (2.0f)) * (___PI)) * (___x))));
			__temp_y = ((___t) * (___t)) - (0.5f);
		}
		_f0 = (0.01f) * (__temp_y);
	}
	float _epsilon = 0.01f;
	float3 _dx = float3(_epsilon,0.0f,0.0f);
	float3 _dy = float3(0.0f,_epsilon,0.0f);
	float3 _dz = float3(0.0f,0.0f,_epsilon);
	float _fx;
	{
		float __temp_y;
		{
			float ___x;
			{
				float ____temp_y;
				{
					float _____temp_y;
					{
						float ______W = 800.0f;
						float ______t = -0.5f;
						float ______f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float ______p;
							{
								float3 ________pos = ((localPos) + (_dx)) * (______f);
								float3 ________P = (fmod(floor(________pos),256.0f)) / (256.0f);
								float3 ________p = (________pos) - (floor(________pos));
								float ________one = (1.0f) / (256.0f);
								float3 ________f = (((________p) * (________p)) * (________p)) * (((________p) * (((________p) * (6.0f)) - (15.0f))) + (10.0f));
								float4 ________AA = (permutation.Sample(pointSampler,(________P).xy)) + ((________P).z);
								float4 ________left;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).x);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).x) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).y);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).y) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,1.0f)));
									}
									________left = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________right;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).z);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).z) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).w);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).w) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,1.0f)));
									}
									________right = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________topDown = lerp(________left,________right,(________f).x);
								float2 ________frontBack = lerp((________topDown).xy,(________topDown).zw,(________f).y);
								______p = lerp((________frontBack).x,(________frontBack).y,(________f).z);
							}
							float ______n = abs(______p);
							______t = float ______temp_y;
							{
								float _______temp_x;
								{
									float ________temp_value;
									{
										float3 _________P = (fmod(floor(((localPos) + (_dx)) * (______f)),256.0f)) / (256.0f);
										float3 _________p = (((localPos) + (_dx)) * (______f)) - (floor(((localPos) + (_dx)) * (______f)));
										float _________one = (1.0f) / (256.0f);
										float3 _________f = (((_________p) * (_________p)) * (_________p)) * (((_________p) * (((_________p) * (6.0f)) - (15.0f))) + (10.0f));
										float4 _________AA = (permutation.Sample(pointSampler,(_________P).xy)) + ((_________P).z);
										float4 _________left;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).x);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).x) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).y);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).y) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,1.0f)));
											}
											_________left = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________right;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).z);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).z) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).w);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).w) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,1.0f)));
											}
											_________right = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________topDown = lerp(_________left,_________right,(_________f).x);
										float2 _________frontBack = lerp((_________topDown).xy,(_________topDown).zw,(_________f).y);
										________temp_value = lerp((_________frontBack).x,(_________frontBack).y,(_________f).z);
									}
									_______temp_x = abs(________temp_value);
								}
								______temp_y = (_______temp_x) / (______f);
							}
							(______t) + (______temp_y);;
							______f =  (______f) * (2.0f);;
							;
						};_____temp_y = ______t;
					}
					____temp_y = (2.0f) * (_____temp_y);
				}
				___x = (((localPos) + (_dx)).x) + (____temp_y);
			}
			float ___f = 1.6f;
			float ___PI = 3.141593f;
			float ___t = (0.5f) + ((0.5f) * (sin((((___f) * (2.0f)) * (___PI)) * (___x))));
			__temp_y = ((___t) * (___t)) - (0.5f);
		}
		_fx = (0.01f) * (__temp_y);
	}
	float _fy;
	{
		float __temp_y;
		{
			float ___x;
			{
				float ____temp_y;
				{
					float _____temp_y;
					{
						float ______W = 800.0f;
						float ______t = -0.5f;
						float ______f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float ______p;
							{
								float3 ________pos = ((localPos) + (_dy)) * (______f);
								float3 ________P = (fmod(floor(________pos),256.0f)) / (256.0f);
								float3 ________p = (________pos) - (floor(________pos));
								float ________one = (1.0f) / (256.0f);
								float3 ________f = (((________p) * (________p)) * (________p)) * (((________p) * (((________p) * (6.0f)) - (15.0f))) + (10.0f));
								float4 ________AA = (permutation.Sample(pointSampler,(________P).xy)) + ((________P).z);
								float4 ________left;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).x);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).x) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).y);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).y) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,1.0f)));
									}
									________left = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________right;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).z);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).z) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).w);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).w) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,1.0f)));
									}
									________right = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________topDown = lerp(________left,________right,(________f).x);
								float2 ________frontBack = lerp((________topDown).xy,(________topDown).zw,(________f).y);
								______p = lerp((________frontBack).x,(________frontBack).y,(________f).z);
							}
							float ______n = abs(______p);
							______t = float ______temp_y;
							{
								float _______temp_x;
								{
									float ________temp_value;
									{
										float3 _________P = (fmod(floor(((localPos) + (_dy)) * (______f)),256.0f)) / (256.0f);
										float3 _________p = (((localPos) + (_dy)) * (______f)) - (floor(((localPos) + (_dy)) * (______f)));
										float _________one = (1.0f) / (256.0f);
										float3 _________f = (((_________p) * (_________p)) * (_________p)) * (((_________p) * (((_________p) * (6.0f)) - (15.0f))) + (10.0f));
										float4 _________AA = (permutation.Sample(pointSampler,(_________P).xy)) + ((_________P).z);
										float4 _________left;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).x);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).x) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).y);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).y) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,1.0f)));
											}
											_________left = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________right;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).z);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).z) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).w);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).w) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,1.0f)));
											}
											_________right = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________topDown = lerp(_________left,_________right,(_________f).x);
										float2 _________frontBack = lerp((_________topDown).xy,(_________topDown).zw,(_________f).y);
										________temp_value = lerp((_________frontBack).x,(_________frontBack).y,(_________f).z);
									}
									_______temp_x = abs(________temp_value);
								}
								______temp_y = (_______temp_x) / (______f);
							}
							(______t) + (______temp_y);;
							______f =  (______f) * (2.0f);;
							;
						};_____temp_y = ______t;
					}
					____temp_y = (2.0f) * (_____temp_y);
				}
				___x = (((localPos) + (_dy)).x) + (____temp_y);
			}
			float ___f = 1.6f;
			float ___PI = 3.141593f;
			float ___t = (0.5f) + ((0.5f) * (sin((((___f) * (2.0f)) * (___PI)) * (___x))));
			__temp_y = ((___t) * (___t)) - (0.5f);
		}
		_fy = (0.01f) * (__temp_y);
	}
	float _fz;
	{
		float __temp_y;
		{
			float ___x;
			{
				float ____temp_y;
				{
					float _____temp_y;
					{
						float ______W = 800.0f;
						float ______t = -0.5f;
						float ______f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float ______p;
							{
								float3 ________pos = ((localPos) + (_dz)) * (______f);
								float3 ________P = (fmod(floor(________pos),256.0f)) / (256.0f);
								float3 ________p = (________pos) - (floor(________pos));
								float ________one = (1.0f) / (256.0f);
								float3 ________f = (((________p) * (________p)) * (________p)) * (((________p) * (((________p) * (6.0f)) - (15.0f))) + (10.0f));
								float4 ________AA = (permutation.Sample(pointSampler,(________P).xy)) + ((________P).z);
								float4 ________left;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).x);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).x) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(0.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).y);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).y) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(0.0f,1.0f,1.0f)));
									}
									________left = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________right;
								{
									float ________temp_x;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).z);
										________temp_x = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,0.0f)));
									}
									float ________temp_y;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).z) + (________one));
										________temp_y = dot((____________grad).xyz,(________p) - (float3(1.0f,0.0f,1.0f)));
									}
									float ________temp_z;
									{
										float4 ____________grad = gradients.Sample(pointSampler,(________AA).w);
										________temp_z = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,0.0f)));
									}
									float ________temp_w;
									{
										float4 ____________grad = gradients.Sample(pointSampler,((________AA).w) + (________one));
										________temp_w = dot((____________grad).xyz,(________p) - (float3(1.0f,1.0f,1.0f)));
									}
									________right = float4(________temp_x,________temp_y,________temp_z,________temp_w);
								}
								float4 ________topDown = lerp(________left,________right,(________f).x);
								float2 ________frontBack = lerp((________topDown).xy,(________topDown).zw,(________f).y);
								______p = lerp((________frontBack).x,(________frontBack).y,(________f).z);
							}
							float ______n = abs(______p);
							______t = float ______temp_y;
							{
								float _______temp_x;
								{
									float ________temp_value;
									{
										float3 _________P = (fmod(floor(((localPos) + (_dz)) * (______f)),256.0f)) / (256.0f);
										float3 _________p = (((localPos) + (_dz)) * (______f)) - (floor(((localPos) + (_dz)) * (______f)));
										float _________one = (1.0f) / (256.0f);
										float3 _________f = (((_________p) * (_________p)) * (_________p)) * (((_________p) * (((_________p) * (6.0f)) - (15.0f))) + (10.0f));
										float4 _________AA = (permutation.Sample(pointSampler,(_________P).xy)) + ((_________P).z);
										float4 _________left;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).x);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).x) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(0.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).y);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).y) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(0.0f,1.0f,1.0f)));
											}
											_________left = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________right;
										{
											float __________temp_x;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).z);
												__________temp_x = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,0.0f)));
											}
											float __________temp_y;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).z) + (_________one));
												__________temp_y = dot((_____________grad).xyz,(_________p) - (float3(1.0f,0.0f,1.0f)));
											}
											float __________temp_z;
											{
												float4 _____________grad = gradients.Sample(pointSampler,(_________AA).w);
												__________temp_z = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,0.0f)));
											}
											float __________temp_w;
											{
												float4 _____________grad = gradients.Sample(pointSampler,((_________AA).w) + (_________one));
												__________temp_w = dot((_____________grad).xyz,(_________p) - (float3(1.0f,1.0f,1.0f)));
											}
											_________right = float4(__________temp_x,__________temp_y,__________temp_z,__________temp_w);
										}
										float4 _________topDown = lerp(_________left,_________right,(_________f).x);
										float2 _________frontBack = lerp((_________topDown).xy,(_________topDown).zw,(_________f).y);
										________temp_value = lerp((_________frontBack).x,(_________frontBack).y,(_________f).z);
									}
									_______temp_x = abs(________temp_value);
								}
								______temp_y = (_______temp_x) / (______f);
							}
							(______t) + (______temp_y);;
							______f =  (______f) * (2.0f);;
							;
						};_____temp_y = ______t;
					}
					____temp_y = (2.0f) * (_____temp_y);
				}
				___x = (((localPos) + (_dz)).x) + (____temp_y);
			}
			float ___f = 1.6f;
			float ___PI = 3.141593f;
			float ___t = (0.5f) + ((0.5f) * (sin((((___f) * (2.0f)) * (___PI)) * (___x))));
			__temp_y = ((___t) * (___t)) - (0.5f);
		}
		_fz = (0.01f) * (__temp_y);
	}
	float3 _dF = (float3((_fx) - (_f0),(_fy) - (_f0),(_fz) - (_f0))) / (_epsilon);
	normal = normalize((_normal) - (_dF));
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
		_diffuse = saturate(mul(__lightFallOff,mul(Diffuse,dot(-(_lightDir),normal))));
	}
	float _specular = saturate(mul(Specular,pow(Shine,saturate(dot(normal,normalize((_lightDir) - (normalize((Eye) - (_worldPos)))))))));
	intensity = saturate(((AmbientLight) + (_diffuse)) + (_specular));
}
return float4(intensity,1.0f); };
'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\SharpDX.D3DCompiler.dll'
	A first chance exception of type 'SharpDX.SharpDXException' occurred in SharpDX.dll
	A first chance exception of type 'SharpDX.CompilationException' occurred in SharpDX.D3DCompiler.dll
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\assembly\GAC_MSIL\Microsoft.VisualStudio.Debugger.Runtime\11.0.0.0__b03f5f7f11d50a3a\Microsoft.VisualStudio.Debugger.Runtime.dll'
	The program '[7068] PerlinNoise.exe: Program Trace' has exited with code 0 (0x0).
	The program '[7068] PerlinNoise.exe: Managed (v4.0.30319)' has exited with code -1 (0xffffffff).

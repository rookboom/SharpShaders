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
	float3 Diffuse : TEXCOORD0;

	float3 Specular : TEXCOORD1;

	float Shine : TEXCOORD2;

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
float4 lumpy(PSInput input) : SV_TARGET{ float3 localPos = (input).PositionOS;
float3 normal;
{
	float3 _normal = normalize((input).Normal);
	float f0;
	{
		float temp_y;
		{
			float3 pos = (localPos) * (10.0f);
			float3 P = (fmod(floor(pos),256.0f)) / (256.0f);
			float3 p = (pos) - (floor(pos));
			float one = (1.0f) / (256.0f);
			float3 f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
			float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
			float4 left;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).x);
					temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).y);
					temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
				}
				left = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 right;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).z);
					temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).w);
					temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
				}
				right = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 topDown = lerp(left,right,(f).x);
			float2 frontBack = lerp((topDown).xy,(topDown).zw,(f).y);
			temp_y = lerp((frontBack).x,(frontBack).y,(f).z);
		}
		f0 = (0.03f) * (temp_y);
	}
	float epsilon = 0.00001f;
	float3 dx = float3(epsilon,0.0f,0.0f);
	float3 dy = float3(0.0f,epsilon,0.0f);
	float3 dz = float3(0.0f,0.0f,epsilon);
	float fx;
	{
		float temp_y;
		{
			float3 pos = ((localPos) + (dx)) * (10.0f);
			float3 P = (fmod(floor(pos),256.0f)) / (256.0f);
			float3 p = (pos) - (floor(pos));
			float one = (1.0f) / (256.0f);
			float3 f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
			float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
			float4 left;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).x);
					temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).y);
					temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
				}
				left = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 right;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).z);
					temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).w);
					temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
				}
				right = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 topDown = lerp(left,right,(f).x);
			float2 frontBack = lerp((topDown).xy,(topDown).zw,(f).y);
			temp_y = lerp((frontBack).x,(frontBack).y,(f).z);
		}
		fx = (0.03f) * (temp_y);
	}
	float fy;
	{
		float temp_y;
		{
			float3 pos = ((localPos) + (dy)) * (10.0f);
			float3 P = (fmod(floor(pos),256.0f)) / (256.0f);
			float3 p = (pos) - (floor(pos));
			float one = (1.0f) / (256.0f);
			float3 f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
			float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
			float4 left;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).x);
					temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).y);
					temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
				}
				left = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 right;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).z);
					temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).w);
					temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
				}
				right = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 topDown = lerp(left,right,(f).x);
			float2 frontBack = lerp((topDown).xy,(topDown).zw,(f).y);
			temp_y = lerp((frontBack).x,(frontBack).y,(f).z);
		}
		fy = (0.03f) * (temp_y);
	}
	float fz;
	{
		float temp_y;
		{
			float3 pos = ((localPos) + (dz)) * (10.0f);
			float3 P = (fmod(floor(pos),256.0f)) / (256.0f);
			float3 p = (pos) - (floor(pos));
			float one = (1.0f) / (256.0f);
			float3 f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
			float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
			float4 left;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).x);
					temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).y);
					temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
				}
				left = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 right;
			{
				float temp_x;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).z);
					temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
				}
				float _temp_y;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
					_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
				}
				float temp_z;
				{
					float4 grad = gradients.Sample(pointSampler,(AA).w);
					temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
				}
				float temp_w;
				{
					float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
					temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
				}
				right = float4(temp_x,_temp_y,temp_z,temp_w);
			}
			float4 topDown = lerp(left,right,(f).x);
			float2 frontBack = lerp((topDown).xy,(topDown).zw,(f).y);
			temp_y = lerp((frontBack).x,(frontBack).y,(f).z);
		}
		fz = (0.03f) * (temp_y);
	}
	float3 dF = (float3((fx) - (f0),(fy) - (f0),(fz) - (f0))) / (epsilon);
	normal = normalize((_normal) - (dF));
}
float3 intensity;
{
	float3 lightVec = (Light) - ((input).PositionWS);
	float3 lightDir = normalize(lightVec);
	float lightFallOff;
	{
		float lightVecSquared = dot(lightVec,lightVec);
		lightFallOff = saturate((LightRangeSquared) / (lightVecSquared));
	}
	float3 diffuse = saturate(mul(Diffuse,max(0.0f,dot(lightDir,normal))));
	float3 specular;
	{
		float3 viewer = normalize((Eye) - ((input).PositionWS));
		float3 half_vector = normalize((lightDir) + (viewer));
		specular = saturate(mul(Specular,pow(max(0.0f,dot(normal,half_vector)),Shine)));
	}
	intensity = saturate(((AmbientLight) + (diffuse)) + (specular));
}
return float4(intensity,1.0f); };
float4 marbled(PSInput input) : SV_TARGET{ float3 localPos = (input).PositionOS;
float3 normal;
{
	float3 _normal = normalize((input).Normal);
	float f0;
	{
		float temp_y;
		{
			float x;
			{
				float _temp_y;
				{
					float _temp_y;
					{
						float t = -0.5f;
						float f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float n;
							{
								float temp_value;
								{
									float3 P = (fmod(floor((localPos) * (f)),256.0f)) / (256.0f);
									float3 p = ((localPos) * (f)) - (floor((localPos) * (f)));
									float one = (1.0f) / (256.0f);
									float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
									float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
									float4 left;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).x);
											temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).y);
											temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
										}
										left = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 right;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).z);
											temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).w);
											temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
										}
										right = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 topDown = lerp(left,right,(_f).x);
									float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
									temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
								}
								n = abs(temp_value);
							}
							t =  (t) + ((n) / (f));
							f =  (f) * (2.0f);
							;
						};_temp_y = t;
					}
					_temp_y = (2.0f) * (_temp_y);
				}
				x = ((localPos).x) + (_temp_y);
			}
			float f = 1.6f;
			float PI = 3.141593f;
			float t = (0.5f) + ((0.5f) * (sin((((f) * (2.0f)) * (PI)) * (x))));
			temp_y = ((t) * (t)) - (0.5f);
		}
		f0 = (0.01f) * (temp_y);
	}
	float epsilon = 0.00001f;
	float3 dx = float3(epsilon,0.0f,0.0f);
	float3 dy = float3(0.0f,epsilon,0.0f);
	float3 dz = float3(0.0f,0.0f,epsilon);
	float fx;
	{
		float temp_y;
		{
			float x;
			{
				float _temp_y;
				{
					float _temp_y;
					{
						float t = -0.5f;
						float f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float n;
							{
								float temp_value;
								{
									float3 P = (fmod(floor(((localPos) + (dx)) * (f)),256.0f)) / (256.0f);
									float3 p = (((localPos) + (dx)) * (f)) - (floor(((localPos) + (dx)) * (f)));
									float one = (1.0f) / (256.0f);
									float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
									float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
									float4 left;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).x);
											temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).y);
											temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
										}
										left = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 right;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).z);
											temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).w);
											temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
										}
										right = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 topDown = lerp(left,right,(_f).x);
									float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
									temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
								}
								n = abs(temp_value);
							}
							t =  (t) + ((n) / (f));
							f =  (f) * (2.0f);
							;
						};_temp_y = t;
					}
					_temp_y = (2.0f) * (_temp_y);
				}
				x = (((localPos) + (dx)).x) + (_temp_y);
			}
			float f = 1.6f;
			float PI = 3.141593f;
			float t = (0.5f) + ((0.5f) * (sin((((f) * (2.0f)) * (PI)) * (x))));
			temp_y = ((t) * (t)) - (0.5f);
		}
		fx = (0.01f) * (temp_y);
	}
	float fy;
	{
		float temp_y;
		{
			float x;
			{
				float _temp_y;
				{
					float _temp_y;
					{
						float t = -0.5f;
						float f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float n;
							{
								float temp_value;
								{
									float3 P = (fmod(floor(((localPos) + (dy)) * (f)),256.0f)) / (256.0f);
									float3 p = (((localPos) + (dy)) * (f)) - (floor(((localPos) + (dy)) * (f)));
									float one = (1.0f) / (256.0f);
									float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
									float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
									float4 left;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).x);
											temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).y);
											temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
										}
										left = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 right;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).z);
											temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).w);
											temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
										}
										right = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 topDown = lerp(left,right,(_f).x);
									float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
									temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
								}
								n = abs(temp_value);
							}
							t =  (t) + ((n) / (f));
							f =  (f) * (2.0f);
							;
						};_temp_y = t;
					}
					_temp_y = (2.0f) * (_temp_y);
				}
				x = (((localPos) + (dy)).x) + (_temp_y);
			}
			float f = 1.6f;
			float PI = 3.141593f;
			float t = (0.5f) + ((0.5f) * (sin((((f) * (2.0f)) * (PI)) * (x))));
			temp_y = ((t) * (t)) - (0.5f);
		}
		fy = (0.01f) * (temp_y);
	}
	float fz;
	{
		float temp_y;
		{
			float x;
			{
				float _temp_y;
				{
					float _temp_y;
					{
						float t = -0.5f;
						float f = 1.0f;

						for (int i=1; i <= 7; i++)
						{
							float n;
							{
								float temp_value;
								{
									float3 P = (fmod(floor(((localPos) + (dz)) * (f)),256.0f)) / (256.0f);
									float3 p = (((localPos) + (dz)) * (f)) - (floor(((localPos) + (dz)) * (f)));
									float one = (1.0f) / (256.0f);
									float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
									float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
									float4 left;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).x);
											temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).y);
											temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
										}
										left = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 right;
									{
										float temp_x;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).z);
											temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
										}
										float _temp_y;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
											_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
										}
										float temp_z;
										{
											float4 grad = gradients.Sample(pointSampler,(AA).w);
											temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
										}
										float temp_w;
										{
											float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
											temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
										}
										right = float4(temp_x,_temp_y,temp_z,temp_w);
									}
									float4 topDown = lerp(left,right,(_f).x);
									float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
									temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
								}
								n = abs(temp_value);
							}
							t =  (t) + ((n) / (f));
							f =  (f) * (2.0f);
							;
						};_temp_y = t;
					}
					_temp_y = (2.0f) * (_temp_y);
				}
				x = (((localPos) + (dz)).x) + (_temp_y);
			}
			float f = 1.6f;
			float PI = 3.141593f;
			float t = (0.5f) + ((0.5f) * (sin((((f) * (2.0f)) * (PI)) * (x))));
			temp_y = ((t) * (t)) - (0.5f);
		}
		fz = (0.01f) * (temp_y);
	}
	float3 dF = (float3((fx) - (f0),(fy) - (f0),(fz) - (f0))) / (epsilon);
	normal = normalize((_normal) - (dF));
}
float3 intensity;
{
	float3 lightVec = (Light) - ((input).PositionWS);
	float3 lightDir = normalize(lightVec);
	float lightFallOff;
	{
		float lightVecSquared = dot(lightVec,lightVec);
		lightFallOff = saturate((LightRangeSquared) / (lightVecSquared));
	}
	float3 diffuse = saturate(mul(Diffuse,max(0.0f,dot(lightDir,normal))));
	float3 specular;
	{
		float3 viewer = normalize((Eye) - ((input).PositionWS));
		float3 half_vector = normalize((lightDir) + (viewer));
		specular = saturate(mul(Specular,pow(max(0.0f,dot(normal,half_vector)),Shine)));
	}
	intensity = saturate(((AmbientLight) + (diffuse)) + (specular));
}
return float4(intensity,1.0f); };
float4 crinkled(PSInput input) : SV_TARGET{ float3 localPos = (input).PositionOS;
float3 normal;
{
	float3 _normal = normalize((input).Normal);
	float f0;
	{
		float temp_y;
		{
			float t = -0.5f;
			float f = 1.0f;

			for (int i=1; i <= 7; i++)
			{
				float n;
				{
					float temp_value;
					{
						float3 P = (fmod(floor((localPos) * (f)),256.0f)) / (256.0f);
						float3 p = ((localPos) * (f)) - (floor((localPos) * (f)));
						float one = (1.0f) / (256.0f);
						float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
						float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
						float4 left;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).x);
								temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).y);
								temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
							}
							left = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 right;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).z);
								temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).w);
								temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
							}
							right = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 topDown = lerp(left,right,(_f).x);
						float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
						temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
					}
					n = abs(temp_value);
				}
				t =  (t) + ((n) / (f));
				f =  (f) * (2.0f);
				;
			};temp_y = t;
		}
		f0 = (-0.15f) * (temp_y);
	}
	float epsilon = 0.00001f;
	float3 dx = float3(epsilon,0.0f,0.0f);
	float3 dy = float3(0.0f,epsilon,0.0f);
	float3 dz = float3(0.0f,0.0f,epsilon);
	float fx;
	{
		float temp_y;
		{
			float t = -0.5f;
			float f = 1.0f;

			for (int i=1; i <= 7; i++)
			{
				float n;
				{
					float temp_value;
					{
						float3 P = (fmod(floor(((localPos) + (dx)) * (f)),256.0f)) / (256.0f);
						float3 p = (((localPos) + (dx)) * (f)) - (floor(((localPos) + (dx)) * (f)));
						float one = (1.0f) / (256.0f);
						float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
						float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
						float4 left;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).x);
								temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).y);
								temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
							}
							left = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 right;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).z);
								temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).w);
								temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
							}
							right = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 topDown = lerp(left,right,(_f).x);
						float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
						temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
					}
					n = abs(temp_value);
				}
				t =  (t) + ((n) / (f));
				f =  (f) * (2.0f);
				;
			};temp_y = t;
		}
		fx = (-0.15f) * (temp_y);
	}
	float fy;
	{
		float temp_y;
		{
			float t = -0.5f;
			float f = 1.0f;

			for (int i=1; i <= 7; i++)
			{
				float n;
				{
					float temp_value;
					{
						float3 P = (fmod(floor(((localPos) + (dy)) * (f)),256.0f)) / (256.0f);
						float3 p = (((localPos) + (dy)) * (f)) - (floor(((localPos) + (dy)) * (f)));
						float one = (1.0f) / (256.0f);
						float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
						float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
						float4 left;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).x);
								temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).y);
								temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
							}
							left = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 right;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).z);
								temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).w);
								temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
							}
							right = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 topDown = lerp(left,right,(_f).x);
						float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
						temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
					}
					n = abs(temp_value);
				}
				t =  (t) + ((n) / (f));
				f =  (f) * (2.0f);
				;
			};temp_y = t;
		}
		fy = (-0.15f) * (temp_y);
	}
	float fz;
	{
		float temp_y;
		{
			float t = -0.5f;
			float f = 1.0f;

			for (int i=1; i <= 7; i++)
			{
				float n;
				{
					float temp_value;
					{
						float3 P = (fmod(floor(((localPos) + (dz)) * (f)),256.0f)) / (256.0f);
						float3 p = (((localPos) + (dz)) * (f)) - (floor(((localPos) + (dz)) * (f)));
						float one = (1.0f) / (256.0f);
						float3 _f = (((p) * (p)) * (p)) * (((p) * (((p) * (6.0f)) - (15.0f))) + (10.0f));
						float4 AA = (permutation.Sample(pointSampler,(P).xy)) + ((P).z);
						float4 left;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).x);
								temp_x = dot((grad).xyz,(p) - (float3(0.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).x) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(0.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).y);
								temp_z = dot((grad).xyz,(p) - (float3(0.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).y) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(0.0f,1.0f,1.0f)));
							}
							left = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 right;
						{
							float temp_x;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).z);
								temp_x = dot((grad).xyz,(p) - (float3(1.0f,0.0f,0.0f)));
							}
							float _temp_y;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).z) + (one));
								_temp_y = dot((grad).xyz,(p) - (float3(1.0f,0.0f,1.0f)));
							}
							float temp_z;
							{
								float4 grad = gradients.Sample(pointSampler,(AA).w);
								temp_z = dot((grad).xyz,(p) - (float3(1.0f,1.0f,0.0f)));
							}
							float temp_w;
							{
								float4 grad = gradients.Sample(pointSampler,((AA).w) + (one));
								temp_w = dot((grad).xyz,(p) - (float3(1.0f,1.0f,1.0f)));
							}
							right = float4(temp_x,_temp_y,temp_z,temp_w);
						}
						float4 topDown = lerp(left,right,(_f).x);
						float2 frontBack = lerp((topDown).xy,(topDown).zw,(_f).y);
						temp_value = lerp((frontBack).x,(frontBack).y,(_f).z);
					}
					n = abs(temp_value);
				}
				t =  (t) + ((n) / (f));
				f =  (f) * (2.0f);
				;
			};temp_y = t;
		}
		fz = (-0.15f) * (temp_y);
	}
	float3 dF = (float3((fx) - (f0),(fy) - (f0),(fz) - (f0))) / (epsilon);
	normal = normalize((_normal) - (dF));
}
float3 intensity;
{
	float3 lightVec = (Light) - ((input).PositionWS);
	float3 lightDir = normalize(lightVec);
	float lightFallOff;
	{
		float lightVecSquared = dot(lightVec,lightVec);
		lightFallOff = saturate((LightRangeSquared) / (lightVecSquared));
	}
	float3 diffuse = saturate(mul(Diffuse,max(0.0f,dot(lightDir,normal))));
	float3 specular;
	{
		float3 viewer = normalize((Eye) - ((input).PositionWS));
		float3 half_vector = normalize((lightDir) + (viewer));
		specular = saturate(mul(Specular,pow(max(0.0f,dot(normal,half_vector)),Shine)));
	}
	intensity = saturate(((AmbientLight) + (diffuse)) + (specular));
}
return float4(intensity,1.0f); };
'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\Users\Johan\SkyDrive\Coding\SharpShaders\Samples\PerlinNoise\bin\Debug\SharpDX.D3DCompiler.dll'
	A first chance exception of type 'SharpDX.SharpDXException' occurred in SharpDX.dll
	A first chance exception of type 'SharpDX.CompilationException' occurred in SharpDX.D3DCompiler.dll
	'PerlinNoise.exe' (Managed (v4.0.30319)): Loaded 'C:\WINDOWS\assembly\GAC_MSIL\Microsoft.VisualStudio.Debugger.Runtime\11.0.0.0__b03f5f7f11d50a3a\Microsoft.VisualStudio.Debugger.Runtime.dll'

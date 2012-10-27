namespace SharpShaders.Shaders

open SharpDX
open SharpDX.Direct3D11
open SharpShaders.Math
open System.Runtime.InteropServices
open SharpShaders
open System

// This is a direct translation of Perlin's original Java implementation
module PerlinReference =
   let private p = 
       let permutation = 
        [| 151;160;137;91;90;15;
            131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
            190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
            88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
            77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
            102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
            135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
            5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
            223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
            129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
            251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
            49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
            138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180
            |]
       Array.init 512 (fun i -> permutation.[i%256])

   let noise(_x:float, _y:float, _z:float) =
      let X,Y,Z = int(Math.Floor(_x)) &&& 255,              // FIND UNIT CUBE THAT
                  int(Math.Floor(_y)) &&& 255,              // CONTAINS POINT.
                  int(Math.Floor(_z)) &&& 255                  
      let x,y,z = _x - Math.Floor(_x),                        // FIND RELATIVE X,Y,Z
                  _y - Math.Floor(_y),                               // OF POINT IN CUBE.
                  _z - Math.Floor(_z)
      let fade t = t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
      let u,v,w = fade(x),                                // COMPUTE FADE CURVES
                  fade(y),                                // FOR EACH OF X,Y,Z.
                  fade(z)
      let A  = p.[X  ]+Y          // HASH COORDINATES OF
      let AA = p.[A]+Z           // THE 8 CUBE CORNERS,
      let AB = p.[A+1]+Z
      let B  = p.[X+1]+Y
      let BA = p.[B]+Z
      let BB = p.[B+1]+Z;      

      let lerp(t, a, b) = a + t * (b - a)
      let grad(hash, x, y, z) =
          let h = hash &&& 15                    // CONVERT LO 4 BITS OF HASH CODE
          let u = if h<8 then x else y                 // INTO 12 GRADIENT DIRECTIONS.
          let v = if h<4 then y else if h=12||h=14 then x else z
          (if (h&&&1) = 0 then u else -u) + (if (h&&&2) = 0 then v else -v)
          
      lerp(w, lerp(v,       lerp(u, grad(p.[AA  ], x  , y  , z   ),  // AND ADD
                                    grad(p.[BA  ], x-1.0, y  , z   )), // BLENDED
                            lerp(u, grad(p.[AB  ], x  , y-1.0, z   ),  // RESULTS
                                    grad(p.[BB  ], x-1.0, y-1.0, z   ))),// FROM  8
                    lerp(v, lerp(u, grad(p.[AA+1], x  , y  , z-1.0 ),  // CORNERS
                                    grad(p.[BA+1], x-1.0, y  , z-1.0 )), // OF CUBE
                            lerp(u, grad(p.[AB+1], x  , y-1.0, z-1.0 ),
                                    grad(p.[BB+1], x-1.0, y-1.0, z-1.0 ))));
module PerlinTexture =
    let private permutation = 
            [| 151;160;137;91;90;15;
            131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
            190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
            88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
            77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
            102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
            135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
            5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
            223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
            129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
            251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
            49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
            138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180
            |]
    let private gradients = [|
            float3(1.0f,1.0f,0.0f);    float3(-1.0f,1.0f,0.0f);    float3(1.0f,-1.0f,0.0f);    float3(-1.0f,-1.0f,0.0f);
            float3(1.0f,0.0f,1.0f);    float3(-1.0f,0.0f,1.0f);    float3(1.0f,0.0f,-1.0f);    float3(-1.0f,0.0f,-1.0f);
            float3(0.0f,1.0f,1.0f);    float3(0.0f,-1.0f,1.0f);    float3(0.0f,1.0f,-1.0f);    float3(0.0f,-1.0f,-1.0f);
            float3(1.0f,1.0f,0.0f);    float3(0.0f,-1.0f,1.0f);    float3(-1.0f,1.0f,0.0f);    float3(0.0f,-1.0f,-1.0f)|]

    let private perm t = permutation.[t%256]
    let permutation2D =
        let perm2D x y =
            let A  = perm(x) + y
            let AA = perm(A)
            let AB = perm(A + 1)
            let B  = perm(x + 1) + y
            let BA = perm(B)
            let BB = perm(B + 1)
            float4(float32 AA, float32 AB, float32 BA, float32 BB) / 255.0f;
        Array2D.init 256 256 perm2D
    let permutedGradients =
        let permGrad x y = float4(gradients.[(perm x) % 16], 1.0f)
        Array2D.init 256 1 permGrad

module Marble =
    [<Struct; ConstantPacking>]
    type MaterialConstants(octaves:int32, initialFrequency:float32, amplitude:float32,lacunarity:float32)  =
        member m.Octaves = octaves
        member m.InitialFrequency = initialFrequency
        member m.Amplitude = amplitude
        member m.Lacunarity = lacunarity

    [<Struct>]
    type PSInput(p:float4, wp:float3, op:float3, n:float3) =
        member m.PositionHS = p
        member m.PositionWS = wp
        member m.PositionOS = op
        member m.Normal = n

    type Shader(scene:BlinnPhong.SceneConstants,
                obj:BlinnPhong.ObjectConstants,
                mat:BlinnPhong.MaterialConstants,
                permutation:Texture,
                gradients:Texture,
                pointSampler:SamplerStateDescription) =

        [<ShaderFunction>]
        let cornerNoise(random:float32) p x y z = 
            let grad = gradients.Sample(pointSampler, random)
            dot grad.xyz (p-float3(x,y,z))

        [<ShaderFunction>]
        let gradperm (t:float32) p =
            let grad = gradients.Sample(pointSampler, t)
            dot grad.xyz p

        [<ShaderFunction>]
        let perlin (pos:float3) =
            let perm2d(pos:float2) = 
                permutation.Sample(pointSampler, pos)

            //let P = (floor(pos) % 256.0f) / 256.0f
            let P = fmod(floor(pos), 256.0f) / 256.0f
            let p = pos - floor(pos)
            let one = 1.0f / 256.0f
            let f = p * p * p * (p * (p * 6.0f - 15.0f) + 10.0f)

            // Find the random noise values at the 8 corners of the surrounding unit cube
            let AA = perm2d(P.xy) + P.z
            // AND ADD BLENDED RESULTS FROM 8 CORNERS OF CUBE
            lerpf( lerpf( lerpf( gradperm AA.x p ,  
                                 gradperm AA.z (p + float3(-1.0f,  0.0f, 0.0f) ), f.x),
                           lerpf( gradperm AA.y (p + float3(0.0f, -1.0f, 0.0f) ),
                                 gradperm AA.w (p + float3(-1.0f, -1.0f, 0.0f) ), f.x), f.y),
                             
                     lerpf( lerpf( gradperm (AA.x+one)  (p + float3( 0.0f,  0.0f, -1.0f) ),
                                 gradperm (AA.z+one)    (p + float3(-1.0f,  0.0f, -1.0f) ), f.x),
                           lerpf( gradperm (AA.y+one)   (p + float3( 0.0f, -1.0f, -1.0f) ),
                                 gradperm (AA.w+one)    (p + float3(-1.0f, -1.0f, -1.0f) ), f.x), f.y), f.z); 
            (*let corner t x y z = cornerNoise t p x y z
            let left =  float4(corner (AA.x)     0.0f 0.0f 0.0f,
                               corner (AA.x+one) 0.0f 0.0f 1.0f,
                               corner (AA.y)     0.0f 1.0f 0.0f,
                               corner (AA.y+one) 0.0f 1.0f 1.0f)
            let right = float4(corner (AA.z)     1.0f 0.0f 0.0f,
                               corner (AA.z+one) 1.0f 0.0f 1.0f,
                               corner (AA.w)     1.0f 1.0f 0.0f,
                               corner (AA.w+one) 1.0f 1.0f 1.0f)
            // Think of the cube vertices as two quads, one at x=0 and one at x=1
            // Reduce the cube to a single quad by interpolating in x
            let topDown = lerp(left, right, f.x)
            // The quad vertices form two line sections, one at y = 0 and one at y = 1
            // Reduce the quad to a single line section by interpolating in y
            let frontBack = lerp(topDown.xy, topDown.zw, f.y)
            // Then reduce the line section to a point by interpolating in z
            lerpf(frontBack.x, frontBack.y, f.z) *)

        [<ShaderFunction>]
        let color lightDirection (materialDiffuse:float3) normal = 
            normal
            |> normalize  
            |> dot -lightDirection
            |> (*) materialDiffuse
            |> saturate
            |> (+) (float3(0.3f,0.3f,0.3f))
            |> saturate

        [<ShaderFunction>]
        let stripes x f =
            let PI = 3.14159265f;
            let t = 0.5f + 0.5f * sin(f * 2.0f*PI * x);
            t * t - 0.5f;

        [<ShaderFunction>]
        let fbm(pos:float3) =
            let Octaves = 10
            let Lacunarity = 2.0f
            let Gain = 0.5f
            
            let mutable value = 0.0f
            let mutable freq = 1.0f
            let mutable amp = 0.5f

            for i in 1..Octaves do
                value <- value + perlin(pos*freq)*amp
                freq <- freq * Lacunarity
                amp <- amp * Gain
            value

        [<ShaderFunction>]
        let turbulance(pos:float3) =
            let W = 640.0f // W = Image width in pixels
            let mutable t = -0.5f
            let mutable f = 1.0f
            let absNoise = perlin >> abs
            for i in 1..7 do
                t <- t + absNoise(pos*f)/f
                f <- f*2.0f
            t



            (*
        [<ShaderFunction>]
        let turbulance pos f =
            let W = 640.0f // W = Image width in pixels
            let rec loop f t =
                if f < W/12.0f then
                    perlin pos f
                    |> (/) f
                    |> abs 
                    |> loop(f*2.0f)
                else
                    t
            loop -0.5f f *)

        // If needed we can use a 3D texture lookup instead. Do some performance profiling to see the difference
        member m.noise3D() =
            let noise i j k = perlin(float3(float32 i/256.0f, float32 j/256.0f, float32 k/256.0f))
            Array3D.init 256 256 256 noise

        member m.noise2D() =
            let octave = 8.0f
            //let noise i j = float32(PerlinReference.noise((float i)/octave, (float j)/octave, 0.0))
            let noise i j = perlin (float3(float32 i/octave, float32 j/octave, 0.0f))
            Array2D.init 512 512 noise

        [<ShaderEntry>]
        member m.vertex(input:Diffuse.VSInput) =
            let worldPos = input.Position * obj.World
            PSInput(input.Position * obj.WorldViewProjection,
                    worldPos.xyz,
                    input.Position.xyz,
                    input.Normal * float3x3(obj.World ))   

        [<ShaderEntry>]
        member m.pixel(input:PSInput) =
            let lumpy(pos:float3) = 0.03f*perlin(pos*8.0f)
            let marbled(pos:float3) = 0.01f*(stripes(pos.x + 2.0f*turbulance(pos)) 1.6f)
            let crinkled(pos:float3) = -0.1f*turbulance(pos)
            let localPos = input.PositionOS
            let bump pos normal F =
                let f0 = F(pos)
                let epsilon = 0.0001f
                let dx = float3(epsilon,0.0f,0.0f)
                let dy = float3(0.0f,epsilon,0.0f)
                let dz = float3(0.0f,0.0f,epsilon)
                let fx = F(pos + dx)
                let fy = F(pos + dy)
                let fz = F(pos + dz)
                let dF = float3(fx-f0,fy-f0,fz-f0)/epsilon
                normal - dF|> normalize
            let normal = bump localPos input.Normal marbled
            let intensity =
                let worldPos = input.PositionWS
                let lightVec = worldPos - scene.Light
                let lightDir = normalize lightVec
                let diffuse = 
                    let lightFallOff = 
                        let lightVecSquared = (lightVec |> dot lightVec)
                        scene.LightRangeSquared/lightVecSquared
                        |> saturatef
                    normal 
                    |> dot -lightDir
                    |> mul mat.Diffuse
                    |> mul lightFallOff
                    |> saturatef
                let specular = scene.Eye - worldPos
                               |> normalize
                               |> subtractFrom lightDir
                               |> normalize
                               |> dot normal
                               |> saturatef
                               |> pow mat.Shine
                               |> mul mat.Specular
                               |> saturatef

                scene.AmbientLight + diffuse + specular
                |> saturate

            float4(intensity, 1.0f)
            (*let fbmNoise (pos:float3) f =
                let amplitude = mat.Amplitude
                let n = f(pos)
                // First octave
                let v1 = amplitude * n
                let amp1 = amplitude * amplitude
                // Second octave
                let v2 = v1 + amp1 * n
                let amp2 = amp1 * amplitude
                // Third octave
                let v3 = v2 + amp2 * n
                let amp3 = amp2 * amplitude
                // Fourth octave
                v3 + amp3 * n
            let diffuse = fbmNoise input.PositionWS absNoise
            let grayMarble = float3(diffuse, diffuse, diffuse)
            let color = Diffuse.color scene.LightDirection grayMarble input.Normal
            float4(color, 1.0f)
            *)
    
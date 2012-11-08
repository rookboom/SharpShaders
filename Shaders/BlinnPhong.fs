namespace SharpShaders.Shaders

open SharpDX
open SharpDX.Direct3D11
open System.Runtime.InteropServices
open SharpShaders.Math
open SharpShaders

module BlinnPhong =
   [<Struct; StructLayout(LayoutKind.Explicit, Size=48)>]
    type SceneConstants_IfYouDoNotHavePostSharp =
        [<FieldOffset(0)>]  val Eye             : float3
        [<FieldOffset(16)>] val Light  : float3
        [<FieldOffset(32)>] val AmbientLight    : float3
        [<FieldOffset(44)>] val LightRangeSquared  : float32
        with
        new(eye, light, ambientLight, lightRange) = { 
            Eye = eye
            Light = light
            AmbientLight = ambientLight
            LightRangeSquared = lightRange }

   [<Struct; ConstantPacking>]
    type SceneConstants(eye:float3, light:float3, ambientLight:float3, lightRangeSquared:float32) =
        member m.Eye = eye
        member m.Light = light
        member m.AmbientLight = ambientLight
        member m.LightRangeSquared = lightRangeSquared
        
    [<Struct; ConstantPacking>]
    type MaterialConstants(ambient:float32, diffuse:float32, specular:float32,shine:float32)  =
        member m.Ambient = ambient
        member m.Diffuse = diffuse
        member m.Specular = specular
        member m.Shine = shine

    [<Struct; ConstantPacking>]
    type ObjectConstants(wvp:float4x4, w:float4x4) =
        member m.WorldViewProjection = wvp
        member m.World = w
    
    [<Struct>]
    type VSInput(p:float4, n:float3, uv:float2) =
        member m.Position = p
        member m.Normal = n
        member m.UV = uv

    [<Struct>]
    type PSInput(p:float4, wp:float3, n:float3, uv:float2) =
        member m.PositionHS = p
        member m.PositionWS = wp
        member m.Normal = n
        member m.UV = uv

    [<ShaderFunction>]
    let intensity (scene:SceneConstants) (mat:MaterialConstants) worldPos normal =
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

        let lightColor = float3(1.0f,1.0f,1.0f)
        scene.AmbientLight + (diffuse + specular)*lightColor
        |> saturate

    type Shader(scene:SceneConstants,
                obj:ObjectConstants,
                mat:MaterialConstants, 
                diffuseTexture:Texture, 
                linearSampler:SamplerStateDescription) =

        [<ShaderEntry>]
        member m.vertex(input:VSInput) =
            let worldPos = input.Position * obj.World
            PSInput(input.Position * obj.WorldViewProjection,
                    worldPos.xyz,
                    input.Normal * float3x3(obj.World),
                    input.UV)    

        [<ShaderEntry>]
        member m.pixel(input:PSInput) =
            let tex = diffuseTexture.Sample(linearSampler, input.UV)
            let surface = intensity scene
                                    mat
                                    input.PositionWS
                                    (normalize input.Normal)
                                    
            let color = tex.rgb * surface 
            float4(color, 1.0f)



open SharpShaders
open SharpShaders.Math
open SharpShaders.Shaders
open SharpDX.Windows
open SharpDX
open System.Windows.Forms
open System.Diagnostics

let width, height = 640, 480
let form = new Form(Visible = true, Text = "Blue skies", Width = width, Height = height)
let run() = 
    let hlsl = ShaderTranslator.toHLSL typeof<Diffuse.Shader>
    let inputElements = 
        InputElements.map typeof<Vertex> typeof<Diffuse.VSInput>
        |> Seq.toArray

    System.Diagnostics.Debug.WriteLine(hlsl);

    use renderer = new MiniRender(form.Handle, width, height)
    
    let eye = Vector3(0.0f,0.0f,-6.0f)
    let sceneConstants, matConstants, objectConstants = 
        let light = -Vector3.Normalize(eye)
        Diffuse.SceneConstants(float3(light)),
        Diffuse.MaterialConstants(float3(1.0f,1.0f,0.0f)),
        Diffuse.ObjectConstants(float4x4.identity,
                                float4x4.identity)

    let updateObjectConstants = 
        renderer.createVertexShader hlsl inputElements objectConstants
    let updateOtherConstants =
        renderer.createPixelShader hlsl (sceneConstants, matConstants)
    let draw = renderer.createScene(Geometry.cube 1.0f)
    let viewProjection = renderer.createViewProjection(eye)
    let sw = Stopwatch.StartNew()

    let render() =
        let world = 
            let time = float32(sw.ElapsedMilliseconds)/1000.0f
            Matrix.RotationYawPitchRoll(time, 2.0f*time, 0.0f)
        Diffuse.ObjectConstants(fromMatrix(world*viewProjection), fromMatrix(world))
        |> updateObjectConstants
        draw()
    RenderLoop.Run(form, render)
    
run()

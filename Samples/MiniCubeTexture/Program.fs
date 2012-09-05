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
    let hlsl = ShaderTranslator.toHLSL typeof<BlinnPhong.Shader>
    let inputElements = 
        InputElements.map typeof<Vertex>
        |> Seq.toArray

    System.Diagnostics.Debug.WriteLine(hlsl);

    use renderer = new MiniRender(form.Handle, width, height)
    
    let eye = Vector3(0.0f,0.0f,-6.0f)
    let sceneConstants, matConstants, objectConstants = 
        let light = eye
        BlinnPhong.SceneConstants(float3(eye), float3(light), float3(0.1f,0.1f,0.1f), 10.0f),
        BlinnPhong.MaterialConstants(0.1f, 0.5f, 0.5f, 30.0f),
        BlinnPhong.ObjectConstants(fromMatrix(Matrix.Identity),
                                   fromMatrix(Matrix.Identity))

    let updateObjectConstants = 
        renderer.createVertexShader hlsl inputElements objectConstants
    let updateOtherConstants =
        renderer.createPixelShader hlsl (sceneConstants, matConstants)
    do renderer.createTextures ["GeneticaMortarlessBlocks.jpg"]
    let draw = renderer.createScene(Geometry.cube 1.0f)
    let viewProjection = renderer.createViewProjection(eye)
    let sw = Stopwatch.StartNew()

    let render() =
        let world = 
            let time = float32(sw.ElapsedMilliseconds)/1000.0f
            Matrix.RotationYawPitchRoll(time, 2.0f*time, 0.0f)
        BlinnPhong.ObjectConstants(fromMatrix(world*viewProjection), fromMatrix(world))
        |> updateObjectConstants
        draw()
    RenderLoop.Run(form, render)
    
run()

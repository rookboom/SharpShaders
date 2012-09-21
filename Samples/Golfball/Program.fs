open SharpShaders
open SharpShaders.Math
open SharpShaders.Shaders
open SharpDX.Windows
open SharpDX
open System.Windows.Forms
open System.Diagnostics
open Assimp

let width, height = 640, 480
let form = new Form(Visible = true, Text = "Golfball", Width = width, Height = height)
let run() = 
    let hlsl = ShaderTranslator.toHLSL typeof<BlinnPhong.Shader>
    let inputElements = 
        InputElements.map typeof<Vertex> typeof<BlinnPhong.VSInput>
        |> Seq.toArray

    System.Diagnostics.Debug.WriteLine(hlsl);

    use renderer = new MiniRender(form.Handle, width, height)
    
    let eye = Vector3(0.0f,0.0f,-6.0f)
    let sceneConstants, matConstants, objectConstants = 
        let light = eye + Vector3(5.0f, 5.0f, 0.0f)
        BlinnPhong.SceneConstants(float3(eye), float3(light), float3(0.01f,0.01f,0.01f), 20.0f),
        BlinnPhong.MaterialConstants(0.1f, 0.5f, 0.5f, 2.0f),
        BlinnPhong.ObjectConstants(fromMatrix(Matrix.Identity),
                                   fromMatrix(Matrix.Identity))

    let updateObjectConstants = 
        renderer.createVertexShader hlsl inputElements objectConstants
    let updateOtherConstants =
        renderer.createPixelShader hlsl (sceneConstants, matConstants)
    let geometry =
        use import = ModelImporter.import "Golfball.obj"
        Geometry.fromMesh import.Scene.Meshes.[0]
    do renderer.createTextures ["Smiley.png"]
    let draw = renderer.createScene(geometry)
    let viewProjection = renderer.createViewProjection(eye)
    let sw = Stopwatch.StartNew()

    let render() =
        let world = 
            let time = float32(sw.ElapsedMilliseconds)/1000.0f
            let rotation = Matrix.RotationYawPitchRoll(time, 2.0f*time, 0.0f)
            let scale = Matrix.Scaling(0.05f)
            scale*rotation
        BlinnPhong.ObjectConstants(fromMatrix(world*viewProjection), fromMatrix(world))
        |> updateObjectConstants
        draw()
    RenderLoop.Run(form, render)
    
run()

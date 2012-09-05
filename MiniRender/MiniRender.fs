namespace SharpShaders

open System
open System.Collections.Generic
open SharpDX
open SharpDX.Direct3D
open SharpDX.Direct3D11
open SharpDX.DXGI
open SharpDX.D3DCompiler
open SharpDX.Windows
//open SharpDXExt
open System.Runtime.InteropServices
open SharpShaders


[<Struct; StructLayout(LayoutKind.Sequential)>]
type Vertex(p:Vector3, n:Vector3, uv:Vector2) =
    member m.Position = p
    member m.Normal = n
    member m.UV = uv


/// A very basic renderer to facilitate creating quick shader tutorials
/// with the minimum boilerplate code.
/// It only renders a single object and the camera always points at the origin                
type MiniRender(windowHandle, width, height) =
    let device, swapChain =
        let modeDescription = new ModeDescription(  
                                Width = width,
                                Height = height,
                                Format = Format.R8G8B8A8_UNorm,
                                RefreshRate = new Rational(60,1))
    
        let swapChainDescription =  new SwapChainDescription(   
                                        BufferCount = 1,
                                        ModeDescription = modeDescription,
                                        Usage = Usage.RenderTargetOutput,
                                        OutputHandle = windowHandle,
                                        SampleDescription = new SampleDescription(Count = 1),
                                        IsWindowed = true,
                                        SwapEffect = SwapEffect.Discard)
    
        let _, device, swapChain = Device.CreateWithSwapChain(
                                            DriverType.Hardware,
                                            DeviceCreationFlags.Debug,
                                            swapChainDescription )
        device, swapChain

    let disposables = List<IDisposable>()
    do disposables.Add(device)
    do disposables.Add(swapChain)
    let createConstantBuffer(size) = 
        let vsConstBufferDesc = new BufferDescription(
                                    BindFlags = BindFlags.ConstantBuffer,
                                    SizeInBytes = size, 
                                    CpuAccessFlags = CpuAccessFlags.Write,
                                    Usage = ResourceUsage.Dynamic)
        new Direct3D11.Buffer( device, vsConstBufferDesc )

    let deviceContext = device.ImmediateContext
    do disposables.Add(deviceContext)

    let updateShaderConstants constBuffer (data:'a) = 
        let _, dataStream = deviceContext.MapSubresource(
                                        constBuffer, 
                                        MapMode.WriteDiscard, 
                                        SharpDX.Direct3D11.MapFlags.None)
        dataStream.Write(data)
        deviceContext.UnmapSubresource( constBuffer, 0 ) 

    let renderTargetView = 
        use renderTarget = Resource.FromSwapChain<Texture2D>(swapChain, 0)
        new RenderTargetView(device, renderTarget)
    do disposables.Add(renderTargetView)
    //Create a viewport with the same size as the window
    let viewport =
        Viewport(
            Width = float32(width), 
            Height = float32(height), 
            MinDepth = 0.0f, 
            MaxDepth = 1.0f,
            TopLeftX = 0.0f,
            TopLeftY = 0.0f)

    do deviceContext.OutputMerger.SetTargets([|renderTargetView|] )
    do deviceContext.Rasterizer.SetViewports([|viewport|])

    let compile(code:string) (entry:string) (target:string) =
            ShaderBytecode.Compile(code, 
                                    entry, 
                                    target, 
                                    ShaderFlags.None, 
                                    EffectFlags.None).Bytecode

    let linearSampler = 
        let desc = SamplerStateDescription( Filter = Filter.MinMagMipLinear,
                                            AddressU = TextureAddressMode.Wrap,
                                            AddressV = TextureAddressMode.Wrap,
                                            AddressW = TextureAddressMode.Wrap,
                                            MaximumAnisotropy = 1,
                                            ComparisonFunction = Comparison.Always,
                                            MaximumLod = System.Single.MaxValue)
        new SamplerState(device, desc)
    do disposables.Add(linearSampler)

    do deviceContext.PixelShader.SetSampler(0, linearSampler)

    let createBuffer setBuffer i (constants:'a)  =
        let buffer = createConstantBuffer(sizeof<'a>)
        disposables.Add(buffer)
        updateShaderConstants buffer constants
        do setBuffer i buffer
        updateShaderConstants buffer

    interface IDisposable with
        member m.Dispose() =
            disposables.ForEach(fun d -> d.Dispose())


    member m.createVertexShader code inputElements constants =
        let vsByteCode = compile code "vertex" "vs_5_0"
        disposables.Add(vsByteCode)

        let inputLayout =
            new InputLayout(
                device, 
                vsByteCode, 
                inputElements)
        disposables.Add(inputLayout)

        do deviceContext.InputAssembler.PrimitiveTopology <- 
            PrimitiveTopology.TriangleList
        do deviceContext.InputAssembler.InputLayout <- inputLayout

        let vertexShader = new VertexShader(device, vsByteCode)
        disposables.Add(vertexShader)
        do deviceContext.VertexShader.Set(vertexShader)

        let setBuffer i b = deviceContext.VertexShader.SetConstantBuffer(i,b)
        createBuffer setBuffer 0 constants


    /// Creates and initializes the pixel shader and the scene and material constants.
    /// Returns callbacks for updating each of the constants
    member m.createPixelShader code constants =
        let pixelShader =
            let psByteCode = compile code "pixel" "ps_5_0"
            disposables.Add(psByteCode)
            new PixelShader(device, psByteCode)
        disposables.Add(pixelShader)
        do deviceContext.PixelShader.Set(pixelShader)

        let setBuffer i b = deviceContext.PixelShader.SetConstantBuffer(i,b)
        let scene, mat = constants
        createBuffer setBuffer 0 scene,
        createBuffer setBuffer 1 mat

    /// Loads texture files and adds them to the pixel shader stage
    member m.createTextures textureFiles =
        let setTexture i filename =
            let textureView = TextureLoader.fromFile device filename
            disposables.Add(textureView)
            do deviceContext.PixelShader.SetShaderResource(0, textureView)

        textureFiles
        |> List.iteri setTexture


    /// Creates a view projection matrix that looks at the origin
    member m.createViewProjection eye =
        let lookAt, up = 
            Vector3.Zero, 
            Vector3(0.0f,1.0f,0.0f)
        // Look at the origin
        let view = Matrix.LookAtLH(eye, lookAt, up)
                                  
        let fovy, aspect, near, far = 
            float32(Math.PI)/4.0f, 
            float32(width)/float32(height), 
            1.0f, 
            20.0f 
        let projection = Matrix.PerspectiveFovLH(fovy, aspect, near, far)
        view*projection


    /// Creates the 'scene' from the provided object vertices.
    /// Returns a draw callback to render the scene
    member m.createScene vertices =
        let vertexSize = sizeof<Vertex>
        let vertexData = 
            let vertex(position,normal,uv) = 
                Vertex(position, normal, uv)
            vertices |> Array.map vertex
        let vertexBufferDesc =  BufferDescription( 
                                    BindFlags   = BindFlags.VertexBuffer,
                                    SizeInBytes = vertexSize*(Array.length vertices),
                                    Usage       = ResourceUsage.Immutable)
        let buffer = Direct3D11.Buffer.Create(device, 
                            vertexData, 
                            vertexBufferDesc)
        let vertexBuffer = VertexBufferBinding(buffer, vertexSize, 0)
        let vertexCount = Array.length vertices

        let draw() =
            do deviceContext.ClearRenderTargetView(
                renderTargetView,
                Color4(0.2f, 0.2f, 0.5f,1.0f))

            do deviceContext.InputAssembler.SetVertexBuffers(0, vertexBuffer)
            do deviceContext.Draw(vertexCount, 0) 
            
            do swapChain.Present(0, PresentFlags.None) |> ignore
        draw




(* Copyright (c) 2012 SharpShaders - Johan Verwey
   See the file license.txt for copying permission. *)
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
open SharpShaders.Math


[<Struct; StructLayout(LayoutKind.Sequential)>]
type Vertex(p:Vector3, n:Vector3, uv:Vector2) =
    member m.Position = p
    member m.Normal = n
    member m.UV = uv


/// A very basic renderer to facilitate creating quick shader tutorials
/// with the minimum boilerplate code.
/// It only renders a single object and the camera always points at the origin                
type MiniRender(windowHandle, width, height) =
    let disposables = List<IDisposable>()
    let disposable d =
        disposables.Add(d)
        d

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
        disposable device, disposable swapChain


    let createConstantBuffer(size) = 
        let vsConstBufferDesc = new BufferDescription(
                                    BindFlags = BindFlags.ConstantBuffer,
                                    SizeInBytes = size, 
                                    CpuAccessFlags = CpuAccessFlags.Write,
                                    Usage = ResourceUsage.Dynamic)
        new Direct3D11.Buffer( device, vsConstBufferDesc )

    let deviceContext = disposable device.ImmediateContext

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
        |> disposable
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
                                            AddressU = TextureAddressMode.Clamp,
                                            AddressV = TextureAddressMode.Clamp,
                                            AddressW = TextureAddressMode.Clamp,
                                            MaximumAnisotropy = 16,
                                            ComparisonFunction = Comparison.Always,
                                            MaximumLod = System.Single.MaxValue)
        disposable(new SamplerState(device, desc))

    do deviceContext.PixelShader.SetSampler(0, linearSampler)

    let createBuffer (constants:'a)  =
        let buffer = createConstantBuffer(sizeof<'a>)
        disposable buffer

    interface IDisposable with
        member m.Dispose() =
            disposables.ForEach(fun d -> d.Dispose())


    member m.createVertexShader code inputElements constants =
        let vsByteCode = compile code "vertex" "vs_5_0"
                         |> disposable

        let inputLayout =
            new InputLayout(
                device, 
                vsByteCode, 
                inputElements)
            |> disposable

        do deviceContext.InputAssembler.PrimitiveTopology <- 
            PrimitiveTopology.TriangleList
        do deviceContext.InputAssembler.InputLayout <- inputLayout

        let vertexShader = new VertexShader(device, vsByteCode)
                           |> disposable
        do deviceContext.VertexShader.Set(vertexShader)

        let buffer = createBuffer constants
        deviceContext.VertexShader.SetConstantBuffer(0,buffer)
        // upload the initial constants
        updateShaderConstants buffer constants
        // return callback to update future constants
        updateShaderConstants buffer


    /// Creates and initializes the pixel shader and the scene and material constants.
    /// Returns callbacks for updating each of the constants
    member m.createPixelShader code constants =
        m.createPixelShaderWithEntry "pixel" code constants

    member m.createPixelShaderWithEntry entry code constants =
        let pixelShader, cb0 =
            let psByteCode = compile code entry "ps_5_0"
                             |> disposable

            use shaderReflection = new ShaderReflection(psByteCode)
            let constantBufferName(i:int) =
                let cb = shaderReflection.GetConstantBuffer(i)
                try
                    cb.Description.Name
                with
                | _ -> ""

            new PixelShader(device, psByteCode) |> disposable,
            constantBufferName 0
        do deviceContext.PixelShader.Set(pixelShader)

        let scene, mat = constants
        let sceneBuffer = createBuffer scene
        let matBuffer = createBuffer mat
        let sceneIndex, matIndex = 
            if cb0 = scene.GetType().Name then
                0,1
            else
                1,0
        deviceContext.PixelShader.SetConstantBuffer(sceneIndex,sceneBuffer)
        deviceContext.PixelShader.SetConstantBuffer(matIndex,matBuffer)
        // upload the initial constants
        updateShaderConstants sceneBuffer scene
        updateShaderConstants matBuffer mat
        // return callback to update future constants
        (updateShaderConstants sceneBuffer),
        (updateShaderConstants matBuffer)

    /// Loads texture files and adds them to the pixel shader stage
    member m.createTextures textureFiles =
        let setTexture i filename =
            let textureView = TextureLoader.fromFile device filename
                              |> disposable
            do deviceContext.PixelShader.SetShaderResource(i, textureView)

        textureFiles
        |> List.iteri setTexture

    member m.createSampler(desc:SamplerStateDescription) =  
        disposable(new SamplerState(device, desc))
        
    member m.createTexture2D data format =
            let width, height = Array2D.length1 data, Array2D.length2 data
            
            let desc =  new Texture2DDescription( 
                                    Width = width,
                                    Height = height,
                                    MipLevels = 1,
                                    ArraySize = 1,
                                    Format = format,
                                    BindFlags   = BindFlags.ShaderResource,
                                    SampleDescription = SampleDescription(Count = 1),
                                    CpuAccessFlags = CpuAccessFlags.Write,
                                    Usage       = ResourceUsage.Dynamic,
                                    OptionFlags = ResourceOptionFlags.None)

            let tex = new Texture2D(device, desc) |> disposable
            let _, dataStream = deviceContext.MapSubresource(
                                            tex, 
                                            0,
                                            MapMode.WriteDiscard, 
                                            SharpDX.Direct3D11.MapFlags.None)
(*            let write (f:float4) = 
                if format = Format.R32G32B32A32_Float then
                    dataStream.Write(f.x)
                    dataStream.Write(f.y)
                    dataStream.Write(f.z)
                    dataStream.Write(f.w)
                else
                    let rgba = float4.toRgba f
                    dataStream.Write(rgba.ToBgra()) *)
                
            if dataStream.CanWrite then
                for y in 0..height-1 do
                    for x in 0..width-1 do
                        let rgba = float4.toRgba data.[x,y]
                        dataStream.Write(rgba.ToBgra())
                deviceContext.UnmapSubresource( tex, 0 ) 

            new ShaderResourceView(device, tex)

    member m.setTextures textures =
        let setTexture i (view, sampler) =
            do deviceContext.PixelShader.SetShaderResource(i, view)
            do deviceContext.PixelShader.SetSampler(i, sampler)

        textures
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
            
            do swapChain.Present(1, PresentFlags.None) |> ignore
        draw




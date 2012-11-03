namespace Flow.Test

open Xunit
open SharpShaders
open SharpShaders.Shaders
open SharpShaders.Math
open System
open System.Text
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open SharpDX
open SharpDX.Direct3D11

module Assert =
    
    let EqualIgnoreWhitespace(s1:string, s2:string) =
        let trimWS s = Regex.Replace(s, @"\s", "")
        Assert.Equal(trimWS s1, trimWS s2)

[<Struct; StructLayout(LayoutKind.Sequential)>]
type TestVertex(p:Vector3, n:Vector3, uv:Vector2) =
    member m.Position = p
    member m.Normal = n
    member m.UV = uv

//=======================================================================================
type TestShaderWithSingleLineLookup(mat:Simplistic.MaterialConstants) =
    [<ReflectedDefinition>]
    member m.pixel(input:Simplistic.PSInput) = mat.MaterialDiffuse

//=======================================================================================
type TestShaderEndingWithLookup(mat:Simplistic.MaterialConstants) =
    [<ReflectedDefinition>]
    member m.pixel(input:Simplistic.PSInput) = 
        let x = 5.0f
        mat.MaterialDiffuse

//=======================================================================================
type TestShaderWithMultipleIndirection(mat:Simplistic.MaterialConstants) =
    [<ReflectedDefinition>]
    member m.pixel(input:Simplistic.PSInput) =
        float4(input.PositionHS.xyz,1.0f)

//=======================================================================================
type TestShaderWithTextureFetch(gradients:Texture,
                                pointSampler:SamplerStateDescription) =
    [<ReflectedDefinition>]
    member m.pixel(input:Simplistic.PSInput) =
        let x = 0.0f
        gradients.Sample(pointSampler, x)

//=======================================================================================
type TestShaderWithMatrixCast(obj:Shaders.BlinnPhong.ObjectConstants) =
    [<ReflectedDefinition>]
    member m.pixel(input:Simplistic.PSInput) = 
        input.PositionHS.xyz * float3x3(obj.World)
         
//=======================================================================================
module ShaderGenerationTests =
    let assertShader expected (t:Type) =
        let shaderCode = ShaderTranslator.shaderMethods t

        match shaderCode |> Seq.toList with
        | [s] ->
            Assert.EqualIgnoreWhitespace(expected, s)
        | _ -> failwith "Expected exactly 1 shader method for this dummy shader"         

    //=======================================================================================
    type TestShaderWithExternalMethod(mat:Simplistic.MaterialConstants) =
        [<ShaderFunction>]
        let color x y z = float3(x,y,z)

        [<ShaderEntry>]
        member m.pixel(input:Simplistic.PSInput) =
            let final x = color x (0.5f+1.0f) 1.0f
            let pos = 2.0f*(final 3.0f)
            float4(pos,2.0f)

    [<Fact>]
    let ``External methods should be inlined``() =
        let expectedPS =  @"float4 pixel(PSInput input) : SV_TARGET
{
    float3 pos;
    {
        float3 _temp_y;
        {
            float __y=(0.5f)+(1.0f);
            float __z = 1.0f;
            _temp_y = float3(3.0f, __y, __z);
        }
        pos = (2.0f)*(_temp_y);
    }
    return float4(pos, 2.0f);
};"
        let shaderCode = ShaderTranslator.shaderMethods typeof<TestShaderWithExternalMethod>
        match shaderCode |> Seq.toList with
        | [f1] ->
            Assert.EqualIgnoreWhitespace(expectedPS, f1)
        | _ -> failwith "Expected exactly 1 shader method"

(*    [<Fact>]
    let ``External methods should be inserted before the shader``() =
        let expectedHelper = @"
float3 color(float x, float y, float z)
{
    return float3(x,y,z);
};"
        let expectedPS =  @"float4 pixel(PSInput input) : SV_TARGET
{
    return float4(color(3.0f,(0.5f)+(1.0f),1.0f), 1.0f);
};"
        let shaderCode = ShaderTranslator.shaderMethods typeof<TestShaderWithExternalMethod>
        match shaderCode |> Seq.toList with
        | [f1;f2] ->
            Assert.EqualIgnoreWhitespace(expectedHelper, f1)
            Assert.EqualIgnoreWhitespace(expectedPS, f2)
        | _ -> failwith "Expected exactly 2 shader methods" *)

    [<Fact>]
    let ``Should use multiplication insteadofmul operator for scalar types``() =
        let expr = <@ let x = 1.0f * float3(1.0f,1.0f, 1.0f)
                      x @>
        Assert.EqualIgnoreWhitespace(@"
float3 x = (1.0f) * (float3(1.0f,1.0f,1.0f));
return x;", ShaderTranslator.methodBody expr)      
    
    [<Fact>]
    let ``Should generate constantBuffer in HLSL``() =            
        let expectedObject = @"
cbuffer ObjectConstants
{
    row_major matrix WorldViewProjection :TEXCOORD0;
};"
        let expectedMaterial = @"
cbuffer MaterialConstants
{
    float4 MaterialDiffuse:TEXCOORD0;
};"

        let constantsCode = ShaderTranslator.constants typeof<Simplistic.Shader>
        match constantsCode |> Seq.toList with
        | [objectConstants; materialConstants] ->
            Assert.EqualIgnoreWhitespace(expectedObject, objectConstants)
            Assert.EqualIgnoreWhitespace(expectedMaterial, materialConstants)
        | _ -> failwith "Expected exactly 2 constants declarations"

    [<Fact>]
    let ``Should generate constantBuffer for BlinnPhong``() =            
        let expectedObject = @"
cbuffer ObjectConstants
{
    row_major matrix WorldViewProjection :TEXCOORD0;
    row_major matrix World :TEXCOORD1;
};"

        let constantsCode = ShaderTranslator.constants typeof<BlinnPhong.Shader>
        match constantsCode |> Seq.toList with
        | [_;objectConstants; _] ->
            Assert.EqualIgnoreWhitespace(expectedObject, objectConstants)
        | _ -> failwith "Expected exactly 3 constants declarations"

    [<Fact>]
    let ``Should allow shaders to return a variable expression``() =
        let expr = <@ let x = float4(1.0f, 1.0f, 1.0f, 1.0f)
                      x @>
        Assert.EqualIgnoreWhitespace(@"
float4 x = float4(1.0f, 1.0f, 1.0f, 1.0f);
return x;", ShaderTranslator.methodBody expr)          

    
    [<Fact>]
    let ``Should allow pixel shaders that end with FieldGet expression as only line``() =
        let expectedVS = @"
float4 pixel(PSInput input) : SV_TARGET
{
    return MaterialDiffuse;
};"
        assertShader expectedVS typeof<TestShaderWithSingleLineLookup>
    [<Fact>]
    let ``A texture fetch should not be considered an external method call``() =
        let expectedVS = @"
float4 pixel(PSInput input) : SV_TARGET
{
    float x = 0.0f;
    return gradients.Sample(pointSampler, x);
};"
        assertShader expectedVS typeof<TestShaderWithTextureFetch>

    [<Fact>]
    let ``Should allow pixel shaders that end with FieldGet expression after other statements``() =
        let expectedVS = @"
float4 pixel(PSInput input) : SV_TARGET
{
    float x = 5.0f;
    return MaterialDiffuse;
};"
        assertShader expectedVS typeof<TestShaderEndingWithLookup>

    [<Fact>]
    let ``Should allow multiple indirections on structs``() =
        let expectedVS = @"
float4 pixel(PSInput input) : SV_TARGET
{
    return float4(((input).PositionHS).xyz,1.0f);
};"
        assertShader expectedVS typeof<TestShaderWithMultipleIndirection>

    [<Fact>]
    let ``Should allow matrix conversion``() =
        let expectedVS = @"
float3 pixel(PSInput input): SV_TARGET
{
   return mul(((input).PositionHS).xyz, (float3x3)(World));
};"
        assertShader expectedVS typeof<TestShaderWithMatrixCast>

    [<Fact>]
    let ``Should convert 'subtract' to - operator``() =
        let expected = @"return (2) - (1);"
        Assert.Equal(expected, ShaderTranslator.methodBody <@ subtract 1 2 @>)

    [<Fact>]
    let ``Should convert 'subtractFrom' to - operator``() =
        let expected = @"return (1) - (2);"
        Assert.Equal(expected, ShaderTranslator.methodBody <@ subtractFrom 1 2 @>)

    [<Fact>]
    let ``Should generate shader input structs in HLSL``() =            
        let expectedVSInput = @"
struct VSInput
{
    float4 Position : POSITION;
};"
        let expectedPSInput = @"
struct PSInput
{
    float4 PositionHS : SV_POSITION;
};"

        let shaderType = typeof<Simplistic.Shader>
        let inputStructs = ShaderTranslator.inputStructs shaderType
        match inputStructs |> Seq.toList with
        | [vsInput; psInput] -> 
            Assert.EqualIgnoreWhitespace(expectedVSInput, vsInput)
            Assert.EqualIgnoreWhitespace(expectedPSInput, psInput)
        | _ -> failwith(sprintf "Expected exactly 2 input structs %A" inputStructs)


    [<Fact>]
    let ``Should generate shader input elements from input type``() =            
        let inputElements = InputElements.map typeof<TestVertex> typeof<Simplistic.VSInput>
        let input = Seq.head(inputElements)
        Assert.Equal("POSITION", input.SemanticName)
        Assert.Equal(DXGI.Format.R32G32B32_Float, input.Format)
        Assert.Equal(InputClassification.PerVertexData, input.Classification)

    [<Fact>]
    let ``ShouldT tanslate shaders to HLSL``() =            
        let expectedVS = @"
PSInput vertex(VSInput input)
{
    PSInput o;
    o.PositionHS = mul((input).Position,WorldViewProjection);
    return o;
};"
        let expectedPS =  @"float4 pixel(PSInput input) : SV_TARGET
{
    return float4(1.0f,0.0f,1.0f,1.0f);
};"
        let shaderCode = ShaderTranslator.shaderMethods typeof<Simplistic.Shader>
        match shaderCode |> Seq.toList with
        | [vs;ps] ->
            Assert.EqualIgnoreWhitespace(expectedVS, vs)
            Assert.EqualIgnoreWhitespace(expectedPS, ps)
        | _ -> failwith "Expected exactly 2 shader methods"
            
    [<Fact>]
    let ``Should generate vertex shader input struct with normal and UVs``() = 
        let expectedStruct = @"
struct VSInput
{
    float4 Position : POSITION;
    float3 Normal : NORMAL;
    float2 UV : TEXCOORD0;
};"
        let inputType = typeof<Shaders.BlinnPhong.VSInput>

        let inputDecl = ShaderTranslator.formatStruct inputType
        Assert.EqualIgnoreWhitespace(expectedStruct, inputDecl)

    [<Fact>]
    let ``Should generate pixel shader input struct with normal and UVs``() = 
        let expectedStruct = @"
struct PSInput
{
    float4 PositionHS : SV_POSITION;
    float3 PositionWS : TEXCOORD0;
    float3 Normal : NORMAL;
    float2 UV : TEXCOORD1;
};"
        let inputType = typeof<Shaders.BlinnPhong.PSInput>

        let inputDecl = ShaderTranslator.formatStruct inputType
        Assert.EqualIgnoreWhitespace(expectedStruct, inputDecl)

    [<Fact>]
    let ``Should generate input elements with normal and UVs``() = 
        let inputType = typeof<Shaders.BlinnPhong.VSInput>
        let vertexType = typeof<TestVertex>
        let inputElements = InputElements.map vertexType inputType
        match inputElements |> Seq.toList with
        | [position;normal;uv] ->
            Assert.Equal("POSITION", position.SemanticName)
            Assert.Equal(0, position.AlignedByteOffset)
            Assert.Equal(DXGI.Format.R32G32B32_Float, position.Format)
            Assert.Equal(InputClassification.PerVertexData, position.Classification)

            Assert.Equal("NORMAL", normal.SemanticName)
            Assert.Equal(sizeof<float3>, normal.AlignedByteOffset)
            Assert.Equal(DXGI.Format.R32G32B32_Float, normal.Format)
            Assert.Equal(InputClassification.PerVertexData, normal.Classification)

            Assert.Equal("TEXCOORD", uv.SemanticName)
            Assert.Equal(sizeof<float3> + sizeof<float3>, uv.AlignedByteOffset)
            Assert.Equal(DXGI.Format.R32G32_Float, uv.Format)
            Assert.Equal(InputClassification.PerVertexData, uv.Classification)
        | _ -> failwith "Expected exactly 3 input elements."
    
    [<Fact>]
    let ``Should generate constants textures and shaders from constructor parameters``() = 
        let constants = ShaderTranslator.constants typeof<BlinnPhong.Shader>
        let textures = ShaderTranslator.textures typeof<BlinnPhong.Shader>
        let samplers = ShaderTranslator.samplers typeof<BlinnPhong.Shader>
        
        Assert.Equal(3, Seq.length constants)
        Assert.Equal(1, Seq.length textures)
        Assert.EqualIgnoreWhitespace("Texture2D diffuseTexture : register(t0);", Seq.head(textures))
        Assert.Equal(1, Seq.length samplers)
        Assert.EqualIgnoreWhitespace("SamplerState linearSampler : register(s0);", Seq.head(samplers))

    let assert16ByteAligned size = 
            Assert.True(0 = size % 16, @"Shader constant buffer size should be 16 byte aligned. 
Pad the last field or set the size using explicit packing.")

    [<Fact>]
    let ``Vector subfields should be accessible ``() =
        let expr = <@   let v = float3(1.0f,1.0f,1.0f)
                        let x = v.x
                        x @>
        let expected = @"
            float3 v = float3(1.0f,1.0f,1.0f);
            float x = (v).x;
            return x;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Temporaries should be declared before assignments``() =
        let expr = <@   let foo x = 
                            let t = x*x
                            t
                        let mutable x = 0
                        x <- foo 5
                        x @>
        let expected = @"
            int x = 0;
            int t = (5)*(5);
            x = t;
            return x;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Higher order functions should be supported ``() =
        let expr = <@   
                    let turbulance(pos:int) =
                        let x = pos
                        x*x
                    let bump (F:int->int) (pos:int) = F(pos)
                    let marbled(pos:int) = 
                        pos + 2*turbulance(pos)
                    bump marbled 1
                @>
        let expected = @"
            int temp _y;
            {
                int _temp_y;
                {
                    int __x = 1;
                    _temp_y = (__x)*(__x);
                }
                temp_y = (2)*(_temp_y);
            }
            return (1) + (temp_y);"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)
    
    [<Fact>]
    let ``Method calls should modify variable names inside scope to prevent clashes``() =

        let expr = <@ 
                      let perlin(x:float32) =
                          let P = abs(x)
                          P
                      let x = perlin(1.0f)
                      6.0f*x@>
        let expected = @"
            float x;
            {
                float _P = abs(1.0f);
                x=_P;
            }
            return (6.0f)*(x);"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Functions should be inlined ``() =
        let expr = 
            <@  let foo x = x*x
                let y = foo 5
                y  @>
        let expected = @"
            int y = (5)*(5);
            return y;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Higher order functions should be inlined ``() =
        let expr = 
            <@  let square x = x*x
                let double x = x+x
                let add x f = x + f(x)
                let y = abs(add 5 square)
                let z = add 4 double
                y+z*y  @>
        let expected = @"
            int y = abs((5) + ((5)*(5)));
            int z = (4) + ((4)+(4));
            return (y)+((z)*(y));"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``External higher order functions should be inlined in its own scope ``() =
        let expr = 
            <@  
                let bar(F:float32->float32) (z:float32) =
                    let x = F(z*2.0f)
                    x  
                let square x = x*x
                let y = bar square 5.0f
                y  @>
        let expected = @"
            float y;
            {
                float  _x = ((5.0f)*(2.0f))*((5.0f)*(2.0f));
                y = _x;
            }
            return y;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Modulus operator should be supported``() =
        let expr =  <@
                        let y = (5) % (2)
                        y  @>
        let expected = @"
            int y = (5) % (2);
            return y;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Function composition with existing function``() =
        let expr = 
            <@  let perlin x = x*x
                let absNoise = perlin >> abs
                let y = absNoise 5
                y  @>
        let expected = @"
            int y = abs((5)*(5));
            return y;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Multiple function composition ``() =
        let expr = 
            <@  let square x = x*x
                let double x = x+x
                let half x = x/2
                let halfSquaredDouble = double >> square >> half
                let y = halfSquaredDouble 5
                y  @>
        let expected = @"
            int y = (((5)+(5))*((5)+(5)))/(2);
            return y;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Unary negation``() =
        let expr = 
            <@  let color lightDir normal = 
                    normal 
                    |> dot -lightDir
                float3(1.0f, color (float3(0.1f,0.2f,0.3f)) (float3(1.0f,2.0f,3.0f)), 4.0f)  @>
        let expected = @"
            return float3(1.0f,dot(-(float3(0.1f,0.2f,0.3f)),float3(1.0f,2.0f,3.0f)), 4.0f);"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Should inline temporaries``() =
        let expr = 
            <@  let calc x = 
                    x 
                    |> mul 3
                    |> mul 4
                    |> mul 5
                calc 2  @>
        let expected = @"
            return mul(5,mul(4,mul(3, 2)));"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Should inline composed functions``() =
        let expr = 
            <@  let calc = 
                    mul 3
                    >> mul 4
                calc 2  @>
        let expected = @"
            return mul(4,mul(3, 2)))"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Scoping problem``() =
        let expr = <@ 
                      let calc(x:float32) =
                          let p = abs(x)
                          p
                      float3(calc(1.0f), calc(2.0f), calc(3.0f))
                      @>
        let expected = @"
            float temp_x;
            {
                float _p = abs(1.0f);
                temp_x = _p;
            }
            float temp_y;
            {
                float _p = abs(2.0f);
                temp_y = _p;
            }
            float temp_z;
            { 
                float _p = abs(3.0f);
                temp_z = _p;
            }
            return float3(temp_x,temp_y,temp_z);"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    let ``Variables inside scope cannot have the same name as variables outside``() =
        let expr = <@ 
                      let calc(t:float32) =
                          let x = abs(t)
                          x
                      let x = calc 4.0f
                      x
                      @>
        let expected = @"
            float x;
            {
                float _x = abs(4.0f);
                x = _x;
            }
            return x;"
        Assert.EqualIgnoreWhitespace(expected, ShaderTranslator.methodBody expr)

    [<Fact>]
    /// <see>http://msdn.microsoft.com/en-us/library/bb509632.aspx</see>
    let ``Shader constant buffers and inputs should conform to HLSL packing rules ``() =
        let verify(t:Type) =
            Assert.True(   t.IsExplicitLayout
                        || t.IsLayoutSequential, "All HLSL types need either sequential or explicit layout")
            let verifyBoundary offset (field:FieldInfo) =
                let fieldOffsetAttrib = field.GetCustomAttribute(typeof<FieldOffsetAttribute>) :?> FieldOffsetAttribute
                let fieldSize = Marshal.SizeOf(field.FieldType)
                let align boundary n = 
                    if n % boundary = 0 then
                        n
                    else
                        n + boundary - n % boundary
                if not(offset % 4 = 0) then
                    // We need explicit packing to enforce placing fields on a four byte boundary
                    Assert.NotNull(fieldOffsetAttrib)
                    let alignedOffset = align 4 offset
                    Assert.Equal(alignedOffset, fieldOffsetAttrib.Value)
                    alignedOffset + fieldSize
                else
                    let alignedOffset = align 16 offset
                    let nextOffset = offset + fieldSize
                    if nextOffset > alignedOffset then
                        // It is only acceptable to cross a 16 byte boundary if the field size is bigger
                        // than 16 bytes and the field starts on a 16 byte boundary.
                        if (fieldSize < 16 || offset % 16 <> 0) then
                            // We need explicit packing to enforce fields not crossing a 16 byte boundary
                            Assert.True(not(fieldOffsetAttrib = null) && (alignedOffset = fieldOffsetAttrib.Value), 
                                sprintf "The %s field of %s with size %d starting at offset %d crosses a 16 byte boundary" 
                                    field.Name t.Name fieldSize offset)
                        else if not(fieldOffsetAttrib = null) then
                            Assert.Equal(offset, fieldOffsetAttrib.Value)
                        alignedOffset + fieldSize
                    else
                        // if the offset is correctly aligned, we do not need a fieldOffset attribute
                        // But if there is one, it better be correct
                        if not(fieldOffsetAttrib = null) then
                            Assert.Equal(offset, fieldOffsetAttrib.Value)
                        nextOffset

            let fields = t.GetFields(BindingFlags.DeclaredOnly ||| BindingFlags.Instance ||| BindingFlags.NonPublic)
            let size = fields
                       |> Seq.fold verifyBoundary 0
            /// The size of shader constants should always be 16 byte aligned
            if t.IsExplicitLayout then 
                let paddedSize = size
                assert16ByteAligned paddedSize
                Assert.Equal(t.StructLayoutAttribute.Size, paddedSize)
            else 
                assert16ByteAligned size

            

        ShaderTranslator.constantTypes typeof<Shaders.BlinnPhong.Shader>
        |> Seq.iter verify 

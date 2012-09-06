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
type TestShaderWithMatrixCast(obj:Shaders.BlinnPhong.ObjectConstants) =
    [<ReflectedDefinition>]
    member m.ConvertWorld(input:Simplistic.PSInput) = 
        input.PositionHS.xyz * float3x3(obj.World)
         
//=======================================================================================
module ShaderGenerationTests =
    [<Fact>]
    let ``Should use multiplication insteadofmul operator for scalar types``() =
        let expr = <@ let x = 1.0f * float3(1.0f,1.0f, 1.0f)
                      x @>
        Assert.EqualIgnoreWhitespace(@"
float3 x = (1) * (float3(1,1,1));
return x;", ShaderTranslator.hlsl expr)      
    
    [<Fact>]
    let ``Should generate constantBuffer in HLSL``() =            
        let expectedObject = @"
cbuffer ObjectConstants
{
    row_major matrix WorldViewProjection;
};"
        let expectedMaterial = @"
cbuffer MaterialConstants
{
    float4 MaterialDiffuse;
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
    row_major matrix WorldViewProjection;
    row_major matrix World;
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
float4 x = float4(1, 1, 1, 1);
return x;", ShaderTranslator.hlsl expr)          

    let assertShader expected (t:Type) =
        let shaderCode = ShaderTranslator.shaders t

        match shaderCode |> Seq.toList with
        | [s] ->
            Assert.EqualIgnoreWhitespace(expected, s)
        | _ -> failwith "Expected exactly 1 shader method for this dummy shader"         

    [<Fact>]
    let ``Should allow pixel shaders that end with FieldGet expression as only line``() =
        let expectedVS = @"
float4 pixel(PSInput input)
{
    return MaterialDiffuse;
};"
        assertShader expectedVS typeof<TestShaderWithSingleLineLookup>

    [<Fact>]
    let ``Should allow pixel shaders that end with FieldGet expression after other statements``() =
        let expectedVS = @"
float4 pixel(PSInput input)
{
    float x = 5;
    return MaterialDiffuse;
};"
        assertShader expectedVS typeof<TestShaderEndingWithLookup>

    [<Fact>]
    let ``Should allow multiple indirections on structs``() =
        let expectedVS = @"
float4 pixel(PSInput input) : SV_TARGET
{
    return float4((input).PositionHS.xyz,1);
};"
        assertShader expectedVS typeof<TestShaderWithMultipleIndirection>

    [<Fact>]
    let ``Should allow matrix conversion``() =
        let expectedVS = @"
float3 ConvertWorld(PSInput input)
{
   return mul((input).PositionHS.xyz, (float3x3)(World));
};"
        assertShader expectedVS typeof<TestShaderWithMatrixCast>

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
    o.PositionHS = mul(input.Position,WorldViewProjection);
    return o;
};"
        let expectedPS =  @"float4 pixel(PSInput input) : SV_TARGET
{
    return float4(1,0,1,1);
};"
        let shaderCode = ShaderTranslator.shaders typeof<Simplistic.Shader>
        match shaderCode |> Seq.toList with
        | [ps;vs] ->
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
    /// <see>http://msdn.microsoft.com/en-us/library/bb509632.aspx</see>
    let ``Shader constant buffers and inputs should conform to HLSL packing rules ``() =
        let verify(t:Type) =
            Assert.True(   t.IsExplicitLayout
                        || t.IsLayoutSequential, "All HLSL types need either sequential or explicit layout")
            let verifyBoundary offset (field:FieldInfo) =
                let fieldOffsetAttrib = field.GetCustomAttribute(typeof<FieldOffsetAttribute>) :?> FieldOffsetAttribute
                let fieldSize = Marshal.SizeOf(field.FieldType)
                if not(offset % 4 = 0) then
                    // We need explicit packing to enforce placing fields on a four byte boundary
                    Assert.NotNull(fieldOffsetAttrib)
                    let alignedOffset = offset + offset%4
                    Assert.Equal(alignedOffset, fieldOffsetAttrib.Value)
                    alignedOffset + fieldSize
                else
                    let alignedOffset = offset + 16 - offset%16
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
                let paddedSize = t.StructLayoutAttribute.Size
                assert16ByteAligned paddedSize
                Assert.Equal(t.StructLayoutAttribute.Size, paddedSize)
            else 
                assert16ByteAligned size

            

        ShaderTranslator.constantTypes typeof<Shaders.BlinnPhong.Shader>
        |> Seq.iter verify 

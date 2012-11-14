(* Copyright (c) 2012 SharpShaders - Johan Verwey
   See the file license.txt for copying permission. *)
namespace SharpShaders

open SharpDX
open SharpDX.Direct3D11
open System
open System.Reflection
open System.Runtime.InteropServices

//=================================================================================================
module InputFormats =
    let private inputFormats = dict ["UV", DXGI.Format.R32G32_Float]
    let map name = 
        if inputFormats.ContainsKey(name) then
            inputFormats.[name]
        else
            DXGI.Format.R32G32B32_Float

//=================================================================================================
module InputElements =
    /// By sticking to some conventions we can determine the shader input elements
    /// from the vertex type.
    /// The input elements offsets are calculated by matching the names in the vertex
    /// buffer element type, and calculating the offset by reflection.
    /// This allows us to use different vertex shaders at runtime on geometry which vertex 
    /// buffer layout is determined at import time.
    /// Note that this implies that vertex shader input element names have to match the
    /// names in the vertex <see cref=" Vertex "> vertex </see> elements.
    let map(vertexType:Type) (vertexShaderInputType:Type) =
        let vertexProperties = vertexType.GetProperties()
        let shaderProperties = vertexShaderInputType.GetProperties()
        let toInputElement i (semantic, name) =
            /// Semantic names like TEXCOORD cannot have a numeric suffix when used
            /// with an InputElement. Instead, the suffix is stored as the semantic index.
            let semanticName, semanticIndex = 
                let numbers, letters = semantic
                                       |> Seq.toList
                                       |> List.partition Char.IsDigit
                String(letters |> List.toArray),
                match numbers with
                                    | [x:_] -> int(Char.GetNumericValue(x))
                                    | _ -> 0
            let alignedByteOffset =
                let offset o (p:PropertyInfo) =
                    o + Marshal.SizeOf(p.PropertyType)
                vertexProperties
                |> Seq.takeWhile(fun p -> not(p.Name = name))
                |> Seq.fold offset 0                         
            InputElement(SemanticName = semanticName,
                         SemanticIndex = semanticIndex,
                         Format = InputFormats.map name,
                         AlignedByteOffset = alignedByteOffset,
                         Classification = InputClassification.PerVertexData)
        let propertyNames = shaderProperties
                            |> Array.toList
                            |> List.map (fun p -> p.Name)
        let semantics = Semantics.map propertyNames 
        propertyNames
        |> List.zip semantics
        |> Seq.mapi toInputElement




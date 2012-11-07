namespace SharpShaders

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Reflection
open System.Runtime.Serialization
open PostSharp.Aspects
open PostSharp.Extensibility
open PostSharp.Reflection

    [<MulticastAttributeUsage( MulticastTargets.Struct, Inheritance = MulticastInheritance.Strict )>]
    [<Serializable>]
    type ConstantPackingAttribute() =
        inherit TypeLevelAspect()
        interface IAspectProvider with
        // This method is called at build time and will inject our attributes.
            member m.ProvideAspects(targetElement) =
                let targetType = targetElement :?> Type
                
                let introduceExplicitLayoutAspect(size:int) =
                    let constructorInfo = typeof<StructLayoutAttribute>.GetConstructor( [|typeof<LayoutKind>|] ) 
                    let objectConstruction = new ObjectConstruction(constructorInfo, LayoutKind.Explicit)
                    objectConstruction.NamedArguments.Add("Size", size)
                    new CustomAttributeIntroductionAspect(objectConstruction)

                let introduceFieldOffsetAspect(offset:int) =
                    let constructorInfo = typeof<FieldOffsetAttribute>.GetConstructor( [|typeof<int>|] )
                    new CustomAttributeIntroductionAspect( 
                        new ObjectConstruction(constructorInfo, offset) )

                let introduceCompilationMappingAspect(offset:int) =
                    let constructorInfo = typeof<CompilationMappingAttribute>.GetConstructor( [|typeof<SourceConstructFlags>; typeof<int>|] )
                    new CustomAttributeIntroductionAspect( 
                        new ObjectConstruction(constructorInfo, SourceConstructFlags.Field, offset) )

                let align boundary n = 
                    if n % boundary = 0 then
                        n
                    else
                        n + boundary - n % boundary

                let enforceBoundary (offset, aspects) (field:FieldInfo) =
                    let fieldSize = Marshal.SizeOf(field.FieldType)
                    let alignedOffset = 
                        let next16ByteBoundary = align 16 offset
                        let nextOffset = offset + fieldSize 
                        if nextOffset > next16ByteBoundary then
                            // It is only acceptable to cross a 16 byte boundary if the field size is bigger
                            // than 16 bytes and the field starts on a 16 byte boundary.
                            next16ByteBoundary
                        else
                            // align on 4 byte boundary
                            align 4 offset
                    let nextOffset = alignedOffset + fieldSize
                    let fieldAspect = AspectInstance( field, introduceFieldOffsetAspect alignedOffset)::aspects
                    nextOffset, fieldAspect

                let fields = targetType.GetFields(BindingFlags.DeclaredOnly ||| BindingFlags.Instance ||| BindingFlags.NonPublic)
                // Add a FieldOffset attribute to every field and accumulate the total size on the way.
                let size, fieldAspects = fields
                                         |> Seq.fold enforceBoundary (0,[])

                seq {
                    // Add the ExplicitLayout attribute to the type.
                    let paddedSize = align 16 size
                    yield new AspectInstance( targetType, introduceExplicitLayoutAspect paddedSize )

                    yield! List.rev fieldAspects
                
                    // Add a CompilationMapping attribute to every relevant property.
                    yield! targetType.GetProperties( BindingFlags.Public ||| BindingFlags.DeclaredOnly ||| BindingFlags.Instance )
                           |> Seq.mapi (fun i prop -> new AspectInstance( prop, introduceCompilationMappingAspect i))
                }

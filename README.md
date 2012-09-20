# Sharp Shaders #

**SharpShaders** is a library that allows you to write GPU shader code in F#. 

## F# Shaders
Here is what a basic diffuse shader might look like written in F#. (Data structure definitions are ommitted for brevity)

<code>
    type Shader(  
 				scene:SceneConstants,  
                obj:ObjectConstants,  
                mat:MaterialConstants) =  

        [< VertexShader >]  
        member m.vertex(input:VSInput) =  
            PSInput(input.Position * obj.WorldViewProjection,  
                    input.Normal * float3x3(obj.World))    

        [< PixelShader >]
        member m.pixel(input:PSInput) =
            let color = 
                input.Normal 
                |> normalize
                |> dot -scene.LightDirection
                |> mul mat.Diffuse
				|> saturate
            float4(color, 1.0f)

</code>
This would translate to:
<code>


PSInput vertex(VSInput input)
{

	PSInput o;
    o.PositionHS = mul(input.Position,WorldViewProjection);
	o.Normal = mul(input.Normal,(float3x3)(World));
    return o;
};

float4 pixel(PSInput input) : SV_TARGET
{

	float3 color = saturate(mul(Diffuse,dot(-(LightDirection),normalize(input.Normal))));  
	return float4(color,1);
};

</code>
## Why on earth?
There are a couple of reasons why it would be desirable to write shader code in F#:

- [Targetting multiple platforms](#multiplatform)
- [Syntactic sugar](#sugar)
- [Unit testing](#testing)

<h4 id="multiplatform">Targeting multiple platforms</h4>
In game programming, we seldom have the luxury of developing for a single platform. We need to write code that will run on anything from an iPhone to a PC. Some cross platform game engines, allow you to write shader code in a proprietary language which gets translated to either HLSL for DirectX based platforms, or GLSL for OpenGL based platforms. There are good reasons for creating a language from scratch, but it certainly is quite an endeavor. You need to create a lexer and a parser to translate the text in to meaningful expressions. Only then can you start translating these expressions into other languages such as HLSL or GLSL. Thanks to a language feature called *Quotations*, translating F# to another language turns out to be much simpler than one might expect.

<h4 id="suga">Syntactic sugar</h4>
*F# is a succinct, expressive and efficient functional and object-oriented language for .NET which helps you write simple code to solve complex problems - Microsoft Research*

F# allows you express computations in a very natural, declaritive way without polluting the code with superfluous syntax. Take for example a typical shader computation for calculating the output color. In English one might say:  
>The output color is the *normalized* input normal *dot* the negative light direction, *multiplied* with the diffuse material color and *saturated* to keep the values within permitted bounds. 

Written as a function in HLSL, we have to explicitly declare the input and output types, add brackets and semicolons to help the compiler understand what we want to do. Worse though, we have to write the logic almost completely in reverse, which makes the code harder to understand:  

<code>
float4 color(float3 normal, float3 lightDirection, float4 materialDiffuse)
{

	return saturate(
			mul(materialDiffuse.rgb, 
				dot(-lightDirection, 
					normalize(normal)));
}
</code>  

In F# we only have to declare types where the compiler is not able to infer it due to ambiguity. Furthermore, we can preserve the logical order by using the pipelining operator:
<code>

    let color normal lightDirection (materialDiffuse:float4) =  
            normal   
            |> normalize  
            |> dot -lightDirection
            |> mul materialDiffuse.rgb
            |> saturate
</code> 

Of course since F# is integrated into Visual Studio, it also has several other benefits such as  

- Syntax highlighting
- Code completion
- Jump to method/struct declaration
- etc
 
<h4 id="testing">Unit Testing</h4>
If you are obsessive compulsive about things like unit testing and code coverage, then shader code will probably be a bit of a thorn in your side. It would be extremely hard to unit test shader code that is executed on the GPU. While certainly possible, the amount of effort involved in pulling this off would probably outway the benefits. If the shaders however are written in F#, which can be executed on the CPU, this becomes trivial. As long as we have faithful implementations of standard shader operations such as *dot*, *cross*, *saturate*, we can run our test code on the CPU and have reasonable confidence that our shaders are behaving according to our expectations.

	
## How
#### Expression Trees
In F#, a unique language feature called *quotations*, allows you to retrieve the expression tree for any given piece of F# code. An expression tree is a description of the desired computation, rather than the computation itself. This is very useful for running queries (written in F#) on a remote database, where it will be translated to TSQL. We can use this same mechanism to easily translate our desired computations into something that can be executed on a GPU.

From the statements:
> let pos = Vector4(1.0f,0.0f,0.0f,1.0f)<br>
let worldMatrix = Matrix.identity<br>
 let expr = <@pos * worldMatrix@>

we get the following expression:

> val expr : Quotations.Expr< Vector4 > =
  Call (None, op_Multiply, [PropertyGet (None, pos, []), PropertyGet (None, worldMatrix, [])])

Note that any code enclosed by the <@ ... @> symbols are regarded as a quotation. From the type information we can see that the above expression is a function call to a method *op_Multiply*, which takes two parameters, *pos* and *worldMatrix*. These two variables are of type PropertyInfo which contains information about the input parameters such as their names and types.
To translate this small snippet to HLSL, we just need to map the *op_Multiply* function call to the *mul* function call. For a great introduction in using quotations, visit Kurt Schelfthout's blog, [Forty Six and Two](http://fortysix-and-two.blogspot.ca/2009/06/traversing-and-transforming-f.html)

#### Pattern Matching
F# is not the only language that is able to be easily converted to an expression tree. C# Expressions for example offer similiar functionality. However, F#'s powerful pattern matching capibilities makes it much easier for us to evaluate these recursive data structures.

<code> match(expr) with<br>
| Call(_, opMultiply, param1, param2) -> sprintf "mul(%s,%s)" param1.Name param2.Name<br>
| _ -> failwith "Unsupported expression..."
</code>

This example is of course a bit over simplified. In reality, the parameters to the mul operator could themselves be expressions. Fortunately F# pattern matching makes parsing recursive data structures a breeze and one can whip up a basic translator in less than 200 lines of code. A basic implementation can be found in the [SharpShaders project on GitHub](http://example.com/ "Title") .


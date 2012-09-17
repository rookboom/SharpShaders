# Sharp Shaders #

**SharpShaders** is a library that allows you to write GPU shader code in F#. 

There are a couple of reasons why it would be desirable to write shader code in F#.

- Target either HLSL or GLSL or Cg depending on the platform
- Run shader code on the CPU for unit testing
- Language features such as type inference and the pipelining operator
- Gains all the benefits of a language that is integrated in to Visual Studio
	- Syntax highlighting
    - Jump to method/struct declaration
	
## Target multiple platforms ##

In game programming, we seldom have the luxury of developing for a single platform. We need to write code that will run on anything from an iPhone to a PC. Game Engines such as Unity3D allows you to write shader code in a propriatary language which gets translated to either HLSL for DirectX based platforms, or GLSL for OpenGL based platforms.

Creating an language from scratch is quite the endeavor. You need to create a lexer and a parser to translate the text in to meaningful expressions. Only then can you start translating these expressions into other languages such as HLSL or GLSL. It turns out that translating F# to another language is much simpler than one might expect. A unique language feature called *quotations* allows you to retrieve the expression tree for any given piece of F# code. An expression tree forms a description of the desired computation. 

From the statement:
> let expr = <@pos * worldMatrix@>
.
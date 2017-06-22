---
title: "Field guide to category theory, episode II"
subtitle: "In glorious Technicolor, with both math and Haskell examples."
blurb: "In glorious Technicolor, with both math and Haskell examples."
date: 2017-06-22
tags: [category-theory]
categories: [math.CT]
---

<div style="display: none;">
\\[
\\usepackage{amsmath,amssymb,amsfonts}
\\newcommand{\\id}[1]{{\\rm id} _ {#1}}
\\newcommand{\\ob}[1]{{\\rm Ob}\\,#1}
\\newcommand{\\cmor}[3]{{\\rm Mor} _ {#1}(#2, #3)}
\\newcommand{\\cmoc}[2]{{\\rm Mor} _ {\\sf C}(#1, #2)}
\\newcommand{\\mor}[3]{{#1}(#2, #3)}
\\newcommand{\\moc}[2]{{\\sf C}(#1, #2)}
\\newcommand{\\trr}{\\triangleright}
\\newcommand{\\ovra}[1]{\\overset{#1}{\\longrightarrow}}
\\newcommand{\\ovla}[1]{\\overset{#1}{\\longleftarrow}}
\\]
</div>

Fix a category \\(\\rc\\) and objects \\(X, Y, Z \\in\\rc\\).

## Endomorphisms

An *endomorphism* of \\(X\\) is a morphism \\(X \\to X\\). The identity \\(\\id X\\) is always an endomorphism for any \\(X\\), but it is usually not the only one. The collection of endomorphisms of \\(X\\) is denoted \\(\\eend_{\\rc} X\\), and forms a subset of \\(\\cmoc X X\\). Here is a stupid endomorphism of the object `Int` of \\({\\sf Hask}\\):

```haskell
lol :: Int -> Int
lol _ = 0
```

Given morphisms \\(f : B \\to E\\) and \\(g : E \\to B\\) such that

\\[ \\begin{align}
f \\trr g &= \\id B \\\\
\\end{align}\\]

we say \\(f\\) is a *section* of \\(g\\), and \\(g\\) is a *retraction* of \\(f\\).


A morphism \\(f : X \\to Y\\) is an *isomorphism* if it is *invertible*: that is, if there exists \\(g : Y \\to X\\) such that

\\[ \\begin{align}
f \\trr g &= \\id X \\\\
g \\trr f &= \\id Y
\\end{align}\\]

<blockquote>
<span style="font-variant:small-caps">Exercise.</span><br>
Show that inverses are unique.<br>(Given \\(f\\), show that \\(g\\) is the only morphism that satisfies the properties above.)
</blockquote>

An invertible endomorphism is called an *automorphism*. The set of automorphisms of \\(X\\) is denoted \\(\\aut X\\). This is a subset of \\(\\eend X\\), but that is only part of the puzzle. First, notice that, by definition, every element of \\(\\aut X\\) has \\(\\id X\\) as an inverse for composition. In addition, one can verify that

<blockquote>
<span style="font-variant:small-caps">Exercise.</span><br>
Given two automorphisms \\(\\alpha\\) and \\(\\beta\\) of \\(X\\), \\(\\alpha\\beta\\) is also an automorphism of \\(X\\).
</blockquote>

These two properties, together with the associativity of composition, imply that \\(\\aut X\\) is a *group*. If you have not met groups before, automorphism groups are as good a first example as any!

## Monos and epis

A morphism \\(f : X\\to Y\\) is a *monomorphism* if, for any other morphisms \\( g, h : Z \\to X \\), 

\\[ g \\trr f = h \\trr f \\implies g = h \\]

The dual notion is that of *epimorphism*, which arises when given morphisms \\( g, h : Y \\to Z \\),

\\[ f \\trr g = f \\trr h \\implies g = h \\]

Alternatively, one might say that \\(- \\trr f \\) (*postcomposition with \\(f\\)*) 
is injective in the case of a monomorphism. Similarly, \\( f \\trr - \\) 
(*precomposition with \\(f\\)*) is injective in the case of an epimorphism.

In \\(\\set\\), monomorphisms are precisely the injective functions, those for which

\\[ f(x) = f(y) \\implies x = y \\]

and epimorphisms are the surjective functions: those \\( f : X \\to Y \\) satisfying

\\[ \\text{for all } y \\in Y, \\exists x \\in X \\text{ with } f(x) = y \\]

I generally think of surjective functions as "covering the target completely".

## Subobjects and quotients

Two monomorphisms \\(B \\overset{f}{\\longrightarrow} X\\) and \\(A \\overset{g}{\\longrightarrow} X\\) are said to be
isomorphic if there exists an isomorphism \\( A \\overset{i}{\\longrightarrow} B \\) such that

\\[ i \\trr f = g \\]

Similarly, two epimorphisms \\(X \\ovra{f} B\\) and \\(X \\ovra g A\\) are said to be
isomorphic if there exists an isomorphism \\( B \\ovra i A \\) such that

\\[ f \\trr i = g \\]

An isomorphism class of monomorphisms with target \\(X\\) is called a subobject of \\(X\\).

An isomorphism class of epimorphisms with source \\(X\\) is called a quotient of \\(X\\).

```latex
\begin{xy}
\xymatrix {
  A \ar[r]^f \ar[d] _ {g \circ f} & B \ar[d]^g \\
  C \ar[r] _ f                    & D
}
\end{xy}
```

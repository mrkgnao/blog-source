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
\\]
</div>

## Kinds of morphisms

Fix a category \\(\\rc\\) and objects \\(X, Y, Z \\in\\rc\\).

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

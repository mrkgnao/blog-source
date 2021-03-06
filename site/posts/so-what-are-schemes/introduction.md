---
layout: post
title: Ideals-as-points, and sheaves
blurb: I cover affine schemes, the Zariski topology, and why these things make sense to study.
permalink: schemes-i
date: 2016-12-14
tags: [schemes, series, sheaves]
---

\\[ 
  \\DeclareMathOperator{Mod}{mod} 
  \\DeclareMathOperator{Frac}{Frac} 
  \\DeclareMathOperator{Spec}{Spec} 
  \\DeclareMathOperator{open}{Open} 
  \\newcommand{id}{{\\mathrm {id}}}
  \\newcommand{res}[2]{{\\mathrm{res}}_#1^#2}
\\]

In this post, which is the first one in my [schemes sequence](/schemes/), we look at why schemes matter, and then go on to define the sets underlying affine schemes, which are the "building blocks" that schemes are made out of. We then talk about sheaves, which are a powerful way to keep track of functions that are defined on parts of a geometric space and use "local knowledge" to prove global things and vice versa.

## Prerequisites

### Topologies on sets

A topology on a set \\(S\\), if you've never met one before, is a collection of "open" sets \\(U _ i\\) of the set \\(S\\), forming a subset \\(T\\subset P(S)\\), where \\(P(S)\\) is the power set of the set \\(S\\), satisfying the following axioms:

* The empty subset and \\(S\\) itself are both open.
* The union of a possibly infinite collection of open sets is itself open.
* The intersection of a finite collection of open sets is itself open.

A closed set is one whose complement is open. We can actually also define a topology using closed sets, in which case we have "dual" axioms:

* The empty subset and \\(S\\) itself are both closed.
* The intersection of a possibly infinite collection of closed sets is itself closed.
* The union of a finite collection of closed sets is itself closed.

These definitions are completely equivalent.

One good example to keep in mind is the classical/Euclidean[^r2top] topology on \\(\\mathbf R^2\\), where open disks (like \\(x^2 + y^2 < 4\\)) are open sets (definitely not the *only* ones!) and closed disks (e.g. \\((x-2)^2 + y^2 \\leq 5\\)) are examples of closed sets. You can get all the other open sets from these by taking unions, intersections, and complements, as long as you follow the axioms above.

## Some philosophy

The main objects of study in "modern" algebraic geometry are *schemes* -- a certain kind of "geometric space" that encompasses both geometric questions (such as questions about lines on surfaces) and number-theoretic or arithmetic problems. This all works out because schemes are built from rings, so we can cast geometric problems in terms of rings of functions on geometric spaces -- and algebraic number theory has been a thing for a very long time.

This is a major idea behind algebraic geometry: sometimes it pays to look at some geometric thingy by considering the functions defined on that space. For instance, given a topological space \\(T\\), you can ask

"What continuous real-valued functions

\\[f:T\\to\{\\mathbf R}\\]

can I define on the space, satisfying *xyz* (where *xyz* is some property)?"

This question, by allowing you an indirect look at the space, often reveals very interesting information about the space.[^proj] You can do similar things for smooth or complex manifolds.

This "indirect viewpoint" is the primary one adopted in algebraic geometry.

<div class="bd-callout bd-callout-info"><h4>Slogan</h4><p>Instead of looking at a space, look at the (rings) of functions defined on its subsets.</p></div>

Here is some motivation for what schemes are like, using some words we haven't defined here yet. You may have heard of differentiable or smooth manifolds: they are spaces that locally "look like" \\(\{\\mathbf R}^n\\). For instance, a sphere looks like \\(\{\\mathbf R}^2\\) if you "zoom in" enough. And we understand \\(\{\\mathbf R}^2\\) just fine, and \\(\{\\mathbf R}^n\\) in general -- this is just multivariable calculus. The idea here is that we can work with complicated spaces as long as, "locally", they look like things we understand.

<blockquote><span style="font-variant: small-caps;">Cop-out definition</span><p>A scheme is a "locally ringed space" that is locally isomorphic to an *affine scheme*.</p></blockquote>

That was a lot of words we haven't defined, but it seems like understanding what affine schemes are might be the first step toward grokking schemes.

<section>
## Affine schemes: the underlying set

This section assumes you know some ring theory, at least enough to know what ideals are. See [this post](/ideals-and-prime-ideals/) for all the background you'll need.

Consider a ring \\(R\\), and think about its prime ideals. You can mentally set \\(R = \{\\mathbf Z}\\) or \\(\{\\mathbf C}[x]\\) if you wish.

<div class="bd-callout bd-callout-info"><h4>Definition</h4><p>The prime ideals of \\(R\\) form a set called \\(\\Spec R\\).</p></div>

We will try to think of the prime ideals as "points" of a "space" \\(\\Spec R\\).
Following notation in, say, Eisenbud-Harris[^geom-schemes], we will write \\([{\\mathfrak p}]\\) when we are thinking of a prime ideal as a point, although we will soon outgrow the need for this. Now, the elements of \\(R\\) are functions on this space!

"Wait, what?"

Yes. Let \\(f\\in R\\) (look at how our notation is evolving!) be a "regular function" on \\(\\Spec R\\). The "value" of the regular function \\(f\\) at \\(x = [{\\mathfrak p}]\\) is defined as the image of \\(f\\) in \\(R/{\\mathfrak p}\\)<!-- [^frac] -->.

Two examples, and all will be clear.

Consider \\(7\\in\{\\mathbf Z}\\). What is its value at the point \\([(2)]\\) of \\(\\Spec {\\mathbf Z}\\)? It's \\(7\\Mod 2\\), or, equivalently, the equivalence class \\([7]\\in \{\\mathbf Z}/2\{\\mathbf Z}\\)<!-- [^reddit-fix-2] -->.

The value of the function

\\[x^2-5x+7\\]

at \\([(x-4)]\\in{\\mathbf C}[z]\\) is just \\(4^2-5\\cdot4+7\\). Alternatively, it is the function \\(f(x)=\\) "\\(x^2 - 5x+7\\) evaluated modulo \\((x-4)\\)", which are both the same thing because of the division algorithm.

## Put some functions on it: the structure sheaf<!-- [^put-a-ring-on-it] -->

Okay, now we have some geometry. Let's look at some functions on these affine schemes. The motivating idea is this: you can't have any functions defined all over \\(\\Spec R\\) with anything in the denominator, because they would fail to be defined at the points in the denominator. This is just like how the function

\\[f(z) = \\frac{z^2-z+1}{z-3}\\]

has domain \\(\\mathbf C\\setminus\\{3\\}\\)[^reddit-fix-2].

### A first step: motivating presheaves

It is then that you might have the clever idea of saying, "Why not consider these partially-defined functions systematically?" Of course, *systematically* isn't very specific, but thinking about what our functions should be like gradually gives us some natural properties we can demand.

Vaguely, we want to attach some data to every open set \\(U\\) of our topological space, where the "data" can be a ring of functions, or a vector space (say of functions defined on \\(U\\) satisfying some differential equation), or a group, or something similar. There are, of course, many different ways to pick "rules" saying what to attach to every open set.

Given such a rule \\(\{\\mathcal F}\\), for every open set \\(U\\) in our space, we want some algebraic object \\(\{\\mathcal F}(U)\\). We call these the *sections* of \\(\{\\mathcal F}\\) *over \\(U\\)*.

If you need an example to hold on to, think of the "rule" that eats an open set \\(U\\subset \{\\mathbf R}^2\\), say, and spits out the set (actually a ring) of differentiable functions

\\[C^1(U) := \\{f:U\\to\{\\mathbf R}:f \\text{ differentiable everywhere on } U\\}\\]

What properties, intuitively, would you want these assigments of ... things to open sets to satisfy? Well, I'll now do the thing where I magically wave my hands and exhibit a list. I think, however, that they will appear well-motivated. 

</section>
<section>
## Presheaves

Any rule \\(\{\\mathcal F}\\) satisfying the following properties is what is called a *presheaf*.

First, you should be able to restrict functions from a bigger open set to a smaller one. This corresponds to a map

\\[\\res U V: \{\\mathcal F}(U)\\to\{\\mathcal F}(V)\\]

if \\(V\\subset U\\) is an open subset. This is a map in the appropriate category -- i.e. a ring homomorphism if the \\(\{\\mathcal F}(U)\\)'s are rings, a group homomorphism if they are groups, a linear map if we're doing vector spaces, and so on.

This restriction should be "sane": for one , the "identity" restrictions, that is, \\(\\res U U\\), should be the identity maps. That is,

\\[\\text{for all }U\\subset X, \\res U U =\\id _ U.\\]

<!-- This is ... just ... it could not be otherwise.[^reaction] -->

Sanity also requires that if we have subsets \\(W\\hookrightarrow V\\hookrightarrow U\\), then the two evident ways to restrict a function on \\(U\\) to \\(W\\) -- restrict to \\(V\\) and then to \\(W\\), or to \\(W\\) at once -- should be the same thing, i.e.

\\[\\res V W\\circ\\res U V = \\res U W.\\]

If you've nodded your head at each of these conditions, congratulations, you both know and intuitively understand how to define a presheaf!

If \\({\\mathcal F}(U)\\) is a ring, we say that \\({\\mathcal F}\\) is a *presheaf of rings*. It shouldn't be too hard to guess what sheaves of sets, groups, rings, and so on are.

</section>

<section>
## A more abstract perspective on presheaves

(This requires a tiny bit of category theory. I will probably write that up sometime soon.)

Consider the category of open sets of \\(X = \\Spec R\\). This is the category \\(\\open X\\) whose objects are the open sets of \\(R\\), and where the only morphisms are the inclusions of smaller open sets into bigger ones. That is, for every inclusion \\(U\\subset V\\), we have a morphism

\\[U\\to V\\]

in the category \\(\\open X\\).

<div class="bd-callout bd-callout-info"><h4>Alternative definition</h4><p>For \\(\{\\mathbf C}C\\) some category (say the category of rings, \\(\{\\mathbf {Ring}}\\)), a *\\(\{\\mathbf C}\\)-valued presheaf* is a functor</p>

\\[\{\\mathcal F}: \\opp{\\open X}\\to\{\\mathbf C},\\]

that is, a contravariant functor from \\(\\open X\\) to \\(\{\\mathbf C}\\).
</div>

For instance, a \\(\{\\mathbf Ring}\\)-valued presheaf is just what we have called a presheaf of rings above. This definition is exactly equivalent to the one we made previously, since:

* the condition that a functor take identity morphisms to identities means that the identity morphisms in \\(\\open X\\) (inclusions of the form \\(U\\hookrightarrow U\\) in \\(\\open X\\)) are sent to identity morphisms \\(\\res U U = \\id _ U\\) in \\(\{\\mathbf C}\\).
* the condition that restrictions from a big open set to a medium and finally to a small open set make sense however it is done corresponds to the fact that functors respect composition of morphisms. To every diagram of the form

\\[W\\overset j\\hookrightarrow V\\overset i\\hookrightarrow U\\]

in \\(\\open X\\), the presheaf \\(\{\\mathcal F}\\) associates a diagram

\\[\{\\mathcal F}(U) \\overset{\\res U V}\\hookrightarrow \{\\mathcal F}(V) \\overset{\\res V W}\\hookrightarrow W\\]

in \\(\{\\mathbf C}\\), which commutes. <!-- (TODO: these should be commutative triangles!) -->

There is a way to extend this to the definition of a sheaf, and more[^sites].

</section>

<section>
## Sheaves

A sheaf is a special kind of presheaf. Before we get into exactly what a sheaf is, here's the

<div class="bd-callout bd-callout-info"><h4>Big Idea™</h4><p>Presheaves of rings or groups are complicated things to work with.
What if we could just work with rings or the groups?
</p></div>

This does not work for presheaves, in general. This is because (*philosophy/buzzword alert*) presheaves do not capture "local" data. We need some extra conditions on our presheaf to make our lives easier, and that is exactly what the notion of a sheaf captures.

<div class="bd-callout bd-callout-info"><h4>Aside</h4><p>For instance -- although we haven't defined it yet, it shouldn't be too hard to believe that there is a notion of a map of sheaves -- there are examples of maps of (pre)sheaves that are surjective on the sections, but not surjective themselves.</p></div>

Working with sheaves instead of presheaves fixes this, in some sense, because the sheaf axiom that we will state next allows us to capture more data.
For instance, there is a notion of the *stalk* of a presheaf at a point, but it only really behaves well for sheaves. The stalk of a sheaf \\(\{\\mathcal F}\\) of rings at a point \\(p\\) is a ring \\(\{\\mathcal F} _ p\\), and so on -- and there are many properties that are true for a sheaf iff they are true for the stalks, so we can prove things "stalk-by-stalk"[^surj-stalk].

<div class="bd-callout bd-callout-info"><h4>Definition</h4><p>A sheaf is a presheaf where, given compatible sections \\(f _ i\\in\{\\mathcal F}(U _ i)\\) over all open sets \\(U _ i\\) in an open cover[^open-cover] of \\(U\\), there exists a unique section \\(f\\in\{\\mathcal F}(U)\\) such that

\\[f| _ {U _ i} = f _ i\\]

for all \\(U _ i\\). 
</p></div>

Here "compatible" means "agreeing on overlaps", i.e. writing \\(U _ {ij}\\) for \\(U _ i \\cap U _ j\\),

\\[{f _ i}| _ {U _ {ij}} = {f _ j}| _ {U _ {ij}}.\\]

This allows us to "patch" sections together. This is a key property: as we will notice in future, sheaves may be harder to define, but they are much easier to work with and prove things about.

We end with a super-elementary example of how "sheafy thinking" works.

<div class="bd-callout bd-callout-info"><h4>Proposition</h4><p>If there is a *global section* \\(f\\) of \\(\{\\mathcal F}\\) (i.e. \\(f\\in\{\\mathcal F}(X)\\) where \\(X\\) is the whole space) which is \\(0\\) when restricted to every open subset in an open cover[^open-cover] of \\(X\\), it is zero everywhere.</p></div>

Notice how "obvious" this sounds! This is in fact true, but not for general presheaves.

**Proof**

* The everywhere-zero section \\(z\\in\{\\mathcal F}(X)\\) (I could've used \\(0\\), but overloading is unhelpful at times) is zero on every open subset of \\(X\\).
* The \\(f\\| _ {U _ i}\\) are compatible sections (since \\(0 = 0\\)), so they patch together to give some global section. By the uniqueness condition in the sheaf axiom, this global section must be equal to \\(z\\).
* Hence \\(z = f\\) as elements of \\(\{\\mathcal F}(X)\\).

In reality, there are a bunch of equivalent ways to think about the sheaf axiom, and they all sort of merge into one idea before long. Here are some:

* Two sections over \\(U\\) that agree on every \\(U _ i\\) in an open cover of \\(U\\) are in fact the same.
* The proposition from above is equivalent to the previous condition, since the uniqueness of the zero section can be used to show that \\(f-g\\) being globally zero makes \\(f = g\\).

In the next instalment, we will construct a topological space out of our set \\(\\Spec R\\), and construct a sheaf, the *structure sheaf*, making \\(\\Spec R\\) into a ringed space.

There's a [Reddit thread](https://www.reddit.com/r/math/comments/5id5ve/so _ what _ are _ schemes _ first _ part _ up _ more _ coming _ soon/) if you want to discuss anything!
</section>

## Bibliography

* Ravi Vakil, Foundations of Algebraic Geometry<!-- [^vakil] -->
* Eisenbud, Harris, The Geometry of Schemes<!-- [^geom-schemes] -->

<!-- [^put-a-ring-on-it]: I debated whether "Put some rings on it" would be going too far and decided against it in the end. After all, not everyone is as cool as the Mathcamp mentor who apparently did a parody of [this](https://www.youtube.com/watch?v=4m1EFMoRFvY) that goes just as you would expect it to. -->

<!-- [^vakil]: Here is a [link](http://math.stanford.edu/~vakil/216blog/FOAGdec2915public.pdf) to the latest-as-of-writing December 2015 edition. I've heard a lot of people say that a physical version is coming out sometime soon. -->

<!-- [^geom-schemes]: Eisenbud-Harris, The Geometry of Schemes. -->

<!-- [^vakil-1]: [FOAG](#vakil). -->

<!-- [^sites]: In fact, this "functorial" definition is the one used in defining, say, étale cohomology or sheaves on other "sites". (While I might feel very grown-up right now, but I think it's imperative to note that I don't know much more about sites and étale-stuff than what I just wrote down.) -->

<!-- [^frac]: Or sometimes its image under the composition of the canonical morphisms \\(R\\to R/{\\mathfrak p} \\to \\Frac(R/{\\mathfrak p})\\). -->

<!-- [^surj-stalk]: For instance, surjectivity on all the stalks implies the surjectivity of a map of sheaves. -->

<!-- [^open-cover]: A bunch of open sets \\(U _ i\\) cover an arbitrary subset \\(U\\subset X\\) if \\(U = \\bigcup U _ i\\). The \\(U _ i\\) are said to form an *open cover* of \\(U\\). -->

<!-- [^reaction]: (*SMBC agitated-professor face*) "I mean, how can you not see it?" -->

<!-- [^r2top]: Or "standard", or "usual", or so on. It goes by many such names. -->

<!-- [^proj]: For instance, Liouville's theorem from complex analysis, or even "the only globally defined functions defined on a projective space are the constants". -->

<!-- [^reddit-fix-2]: Fixed thanks to /u/ImJustPassinBy on Reddit. -->

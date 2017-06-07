---
title: Fermat's Christmas theorem
subtitle: When is a prime number a sum of two squares?
blurb: When is a prime number a sum of two squares?
date: 2017-06-07
tags: [number-theory, algebraic-number-theory]
categories: [math.NT]
---



\\[ \\DeclareMathOperator{Mod}{mod} \\]
Does the equation<label for="mn-demo" class="margin-toggle">&#8853;</label>
<input type="checkbox" id="mn-demo" class="margin-toggle"/>
<span class="marginnote">This post is yet another from Ye Olde Days: I found a stray \\(\\TeX\\) file in an old `/home` folder. I wrote it for a friend at Canada/USA Mathcamp.</span></span>


\\[ \\begin{align}p = a ^ 2 + b ^ 2 &&(p \\text{ prime})\\end{align} \\]

have nontrivial solutions with integer \\(a\\) and \\(b\\)?

One start is to look at the equation mod \\(4\\): squares are always either \\(0\\) or 
\\(1\\) modulo \\(4\\), so \\(p\\) can only be \\(1\\) mod \\(4\\).<label for="sn-demo" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-demo" class="margin-toggle"/><span class=sidenote>\\(0\\) and \\(2\\) are, uh, not really possibilities, except in the case \\(2 = 1^2 + 1^2\\).</span> Keeping this in mind, we restrict our attention to primes \\(\\equiv 1\\Mod 4\\).

We notice that a nontrivial "factorization" of \\(p\\) of the form

\\[\\begin{align}p=(a+ib)(a-ib) &&a, b\\in{\\mathbf Q}\\end{align}\\]

gives us an expression of the form we want, if we are willing to expand our
notion of what factorization means beyond its usual meaning in \\({\\mathbf Z}\\).
In fact, the approach that we follow
is to look at how different primes factor or "split" in the ring \\({\\mathbf Z}[i]\\),
which is the ring of numbers of the form

\\[ {\\mathbf Z}[i] = \\{a+ib : a, b\\in{\\mathbf Z}\\} \\]

We will obtain a complete description of how integer primes split in this ring, the
*ring of Gaussian integers*, and thus prove the following:

<blockquote>
<span style="font-variant:small-caps">Theorem (Fermat).</span><br>
A prime number can be expressed as a sum of two integer squares iff it is congruent to \\(1\\) mod \\(4\\).
</blockquote>

## \\({\\mathbf Z}[i]\\) as a lattice

Notice that it can often be worthwhile to think of \\({\\mathbf Z}[i]\\) geometrically, as a
subset of the complex plane corresponding to all the complex numbers with
integer real and complex parts. This gives \\({\\mathbf Z}[i]\\) the structure of a (square)
*lattice*, which has an obvious meaning that we will not work hard to
rigorize for now. 

<!-- Low-effort picture: -->
<!-- \\begin{tikzpicture} -->
<!-- \\begin{scope} -->
<!-- \\clip (-5,-5) rectangle (4cm,4cm); % Clips the picture... -->
<!-- \\draw[style=help lines,dashed] (-14,-14) grid[step=2cm] (14,14); % Draws a grid in the new coordinates. -->
<!-- \\foreach \\x in {-7,-6,...,7}{                           % Two indices running over each -->
<!--     \\foreach \\y in {-7,-6,...,7}{                       % node on the grid we have drawn -->
<!--     \\node[draw,circle,inner sep=2pt,fill] at (2*\\x,2*\\y) {}; % Places a dot at those points -->
<!--     } -->
<!-- } -->
<!-- \\end{scope} -->
<!-- \\end{tikzpicture} -->

<span style="font-variant:small-caps;">Aside</span>. This has very deep applications once we start considering more interesting number fields:
here we are looking at \\({\\mathbf Q}(i)\\) and its *ring of integers* \\({\\mathbf Z}[i]\\), and
\\({\\mathbf Q}(i)\\) is basically the nicest number field in existence. We will see that
arguments using the "geometry of numbers", also called *Minkowski theory*, are very powerful, and will (for instance) enable us to prove the
Dirichlet unit theorem.

## Preliminaries

First, we review a few definitions from ring theory.

<!-- \\begin{defn} -->
  An element \\(r\\) of a ring \\(R\\) is *prime* if, whenever it divides a
  product \\(ab\\), it must divide either \\(a\\) or \\(b\\).
<!-- \\end{defn} -->

<!-- \\begin{defn} -->
  An element \\(r\\in R\\) is *irreducible*<label for="sn-demo" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-demo" class="margin-toggle"/><span class=sidenote>*irred* if I'm lazy</span> if the only way to write it as a product of elements is
  \\[r=u\\cdot r_1\\]
  where \\(u\\) is a unit, and \\(r_1\\) is \\(u_1r\\) for some unit \\(u_1\\).
<!-- \\end{defn} -->

For example, \\(2\\) is not irred in \\({\\mathbf Z}[i]\\), since \\(2={(1+i)}^2\\), but it is an irred
in \\({\\mathbf Z}\\), since the only ways to write it as a product are silly things like
\\((-1)\\cdot (-2)\\) and \\(-2\\) is manifestly a unit times \\(2\\).

A ring \\(R\\) is *Euclidean* if there exists a function \\[\\nu : R\\setminus\\{0\\}\\to{\\mathbf Z} ^ {\\geq 0},\\] called the
*Euclidean valuation*, such that we can perform a "division algorithm" in
the ring using \\(\\nu\\).That is, for any \\(a, b\\in R\\), there exist \\(q, r\\in R\\) such that<label for="sn-demo" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-demo" class="margin-toggle"/><span class=sidenote>Note that the valuation of \\(0\\) is not defined, at least per this definition. However, setting \\(\\nu(0)=0\\) seems to work just fine.</span>


\\[a=bq+r, \\nu(r) < \\nu(b).\\]

For instance, \\({\\mathbf Z}\\) is an Euclidean domain, with valuation \\(\\nu(r) = |r|\\).

A *principal ideal domain*, or *PID* for short, is a ring where
all ideals are generated by a single element (all ideals are *principal*).


A *unique factorization domain* (often *UFD* for short, or sometimes
*factorial* ring or domain) is a ring where elements can be uniquely
factored into irreducible elements. That is, for all \\(r\\in R\\), there exists a
factorization

\\[ r=u\\cdot p_1 ^ {a_1}p_2 ^ {a_2}\\cdots p_k ^ {a_k} \\]

where \\(u\\) is a unit, and the \\(p_i\\) and \\(a_i\\) are unique, up to reordering of
the \\(p_i\\).

Now, we have:

<blockquote>
<span style="font-variant:small-caps">Theorem</span>.<br>
Euclidean domain \\(\\implies\\) PID.
</blockquote>

<span style="font-variant:small-caps">Proof</span>.
Consider an arbitrary ideal \\(I\\) of our ring \\(R\\). Choose a smallest nonzero
element \\(w\\) from \\(I\\) (where "smallest" refers to the valuation being the least).

Any other element of \\(I\\) can be written as

\\[a = wq + r\\]

where \\(\\nu(r) < \\nu(w)\\). But there is no such nonzero element, by the
assumption that \\(w\\) has the smallest valuation of any element of \\(I\\). We
conclude that \\(r=0\\).
Hence \\(w\\) divides every other element of \\(I\\), so \\(I=(w)\\). \\(\\square\\)

<blockquote>
<span style="font-variant:small-caps">Theorem</span>.<br>
PID \\(\\implies\\) UFD.
</blockquote>

<span style="font-variant:small-caps">Proof</span>.
Messy, won't do. (Add reference!)

Here's an easy exercise:

<blockquote>
<span style="font-variant:small-caps">Exercise</span>.<br>
In any integral domain, primes are irreducible.
</blockquote>

Now, we have the very important partial converse:

<blockquote>
<span style="font-variant:small-caps">Fact</span>.<br>
In a UFD, irreducibles are prime.
</blockquote>

This will be helpful shortly. In fact, the only reason why we went to all this
trouble is so that we could state this fact!

## \\({\\mathbf Z}[i]\\) is a UFD

The function \\(\\nu: {\\mathbf Z}[i]\\to{\\mathbf Z}^{\\geq 0}\\) defined by 

\\[ \\nu(a + ib) = a^2 + b^2 = | a + ib |^2 \\]

is an Euclidean valuation on the ring \\({\\mathbf Z}[i]\\).

Given \\(a,b\\in{\\mathbf Z}[i]\\), we need to show that there exist \\(q,r\\in{\\mathbf Z}[i]\\) such that
\\[a=bq+r, |r|^2 < |b|^2.\\] 

This is equivalent to
\\[\\frac ab - q = \\frac r b.\\] 

We need to find a \\(b\\) such that
\\[\\left|\\frac ab - q\\right| = \\left| \\frac rb\\right|< 1.\\]

Why is this possible? Notice that the number \\(\\frac ab\\) lies in some square
of the lattice formed by the Gaussian integers in the complex plane. So there
will always be some lattice point within (at most) half the length of a
diagonal of a lattice square from \\(\\frac ab\\). This suffices, since that length
is \\(\\frac{\\sqrt 2}2 < 1\\).

If \\(p=4n+1\\) is a prime, then the congruence \\[-1\\equiv x^2\\Mod p\\] has a solution: 
indeed, by Wilson's theorem,

\\[
  \\begin{align}
    -1\\equiv (p-1)! &= (1\\cdot2\\cdots{(2n)})\\cdot((2n+1)\\cdot(2n+2)\\cdots{(4n)}) \\\\
                     &\\equiv (1\\cdot2\\cdots{(2n)})\\cdot((-2n)\\cdot(-2n+1)\\cdots{(-1)}) \\\\
                     &= (2n)!\\cdot{(-1)^{2n}(2n)!} \\\\
                     &= {[(2n)!]}^2 \\Mod p
  \\end{align}
\\]

so taking \\(x=(2n)!\\) works.

It now suffices to show that a prime \\(p\\in{\\mathbf Z}\\) does not remain prime in \\({\\mathbf Z}[i]\\) if
\\(p\\equiv 1\\Mod 4\\).
When that is done, we have a nontrivial factorization

\\[ p=\\alpha\\cdot\\beta \\]

and taking norms (i.e. valuations) of both sides gives

\\[ p^2 = (a^2 + b^2) \\cdot\\nu(\\beta) \\]

where we write \\(\\alpha = a+ib\\). Now, since the factorization is nontrivial,
neither \\(\\alpha\\) nor \\(\\beta\\) are units, and hence we have \\( p = a^2 + b^2 \\), and we're done.

But for primes congruent to \\(1\\) modulo \\(4\\), we have our lemma which
states that there exists \\(x\\) such that \\(x^2+1\\equiv 0\\Mod 4\\). So
\\(p|x^2+1 = (x+i)(x-i)\\), but it does not divide either of the factors on the
right (it doesn't divide either of their imaginary parts).
So \\(p\\) is not prime in \\({\\mathbf Z}[i]\\).

Now we use all the algebra we did: since \\({\\mathbf Z}[i]\\) is a UFD, \\(p\\) not prime
implies that \\(p\\) is not irreducible. So \\(p\\) has some factorization

\\[p=\\alpha\\cdot\\beta\\]

in \\({\\mathbf Z}[i]\\), and we're done.

---
title: XyJax examples
date: 2017-07-22
tags: [category-theory, code-examples, diagrams]
---

Here are some example diagrams drawn using the XyJax extension for MathJax, which brings 
xypic compatibility to the latter. My formatting and alignment is somewhat idiosyncratic
and slightly motivated by avoiding syntax highlighting clashes between underscores with
different meanings.

The images may currently be invisible, but I assure you they exist and are *glorious*.

## A commutative square
$$
\begin{xy}
\xymatrix {
  A \ar[r] ^ {f} \ar[d] _ {i} & B \ar[d] ^ {j} \\
  C \ar[r] _ {g}              & D
}
\end{xy}
$$

```latex
\begin{xy}
\xymatrix {
  A \ar[r] ^ {f} \ar[d] _ {i} & B \ar[d] ^ {j} \\
  C \ar[r] _ {g}              & D
}
\end{xy}
```

## Natural transformation

$$
\begin{xy}
\xymatrix {
  L(X) \ar[r] ^ {\theta _ X} \ar[d] _ {L(f)} &
  R(X) \ar[d] ^ {R(f)} \\
  %%%
  L(Y) \ar[r] _ {\theta _ Y} &
  R(Y)
}
\end{xy}
$$
A natural transformation between \\(F\\) and \\(G\\).

```latex
\begin{xy}
\xymatrix {
  L(X) \ar[r] ^ {\theta _ X} \ar[d] _ {L(f)} &
  R(X) \ar[d] ^ {R(f)} \\
  %%%
  L(Y) \ar[r] _ {\theta _ Y} &
  R(Y)
}
\end{xy}
```

## Inverse function

$$
\begin{xy}
\xymatrix @C=4pc {
  x 
  %%% loop left-up, then left-down
  \ar@(lu,ld) [] _ {id} 
  \ar@/^/^{f} [r] & 
  f(x) 
  \ar@/^/^{f^{-1}} [l]
}
\end{xy}
$$

```latex
\begin{xy}
\xymatrix @C=4pc {
  x 
  %%% loop left-up, then left-down
  \ar@(lu,ld) [] _ {id} 
  \ar@/^/^{f} [r] & 
  f(x) 
  \ar@/^/^{f^{-1}} [l]
}
\end{xy}
```

## Fiber products

$$
\begin{xy}
\xymatrix{
U \ar@/ _ /[ddr] _ y \ar@{.>}[dr]|{\langle x,y \rangle} \ar@/^/[drr]^x \\
 & X \times _ Z Y \ar[d]^q \ar[r] _ p & X \ar[d] _ f \\
 & Y \ar[r]^g & Z
}
\end{xy}
$$

```latex
\begin{xy}
\xymatrix{
U \ar@/ _ /[ddr] _ y \ar@{.>}[dr]|{\langle x,y \rangle} \ar@/^/[drr]^x \\
 & X \times _ Z Y \ar[d]^q \ar[r] _ p & X \ar[d] _ f \\
 & Y \ar[r]^g & Z
}
\end{xy}
```

## The snake lemma

$$
\newcommand\Ker{\mathrm{ker}\,}
\newcommand\Coker{\mathrm{coker}\,}
\begin{xy}
\xymatrix {
  0 \ar@[red][r] 
  & {\Ker f} \ar@[red][r] 
  & {\Ker a} \ar@[red][r] \ar[d] 
  & {\Ker b} \ar@[red][r] \ar[d] 
  %%% The big arrow.
  & {\Ker c} \ar@[red]@`{[] + /r10pc/, [dddll] + /l10pc/}[dddll] \ar[d]
\\
  &
  & A _ 1 \ar[r]^f \ar[d]^a 
  & B _ 1 \ar[r]   \ar[d]^b 
  & C _ 1 \ar[r]   \ar[d]^c 
  & 0
\\
  & 0     \ar[r] 
  & A _ 2 \ar[r]      \ar[d] 
  & B _ 2 \ar[r]^g \ar[d] 
  & C _ 2 \ar[d]
\\
  &
  & {\Coker a} \ar@[red][r] 
  & {\Coker b} \ar@[red][r] 
  & {\Coker c} \ar@[red][r] 
  & {\Coker g} \ar@[red][r] 
  & 0
}
\end{xy}
$$

I'm guessing that, in the code below, `[] + /r10pc/` and `[dddll] + /l10pc/` 
are control points of some sort around which the arrow "pivots". 
The first one is essentially `10pc` to the right of the 
current position, and the second one is to the left of the `[dddll]` 
(which is the first \\(\\coker\\)).

```latex
\newcommand\Ker{\mathrm{ker}\,}
\newcommand\Coker{\mathrm{coker}\,}
\begin{xy}
\xymatrix {
  0 \ar@[red][r] 
  & {\Ker f} \ar@[red][r] 
  & {\Ker a} \ar@[red][r] \ar[d] 
  & {\Ker b} \ar@[red][r] \ar[d] 
  %%% The big arrow.
  & {\Ker c} \ar@[red]@`{[] + /r10pc/, [dddll] + /l10pc/}[dddll] \ar[d]
\\
  &
  & A _ 1 \ar[r]^f \ar[d]^a 
  & B _ 1 \ar[r]   \ar[d]^b 
  & C _ 1 \ar[r]   \ar[d]^c 
  & 0
\\
  & 0     \ar[r] 
  & A _ 2 \ar[r]      \ar[d] 
  & B _ 2 \ar[r]^g \ar[d] 
  & C _ 2 \ar[d]
\\
  &
  & {\Coker a} \ar@[red][r] 
  & {\Coker b} \ar@[red][r] 
  & {\Coker c} \ar@[red][r] 
  & {\Coker g} \ar@[red][r] 
  & 0
}
\end{xy}
```

## 3D diagrams

$$
\begin{xy}
\xymatrix@R=1pc{
%%%
\zeta 
\ar@{|->} [dd] 
\ar@{.>} _ \theta [rd] 
\ar@/^/^\psi [rrd] \\ & 
%%%
\xi 
\ar@{|->} [dd] 
\ar _ \phi [r] & 
%%%
\eta 
\ar@{|->} [dd] \\
%%%
P _ {F}\zeta 
\ar _ t [rd] 
\ar@/^/ [rrd]|!{[ru];[rd]}\hole \\ & 
%%%
P _ {F}\xi 
\ar [r] & 
%%%
P _ {F}\eta
}
\end{xy}
$$

```latex
\begin{xy}
\xymatrix@R=1pc{
%%%
\zeta 
\ar@{|->} [dd] 
\ar@{.>} _ \theta [rd] 
\ar@/^/^\psi [rrd] \\ & 
%%%
\xi 
\ar@{|->} [dd] 
\ar _ \phi [r] & 
%%%
\eta 
\ar@{|->} [dd] \\
%%%
P _ {F}\zeta 
\ar _ t [rd] 
\ar@/^/ [rrd]|!{[ru];[rd]}\hole \\ & 
%%%
P _ {F}\xi 
\ar [r] & 
%%%
P _ {F}\eta
}
\end{xy}
```

## Basic composition

$$
\begin{xy}
\xymatrix@R=1pc{
%%%
\zeta 
\ar@{-->} _ \theta [rd] 
\ar@/^/^\psi [rrd] \\ & 
%%%
\xi 
\ar _ \phi [r] & 
%%%
\eta
}
\end{xy}
$$

```latex
\begin{xy}
\xymatrix@R=1pc{
%%%
\zeta 
\ar@{-->} _ \theta [rd] 
\ar@/^/^\psi [rrd] \\ & 
%%%
\xi 
\ar _ \phi [r] & 
%%%
\eta
}
\end{xy}
```

$$
\begin{xy}
\xymatrix{
%%%
\zeta 
\ar _ \theta [r]
\ar@/^1pc/[rr]^ {\theta \,\triangleright\,\phi} &
%%%
\xi 
\ar _ \phi [r] & 
%%%
\eta
}
\end{xy}
$$

```latex
\begin{xy}
\xymatrix{
%%%
\zeta 
\ar _ \theta [r]
\ar@/^1pc/[rr]^ {\theta \,\triangleright\,\phi} &
%%%
\xi 
\ar _ \phi [r] & 
%%%
\eta
}
\end{xy}
```

## Newcommands for reuse

```latex
%%% Commutative squares.
%%% The first four arguments are the corner labels in anticlockwise
%%% order, starting from the top-right.
%%% The next four arguments are the arrow labels between successive 
%%% pairs of the labels given above.
%%%
\newcommand{\xycommsq}[8]{
\begin{xy}
\xymatrix {
  {#1} \ar[r] ^ {#8} \ar[d] _ {#5} &
  {#4} \ar[d] ^ {#7} \\
  %%%
  {#2} \ar[r] _ {#6} &
  {#3}
}
\end{xy}
}
```

```latex
\xycommsq{L(X)}{L(Y)}{R(Y)}{R(X)}{L(f)}{\theta _ Y}{R(f)}{\theta _ X}
```

$$
\newcommand{\xycommsq}[8]{
\begin{xy}
\xymatrix {
  {#1} \ar[r] ^ {#8} \ar[d] _ {#5} &
  {#4} \ar[d] ^ {#7} \\
  %%%
  {#2} \ar[r] _ {#6} &
  {#3}
}
\end{xy}
}
\xycommsq{L(X)}{L(Y)}{R(Y)}{R(X)}{L(f)}{\theta _ Y}{R(f)}{\theta _ X}
$$

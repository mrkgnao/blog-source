---
title: "So what are schemes?"
blurb: The master post for my Schemes sequence.
date: 2016-12-13
tags: [algebraic-geometry, schemes, series]
image: mumford-oda-map.png
image-desc: "David Mumford and Tadao Oda's updated version of Mumford's treasure map, taken from <em>Algebraic Geometry II </em>. <br>From Pieter Belmans's <a href=\"http://pbelmans.ncag.info/blog/atlas/\">blog post</a> on the evolution of this visualization of \\({\\rm Spec}\\; {\\mathbf Z}[x]\\)."
---

In keeping with the *long*-term goal of understanding the proofs of the Weil conjectures, I'm going to start talking about some basic algebraic geometry on this blog.

This is the master post for what I'm calling my *Schemes sequence*.<label for="sn-demo" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-demo" class="margin-toggle"/><span class=sidenote>No, I don't read LessWrong. (This, of course, means nothing more or less than what it appears to mean; I've just found it harder and harder to find time to read over the last few years.)</span>

Note that I'm writing about this stuff *as* I learn about it, primarily to improve my own understanding of the material. Any corrections, however minor, will be accepted graciously (I'm not a native English speaker, so there's that, too!), as will comments from anyone who knows this stuff better than I do.

## Preamble

All through this series, our rings will be commutative and have \\(1\\). If that doesn't make sense, think about \\(\\mathbf Z\\) and you'll be fine (if you read around a lot).

I will try to assume very little algebraic or topological knowledge and will talk about the definitions of, e.g. ideals, as we go. I'll break the background up into separate posts, since I expect that a significant part of my audience is familiar with ring theory and modules.

## Posts

### Part 1: [Ideals-as-points, and sheaves](/so-what-are-schemes/introduction/)

After some motivation for schemes, this is the first part of the definition of affine schemes, in which I outline how prime ideals can sensibly be considered points of a space. I then define sheaves and talk about how I think of them.

<!-- ### Part 2: The topology on an affine scheme -->

<!-- ### Part 3: The structure sheaf of an affine scheme -->
<pre class="code fullwidth">
;; Some code examples in Clojure. This is a comment.

;; applying a function to every item in the collection
(map tufte-css blog-posts)
;;;; if unfamiliar, see http://www.lispcast.com/annotated-map 
</pre>



## Background

### What is a prime ideal?

Ring theory background for Part 1. Gentle introduction to ideals, prime ideals, and maximal ideals. Lots of examples. (Part 1 links to this at the appropriate point, go [read that](/schemes-i/) instead.)

---
date: 2014-07-06 06:55:53
tags: maths, pl, TaPL
title: TaPL Exercise 2.2.6
---

> Suppose we are given a relation $R$ on a set $S$.  Define the relation
> $R^{\prime}$ as follows:
>
> $$
> R^{\prime} = R \cup \left\{ (s, s) | s \in S \right\}
> $$
>
> That is, $R^{\prime}$ contains all the pairs in $R$ plus all pairs of the form
> $(s, s)$.  Show that $R^{\prime}$ is the reflexive closure of $R$.

The reflexive closure is defined as the smallest reflexive relation that
contains $R$.  So I need to prove:

1. That the relation $R^{\prime}$ contains $R$;
2. That it is reflexive;
3. That it is the smallest relation satisfying the above two properties.

The first property is given in the definition of $R^{\prime}$; $R^{\prime}$ is formed by
the union of $R$ and some other relation, so it must contain $R$.

To understand the second property we must consider what is meant by the term
*reflexive*; luckily this is given to us by definition 2.2.1 in the text:

> A binary relation $R$ on a set $S$ is *reflexive* if $R$ relates every element
> of $S$ to itself -- that is, $s R s$ (or $(s, s) \in R$) for all $s \in S$.

So, for $R^{\prime}$ to be reflexive, the following must hold:

$$
\forall s \in S: (s, s) \in R
$$

or in other words, it must contain the relations:

$$
\left\{ (s, s) | s \in S \right\}
$$

which, as fortune would have it, is *also* given in the definition of $R^{\prime}$,
so the second property turns out to be as trivially true as the first.

That leaves only one thing left to prove, which is that $R^{\prime}$ is the smallest
relation satisfying the above two properties.  Well, let's consider a relation
$R^{\prime\prime}$ that satisfies the first two properties but is smaller
than $R^{\prime}$.  In order for $R^{\prime\prime}$ to be smaller
than $R^{\prime}$, there must be some $x$ where $x \in R^{\prime}$
and $x \not \in R^{\prime\prime}$.  But, in order to satisfy the first two
properties, for every $x$ in $R^{\prime\prime}$, either $x \in R$
or $x \in \left\{ (s, s) | s \in S \right\}$.  Since $R^{\prime}$
is the union of these two sets by definition, there can be no $x \not \in
R^{\prime}$ which is in either set, thus there can be no
$R^{\prime\prime}$ smaller than $R^{\prime}$ which is a reflexive
relation containing $R$.

---
title: LCM and HCF as Operations on Sets
date: 2014-11-13
---
Most of us were taught sometime is school that the product of the
Lowest Common Multiple, LCM and the Highest Common Factor, HCF of
two numbers is equal to the product of the numbers. Lately, I've
been thinking about it and tried to generalize this idea to larger number
of numbers. I don't know if it has already been done but it was a nice eureka
moment for me!

Let's start with the case that we are familiar with
$$LCM(a, b) \times HCF(a,b) = a \times b$$
where $a$ and $b$ are positive numbers.

To convince ourselves that the above statement is true for any two positive
integers $a$ and $b$, we can think of the product of the two numbers as
the product of their factors. The two number will have some factors in common
while each of them will have some unique factors. Let's denote the common factors
by $k_1,k_2 \dots k_c$.

$$ \begin{align} 
a &= k_1 \times k_2 \dots \times k_c \times a_1 \times a_2 \dots a_m \\ 
b &= k_1 \times k_2 \dots \times k_c \times b_1 \times b_2 \dots b_n
\end{align} $$

where $a_i$ are factors which are unique to $a$ and $b_j$ are factors unique to
$b$. For the numbers $6$ and $10$, the common factor is $2$.
The factor $3$ is unique to $6$ and $5$ is unique to $10$.
$$ \begin{align} 
6 &= 2 \times 3  \\
10 &= 2 \times 5
\end{align} $$

The highest common factor is actually the product of all the factors common
to $a$ and $b$.
$$ \begin{align}
HCF(a, b) &= k_1 \times k_2 \dots k_{c-1} \times k_c \\
HCF(6, 10) &= 2
\end{align} $$

Try looking at the product of the two numbers $a$ and $b$ with the common factors
separated to the left end.
$$a \times b = (k_1 \times k_2  \dots k_c) \times (a_1 \times a_2 \dots a_m \times
k_1 \times k_2 \dots k_c \times b_1 \times b_2 \dots \times b_n)$$  
= { Replacing the product of common factors by $HCF(a, b)$ }
$$a \times b = HCF(a, b)\times (a_1 \times a_2 \dots a_m \times k_1 \times k_2
\dots k_c \times b_1 \times b_2 \dots b_n)$$

Now let's focus all our concentration on the product in parenthesis on the right
hand side, i.e.
$$a_1 \times a_2 \dots a_m \times k_1 \times k_2 \dots k_c \times b_1 \times b_2 \dots b_n$$

Can you see that this number is a multiple of both $a$ and $b$.
If you collect the $k_i$'s and merge them with $a_i$'s, you get $a$.
Alternatively, if you merge the $k_i$'s with the $b_i$'s, you get $b$.
Since we will be referring to the product a lot, let me bind a shorter name to it.
$$CM = a_1 \times a_2 \dots a_m \times k_1 \times k_2 \dots k_c \times b_1
\times b_2 \dots b_n$$
$CM$ stands for a _common multiple_ of $a$ and $b$. Now we need to show that $CM$
is indeed the _Lowest_ Common Multiple.

The most trivial common multiple that one can come up with for two number is
really the product of the numbers. Now to obtain a lower common multiple, all we
need to do is take out factors which are common to both the numbers. If a factor
$k$ is common to $a$ and $b$, even after taking one such factor out, a copy of
$k$ remains in the product! So the more we can take out factors from the product,
the smaller the common factor we'll obtain. The largest piece that we can take
out is really the $HCF$. That should and would leave us with the $LCM$ in the
product.

So, we have somehow informally convinced ourselves of the fact that
$$ a \times b = HCF(a, b) \times LCM(a,b) $$
If you couldn't wrap your head around the idea, spend some time thinking about it.

**How can we extend this to larger number of numbers?**  
Here's the neat idea! I will show this to you with the help of an example.
Let's take three numbers-- 30, 12 and 15. Factorize each of them.
$$30 = 2 \times 3 \times 5$$
$$12 = 2 \times 2 \times 3$$
$$15 = 3 \times 5$$

To treat each factor as a unique object for a number, we will use subscripts as
labels. $12$ has two $2$'s. So one of them becomes $2_1$ and the other becomes
$2_2$.  
Think of them as mental labels such that they don't change the nature
of the object that is labeled, i.e. $5_1$ is nothing but the number $5$.

Now we have
$$30 = 2_1 \times 3_1 \times 5_1$$
$$12 = 2_1 \times 2_2 \times 3_1$$
$$15 = 3_1 \times 5_1 $$

Now, we can think of each of the numbers as a set of objects, namely labeled
factors.
$$ S(30) = \left\{ 2_1, 3_1, 5_1 \right\} $$
$$ S(12) = \left\{ 2_1, 2_2, 3_1 \right\} $$
$$ S(15) = \left\{ 3_1, 5_1 \right\} $$

Now, think for a while about the intersection of two sets and try to figure out how
it may relate to the two numbers. When I say a number, I may refer to the set
of factors that constitute the number and vice-versa.

Yes, you got it right! The intersection of two sets is the set of common elements
which is equivalent to the set that represents the $HCF$ of the numbers
represented by the sets. And it's easy to see that $LCM$ is the union of sets
in our set-space representation of numbers. Too much of dance of terms! 

$$ S(30) \cap S(12) \cap S(15) = \left\{ 3_1 \right\} $$
And $3$ is indeed the $HCF$ of 30, 12 and 15.
$$ S(30) \cup S(12) \cup S(15) = \left\{ 2_1, 2_2, 3_1, 5_1 \right\} $$
And $2 \times 2 \times 3 \times 5 = 60$ is indeed the $LCM$ of the three
numbers.

Now, you must be familiar with the famous equation from Set Theory--
$$ A \cup B = A + B - (A \cap B) $$
You can either work in the set-space where each number is represented as the set
of its factors or do some translations right in the number-space to obtain the 
relationship implied by the above equation.
Translate each $+$ to multiplication and $-$ to division, along with the above
relations that we found for union and intersection. The equation easily becomes
$$ LCM(A, B) = \frac{A \times B}{HCF(A, B)} $$

Aha! Now let's go for three sets!
We already know,
$$ A \cup B \cup C = A + B + C - (A \cap B) - (B \cap C) - (C \cap A) + (A \cap B \cap C) $$

Doing the same set of translations to this equation, we have our little jewel--
$$ LCM(A, B, C) = \frac{A \times B \times C \times HCF(A, B, C)}{HCF(A, B) \times HCF(B,C) \times HCF(C,A)} $$

You can verify it for our near and dear companion numbers so far.
$$LCM(12,15,30) = 60 $$
And,
$$\frac{12 \times 15 \times 30 \times HCF(12, 15, 30)}
{HCF(12,15) \times HCF(15,30) \times HCF(30, 12)}$$
$$ = \frac{12 \times 15 \times 30 \times 3} {3 \times 15 \times 6} $$
$$ = 60 $$









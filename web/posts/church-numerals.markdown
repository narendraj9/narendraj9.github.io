---
title: Church Numerals
date: 2014-10-18
tags: Miscellany
---

Reading the second chapter of The Structure and Interpretation of Computer
Programs, I came across an exercise wherein natural numbers were defined
entirely in terms of lambda expressions, i.e. anonymous functions.

To understand how this can be accomplished, we must look into how one can have a
functional representation for numbers. To represent all of the natural numbers,
it is sufficient to have two things defined --

1. zero
2. succ, a function that returns the successor of the number it is provided with.

It's easy to see how the above two definitions would be sufficient to obtain any
natural number. You just have to repeatedly apply the successor function starting
from zero. But how on earth are we going to define both of them without using
any data at all!

Let's see how Mr. Church defined them.

~~~~ { .haskell }

-- zero is defined as a function that takes the successor funtion and
-- the representation for zero
-- and evaluates to the representation of zero. Seems legit.
zero = \f x -> x

-- succ function first gets the concrete representation for the numeral n,
-- then applies the function f to it to get the next numeral.
succ n = \f x -> f (n f x)

~~~~

If you try evaluating the functions to obtain representations for natural
numbers, you will find out that the definitions are as follows --

~~~~ { .haskell }
one = \f x -> f x
two = \f x -> f (f x)
three = \f x -> f (f (f x))
~~~~

We can figure out what the number is with this representation as long as we can
look *into* the definition of a function. It is so because the subsequent calls
to f that we are courting lie inside a function's body itself. That violates the
abstraction barrier, i.e. you should use a function without having any knowledge
about its implementation.

Before we come to problem of actually understanding the numbers in the
representation that we have, let's define the complete algebra of these numbers.

Definitions for addition and multiplication:

~~~~ { .haskell }
add m n = \f x -> m f (n f x)
multiply m n = \f x -> m (n f) x
~~~~

It's fairly easy to see how the above definitions work. For example to obtain
the product of numerals that represent 2 and 3, we must consider the internal
representation of one of the numerals as the *zero* and then call the successor
function as many times as the other numerals.

~~~~ { .haskell }
add two three
= \f x -> two f (three f x)
= \f x -> f (f (three f x))
= \f x -> f (f (f (f (f x))))
~~~~

~~~~ { .haskell }
multiply two three
= \f x -> two (three f) x
= \f x -> ((\f x -> f (f x)) (three f)) x
= \f x -> (\x -> (three f) ((three f) x)) x
= \f x -> (\x -> f (f (f ((three f x))))) x
= \f x -> (\x -> f (f (f (f (f (f x)))))) x
- \f x -> f (f (f (f (f (f x)))))
~~~~

If you are looking at these things for the first time, it may seem daunting.
Here's how you should think about them. Think of a number being internally
thought of as the number of times a special functions, i.e. the successor,
function has been applied to a special value, i.e. zero, to obtain that number.
For example, the number one is obtained by a single application of the successor
function to zero, number two requires to successive applications to zero and so on.

So far so good! We have a number system with an almost complete algebra. But how
do we make sense of these numbers. It turns out that if our language is a little
more expressive and we can define the successor function to do something to the
world, e.g. print a dot on the screen or increment the value stored in a
register by one unit, we can __understand__ the numbers that we have developed
so far.

How can we store Church Numerals in hardware?
---
I have no idea!

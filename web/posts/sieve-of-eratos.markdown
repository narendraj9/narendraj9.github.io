---
title: The Genuine Sieve of Eratosthenes
date: 2015-10-02
tags: Haskell, Mathematics
---

This post is inspired by the paper--
[The Genuine Sieve of Eratosthenes](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf).
Since I did the recreational activity of thinking about the wheels, I intend to
talk about their construction and the fact that they exhibit
[Monoid](http://mathworld.wolfram.com/Monoid.html) structure.

Sieve of Eratosthenes
---------------------

The problem we are trying to solve here is to find out all primes up to a given
natural number. Eratosthenes came up with an algorithm, famously known as the
*Sieve of Eratosthenes*. The gist of the algorithm is to pick up a prime and
then cross off all its multiples.

We start with writing the numbers from 2 to $n$. Let's call the first uncrossed
number $p$. Declare $p$ as a prime and cross all of its multiples starting at
$p^2$. Starting at $p^2$ is a slight optimization because the smaller numbers must
have been crossed by the primes we discovered earlier. Repeat this procedure until
you reach beyond $\sqrt{n}$. Declare the numbers that remain uncrossed as primes.

The home page of [haskell.org](https://www.haskell.org) has the following definition to illustrate
the ease with which Haskell lets us define infinite structures like the sequence
of prime numbers.

~~~~ { .haskell }
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
~~~~

But the above algorithm isn't *The Sieve of Eratosthenes* even though it is
given that name very often. The paper talks about the real *sieve* pretty well.

Spinning Wheels and the Sieve
-----------------------------

The above algorithm and its real counterpart can be improved by providing a
better list of numbers to the function $filterPrime$. For example, it would be
better if we avoid checking even numbers and all multiples of 3. This can be
achieved by generating the list to be passed using what is known as a *wheel*. 

Imagine yourself standing on the number line. You are initially on the number 1.
Now, I am supposed to give you the number of steps you must take so that you
end up on the next number that isn't a multiple of 2 or 3; or in general not a
multiple of any of the numbers in a set. What would be the steps that you would
take if that set of numbers happens to be empty? It's trivially a never-ending
sequence of 1's. You just walk the whole number line stepping upon each number.

Now, let's see what the sequence is when we have one element in the set,
say $p$. Since you are standing on 1, the first multiple is yet to be
encountered and is at $p$. All numbers between 1 and $p$ are numbers you must
step upon. So the sequence must contain $(p - 2)$ values equal to 1 followed
by a single 2. The 2 in the sequence is for the point in time when I ask you
to jump over $p$. After this, the same numbers just repeat themselves
indefinitely.

~~~~ { .haskell }
genWheel :: Int -> Wheel
genWheel x = let ys = replicate (x - 2) 1 ++ [2] ++ ys in Wheel ys
~~~~

Eventually, we would want our wheels to be generated for larger sets of numbers
because we would like to avoid as many multiples of already known primes as
possible. So, we want to combine two wheels, say `genWheel 2` and `genWheel 3`.
We definitely expect the `combine` operation to be associative and the wheel
generated for the empty set to be its identity element. Yes, it is a Monoid.

~~~~ { .haskell }
data Wheel = Wheel { getWheel :: [Int] }
           deriving Show

instance Monoid Wheel where
  mempty = Wheel (repeat 1)
  mappend (Wheel xs) (Wheel ys) = Wheel (go xs ys)
    where go as@(x:xs) bs@(y:ys) | x == y = x : go xs ys
                                 | x > y = go as (y + head ys : tail ys)
                                 | otherwise = go (x + head xs : tail xs) bs
~~~~

Our combine operation is defined above as `mappend`. The idea is to merge
consecutive elements of the wheel when we encounter a mismatch between the
two wheels to be combined. It makes sense because the sum of consecutive
elements on a wheel constitutes a valid step size. So, in essence we are trying
to morph the smaller wheel to look more like the larger wheel whenever we see
a local difference in the two. So, the identity wheel, i.e. the one generated
from the empty set, morphs trivially into any other wheel by always growing to
make its elements equal to the elements in the other wheel.

Now, all we need is a function that would help us generate the sequence of
numbers that we must check for primality given a wheel.
So, let's spin the wheel.

~~~~ { .haskell }
spin :: Wheel -> [Int]
spin (Wheel (x:xs)) = spin' xs (x + 1)
  where spin' (y:ys) n = n : spin' ys (n + y)
~~~~

We make use of the fact that the first potential prime number is one more than
the number at the head of the wheel. *Why? [Think about it.]*

Here's an example showing it in action.

~~~~ { .haskell }
λ> take 10 $ spin (genWheel 2)
[3,5,7,9,11,13,15,17,19,21]
λ> take 10 $ spin (genWheel 2 `mappend` genWheel 3)
[5,7,11,13,17,19,23,25,29,31]
λ> take 10 $ spin (genWheel 2 `mappend` genWheel 3)
[5,7,11,13,17,19,23,25,29,31]
~~~~













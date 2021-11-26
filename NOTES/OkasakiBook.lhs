- [Purely Functional Data Structures](https://web.archive.org/web/20200206082417/https://doc.lagout.org/programmation/Functional%20Programming/Chris_Okasaki-Purely_Functional_Data_Structures-Cambridge_University_Press(1998).pdf)

Resources:

- [Appendix code](https://github.com/aistrate/Okasaki/tree/master/Original)
- [Solutions by kgeorgiy](https://github.com/kgeorgiy/okasaki)

-- 1.2 - Strict vs. Lazy Evaluation

"As we will se in Chapters 3 and 4, strict languages can describe worst-case data structures, but not amortized one, and lazy languages can described amortized data structures, but not worst-case ones. To be able to describe both kinds of data structures, we need a programming language that supports both evaluation orders."

"Persistent data structures. Until this research, it was widely believe that amortization was imcompatible with persistence [DST94, Ram92]. However, we show that memoization, in the form of lazy evaluation, is the key to reconciling the two. Furthermore, as noted by Kaplan and Tarjan [KT96b], functional programming is a convenient medium for developing new persistent data structures..."

-- 1.6 - Overview

"[Chapter 4] describes how one can often derive a worst-case data structure from an amortized data structure by systematically scheduling the premature execution of lazy components."

-- 2.1 - Lists

_Persistent_

Updating a funcitonal data structure does not destroy the existing version, but rather creates a new version that _coexists_ with the old one.

During an update affected nodes are copies rather than changes to the original. Unaffected nodes can therefore be shared between the old and new versions without worrying about visibility to the new one.

Figure 2.3

\begin{code}
{-# LANGUAGE BangPatterns #-}
import Prelude hiding ((++))

data List a = Nil | Cons !a !(List a)
    deriving Show
\end{code}

Figure 2.4 remarks, O(1) list concat can be done in an imperative setting by utilizing a pointer to the last element of a list and switching it to the head of the next list. This effectively consumes/destroys the function arguments however.

```c
struct List {
    struct ListNode* head;
    struct ListNode* end;
};

struct ListNode {
    void * value;
    struct ListNode* next;
};

struct List concat(struct List xs, struct List ys){
    //Stitch lists together
    xs.end->next = ys.head;

    struct List zs = {
        xs.head,
        xs.end
    };

    return zs;
};
```

Without Rust-style move semmantics this detail wouldn't be obvious that they are consumed. Passing it by value in C doesn't necessarily suggest that either.

\begin{code}
(++) :: List a -> List a -> List a
(++) (Nil) ys = ys
(++) (Cons x xs) ys = Cons x (xs ++ ys)
\end{code}

The nil clause effectively points directly at the original list.

```
zs -> |0|->|1|->|2|--------|
                           \/
xs -> |0|->|1|->|2|  ys -> |3|->|4|->|5|NIL|
```

\begin{code}

update :: List a -> Int -> a -> List a
update (Cons x xs) 0 y = Cons y xs
update (Cons x xs) i y = Cons x $ update xs (i - 1) y
update Nil _ _ = error "Subscript"

--xs = Cons 1 $ Cons 2 $ Cons 3 Nil
\end{code}

One thing to note is that the Cons constructor is strict in both arguments so upon the List will be fully evaluated upon evaluation of update.

† In Chapters 10 and 11 we'll see how to support concat in O(1) time without sacrificing persistence.

--- Exercise 2.1

Write a function suffixes of `List a -> a` that takes the list xs and reutrns a list of all the suffixes of xs in decreasing order of length.

```
suffixes [1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], []]
```

Show that the resulting list of suffixes can be genearted in O(n) time and represented in O(n) space.

For every list there are N list suffixes that are accessible by traversing the cons operator and recursing on the tail. This can be make more clear by using a fold.

\begin{code}
tx = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil

suffixesL :: List a -> List (List a)
suffixesL Nil = Nil
suffixesL l@(Cons x xs) = Cons l $ suffixesL xs

foldrL f z Nil = z
foldrL f z (Cons x xs) = f x $ foldrL f z xs

headL :: List a -> a
headL (Cons a _) = a
headL Nil = error "No head of empty list."

--From kgeorgiy
suffixesFoldrL :: List a -> List (List a)
suffixesFoldrL = foldrL (\x xs -> Cons (Cons x $ headL xs) xs) $ Cons Nil Nil

--A more ghci readable version.
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(x:xs) = l : suffixes xs

suffixesFoldr :: [a] -> [[a]]
suffixesFoldr = foldr (\x xs -> (x : head xs) : xs) $ [[]]
\end{code}

The foldr version makes sense when you consider it starts from the right side of list and builds back up.

[[1,2,3,4],[2,3,4],[3,4],[4],[]

At the top level x is 1 and xs is [[2,3,4],[3,4],[4],[]] so it cons 1 and [2,3,4] (the head of xs).

TODO ghcvis this.

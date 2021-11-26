- [Purely Functional Data Structures](https://web.archive.org/web/20200206082417/https://doc.lagout.org/programmation/Functional%20Programming/Chris_Okasaki-Purely_Functional_Data_Structures-Cambridge_University_Press(1998).pdf)

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

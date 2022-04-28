# [An Efficient Unification Algorithm](https://dl.acm.org/doi/pdf/10.1145/357162.357169)

## MISC

- [unification-fd](https://github.com/wrengr/unification-fd)
- [Unification-fd tutorial (part 1/n)](https://winterkoninkje.dreamwidth.org/100478.html)
- [Atze Dijkstra, Arie Middelkoop, S. Doaitse Swierstra (2008) Efficient Functional Unification and Substitution/, Technical Report UU-CS-2008-027, Utrecht University.](http://www.cs.uu.nl/research/techreps/repo/CS-2008/2008-027.pdf)
- [Roman Cheplyaka - Generic Unification](https://ro-che.info/articles/2017-06-17-generic-unification)
- [Handbook of automated reasoning Vol. 1 - Chapter 8 1.2 History and applications](https://cloudflare-ipfs.com/ipfs/bafykbzaceaazizzz5zga7bgogllndxijks457hdym6h6igovzrmk7huatdqqa?filename=Robinson%20A.%2C%20Voronkov%20A.%20%28eds.%29%20-%20Handbook%20of%20automated%20reasoning%20Vol.%201%20%5B...%5D-Amsterdam%20%5Bu.a%5D%20Elsevier%20%5Bu.a.%5D%20%282001%29.djvu)
- [Type inference for ML](https://www.ccs.neu.edu/home/amal/course/7480-s12/inference-notes.pdf)
- [MM Unification Algorithm Notes](http://www.ale.cs.toronto.edu/docs/ref/ale_trale_ref/ale_trale_ref-node4.html)

## Misc common place

From Handbook of automated reasoning, page 448:
"In the propositional case, the resolution pricinple can be described as follows, see also [Bachmair and Ganzinger 2001] (Chapter 2 of this Handbook). Assume that clauses $C \vee p$ and $C\prime \vee \neg p$ have already been derived (where $C$, $C\prime$ are sub-clausses and p is a propositional variable). Then one can also deduce $C \vee C\prime$

In the first-order case, the clauses one starts with may contain variables. Herbrand's famous theorem implies that finitely many ground instances (i.e., instances obtained by substituting all variables by terms without variables) are sufficient to show unsatisfiability of a given unsatisfiable set of classes by propositional reasoning. (e.g., propositonal resolution). The problem is, however, to find the appropriate instantiations.

...

Instead of using all ground unifiers for instantiation, Robinson proposed to lift the resolution principle to terms with variables, and apply only the most general unifier $\sigma$ of $s$ and $t$. In the example, this yields the resolvent $(C \vee C\prime)\sigma$

## Historical Notes from [F.Pfenning's notes](http://symbolaris.com/course/constlog16/18-unicomp.pdf#page=4)

"It is possible to improve the complexity of unification to linear in the size of the input terms if a different represntation fro the terms and substitutions is chose, such as a set of multi-equations [FP4, FP5], or dag structures with parent pointers [FP6]..."

Multi-equations : Martelli Montanari
Dag structures with parent poitners : Paterson Wegman

- FP4 Alberto Martelli and Ugo Montanari. Unification in linear time and space: A structured presentation. Internal Report B76-16, Istituto di Elaborazione delle Informazione, Consiglio Nazionale delle Ricerche, Pisa, Italy, July 1976.
- FP5 Alberto Martelli and Ugo Montanari. An efficient unification algorithm. ACM Transactions on Programming Languages and Systems, 4(2):258–282, April 1982.
- FP6 [15] M. S. Paterson and M. N. Wegman. Linear unification. Journal of Computer and System Sciences, 16(2):158–167, April 1978

## Section 1

- [10] [Predicate Logic as a Programming Language](https://www.doc.ic.ac.uk/~rak/papers/IFIP%2074.pdf)

"Kowalski's idea of interpreting predicate logic as a programming
language [10]. Here predicate logic clauses are seen as procedure declarations,
and procedure invocation represents a resolution step. From this viewpoint,
theorem provers can be regarded as interpreters for programs written in predicate
logic, and this analogy suggests efficient implementations [3, 25]."

- [3] [BOYER, R.S., AND MOORE, J.S. The sharing of structure in theorem-proving programs. In Machine Intelligence, vol. 7, B. Meltzer and D. Michie (Eds.). Edinburgh Univ. Press, Edinburgh, Scotland, 1972, pp. 101-116. ](https://www.cs.utexas.edu/users/moore/publications/structure-sharing-mi7.pdf)

- [25] [WARREN, D.H.D., PEREIRA, L.M., AND PEREIRA, F. PROLOG--The language and its implementation compared with LISP. In Proceedings of Symposium on Artificial Intelligence and Programming Languages, Univ. of Rochester, Rochester, N.Y., Aug. 15-17, 1977. Appeared as joint issue: SIGPLAN Notices (ACM) 12, 8 (Aug. 1977), and SIGART Newsl. 64 (Aug. 1977), 109-115. ](http://www-public.int-evry.fr/~gibson/Teaching/CSC4504/ReadingMaterial/WarrenPereiraPereira77.pdf)

"attempts have been made to find more efficient algorithms [2, 7, 13, 15, 16, 22]."

- [2] [BAXTER, L.D. A practically linear unification algorithm. Res. Rep. CS-76-13, Dep. of Applied Analysis and Computer Science, Univ. of Waterloo, Waterloo, Ontario, Canada.](https://cs.uwaterloo.ca/research/tr/1976/CS-76-13.pdf)
- [7] HUET, G.  Resolution d'Equations dans des Langages d'Ordre 1,2,...ω (These d'etat). Universite de Paris VII.
- [13] MARTELLI, A., AND MONTANARI, V. Unification in linear time and space: A structured presentation. Internal Rep. B76-16, Ist. di Elaborazione delle Informazione, Consiglio Nazionale delle
Ricerche, Pisa, Italy, July 1976.
- [15] [PATERSON, M.S., AND WEGMAN, M.N. Linear unification. J. Comput. Syst. Sci. 16, 2 (April 1978), 158-167.](https://dl.acm.org/doi/pdf/10.1145/800113.803646)
- [16] ROBINSON, J.A. Fast unification. In Theorem Proving Workshop, Oberwolfach, W. Germany,
Jan. 1976.
- [22] VENTURINI ZILLI, M. Complexity of the unification algorithm for first-order expressions. Calcolo
12, 4 (Oct.-Dec. 1975), 361-372.


"Unification algorithms have also been extended to the case of higher order logic [8] and to deal directly with associativity and commutativity [20]."

- [8] [HUET, G.P. A unification algorithm for typed λ-calculus. Theor. Comput. Sci. 1, 1 (June 1975), 27-57.](https://www.sciencedirect.com/science/article/pii/0304397575900110/pdf?md5=3970bc02d8c05c03641a35e8fb08f94d&pid=1-s2.0-0304397575900110-main.pdf)

The problem was also tackled from a computational complexity point of
view, and linear algorithms were proposed independently by Martelli and Montanari [13] and Paterson and Wegman [15].

- [13] MARTELLI, A., AND MONTANARI, V. Unification in linear time and space: A structured presentation. Internal Rep. B76-16, Ist. di Elaborazione delle Informazione, Consiglio Nazionale delle
Ricerche, Pisa, Italy, July 1976.
- [15] [PATERSON, M.S., AND WEGMAN, M.N. Linear unification. J. Comput. Syst. Sci. 16, 2 (April 1978), 158-167.](https://dl.acm.org/doi/pdf/10.1145/800113.803646)

"the performance of this algorithm is compared with that
of two well-known algorithms, Huet's [7] and Paterson and Wegman's [15].

- [7] HUET, G.  Resolution d'Equations dans des Langages d'Ordre 1,2,...ω (These d'etat). Universite de Paris VII.
- [15] [PATERSON, M.S., AND WEGMAN, M.N. Linear unification. J. Comput. Syst. Sci. 16, 2 (April 1978), 158-167.](https://dl.acm.org/doi/pdf/10.1145/800113.803646)

## Section 2

### Algorithm 1

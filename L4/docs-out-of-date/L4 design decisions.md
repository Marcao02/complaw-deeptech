# Type system

My current favoured plan: Whatever fancy things we put into L4's type system, they are all mere abbreviations for simply-typed function symbols, and those abbreviations are expanded in the _minimal_ S-expression syntax/AST (note: we can still offer a non-minimal S-expression syntax that has such niceties, but that we will consider one of a number of possible concrete syntaxes).

One reason for this is that most SMT languages and first-order theorem provers don't support types beyond that.

It is also not hard to achieve. For example, if we have a polymorphic `if · then · else ·` expression of type `Πα. α -> α -> bool` in the concrete syntax, then whether or not the `α` is always/sometimes inferred by type inference (versus given explicitly), in the minimal S-expression syntax, all instances of that symbol will be something like
`("if_then_else[Int]" t b1 b2)` or `("if_then_else[List[Int]]" t b1 b2)` etc, where from the perspective of the execution engine, NLG (*without extra special handling linking it back to the polymorphic version*, which we will still have the option of), and formal verification, `if_then_else[List[Int]]` is just a simply-typed function symbol of type `"List[Int]" -> "List[Int]" -> Int`, and `"List[Int]"` is just an atomic type.

# post from Slack

Updates and some things I’m thinking about today when I have to be afk, in case anyone on here has a few spare afk cycles too. @mengwong @jobchong @virgil @vi

I'm quite confident that delaying the writing of a parser for nicer-looking L4 concrete syntax was a good idea, for at least these reasons:
	(1) It lets us quickly experiment with concrete syntax options by modifying the pretty printer (aka “unparser”), without having to modify any examples.
	(2) It mitigates the inevitable fracturing issue of disagreement on what the L4 concrete syntax "should" be.

I'm fairly confident that writing a separate definition of the formal-verification-facing version of L4, currently known as Linear State Machines (better name wanted), and given in (the slightly out-of-date) `L4-LSM-Formal-Semantics-LaTeX`, was also a good idea, for at least these reasons:
	(3) It results in a lower barrier to entry for academic formal verification experts.
	(4) Similar to (2) above, it mitigates the fracturing issue of differing opinions on what L4 "should" be. It's easier to get people to agree on math than on languages.
	(5) There are human-centric features we want for L4 that go beyond concrete syntax, but that would be unnecessary complications for formal verification. An example: human representations of dates/time are important in contracts, but they are complex enough that I don't think they can be accurately modeled as mere syntactic sugar (i.e. a feature of the concrete syntax only), as the computational contracts academics always do.

I imagine Vitalik and Yoichi and others came to similar conclusions while designing the EVM (the closest analogue of LSMs in this post) vs Solitidy Assembly vs (Solidity / Viper / Bamboo etc).

We currently don't have a well-delineated explicit model of Linear State Machines in the python code. One of the things I'll be thinking about on my run today is whether we should (this would be (v) in the list below). Would be grateful for any thoughts.

In summary, we have:
(i)   L4 concrete syntax, which is only imagined by the pretty printer for (ii) atm.
(ii)  L4 AST, the s-expression language, that I'm writing examples in.
(iii) The somewhat-higher-level-than-AST in-memory python representation of L4 files.
(iv)  The Linear State Machines mathematical model.

We don't currently have anything for NLG implemented in the python code. The plan has been to leverage all the work that has gone into multi-lingual NLG by the GF community. @inari @paula

In Sweden we also talked about writing a parser for L4 concrete syntax in GF. I no longer think that's a good idea, but I'm not confident (it's a big dependency, and the GF community is focused on natural language parsing...). What I'm more confident about is that if we were to use GF for that, it should be _completely separate_ from the use of GF for NLG.

I still like the idea of using GF for multi-lingual NLG. However, it is not clear to me whether we should be doing that _soon_. We do want to have a connection between the L4 and natural language versions of SAFE/KISS/etc _soon_, but that could be by less sophisticated means, e.g. by adding location labels to our V2 SAFE representation, and putting those labels in various parts of the L4 file. I'd be very grateful for some opinions about this.
Minesweeper Reimplementation
============================
This is our project for our Networks course. For the original paper, look at Ryan Beckett's ["A General Approach to Network Configuration Verification"](https://dl.acm.org/doi/10.1145/3098822.3098834). Minesweeper is a network verification tool, as opposed to the various network testing tools like Batfish. It accepts a network configuration and high-level properties that we wish to check (reachability, isolation, presence of black holes, etc.) and spits out SMT constraints. Beckett considers two classes of optimizations -- hoisting and slicing -- on Minesweeper to spit out constraints that Z3 is more likely to solve. We suspect that slicing is merely dead code elimination. Hoisting seems related to common subexpression elimination. 

Using SMT constraints as a target language has several benefits. 
1. We can verify over entire network graphs, instead of testing on individual paths.
2. Similarly, we can generalize over message sets rather than compute them in every case.
3. We can, obviously, throw SMT constraints at Z3 and hope for a solution. :)

This is a work in progress, but our code seems to be much more concise than the original implementation thus far. This is due to two major reasons:
1. We use Scala, the original source is in Java. 
2. The original codebase is heavily dependent on Batfish. We don't really know why yet. Batfish is one of the tools that Beckett contrasts Minesweeper with in the paper. As far as we saw, Minesweeper was not using any analyses from Batfish, but simply some data structures for network components and an intermediate representation of constraints. 

Note: Our first reimplementation attempt was also dependent on Batfish, but we eventually chose to remove that dependency and implement some basic network components from scratch, since interacting with Java libraries in Scala requires you to wrap the Java code in a straightforward but tedious manner to avoid antipatterns in Scala (for example, nullable references need to be promoted to options). The bulk of the work seemed to be implementing this interface, and that wasn't where we wanted to be spending our time, so we decided to forgo Batfish and define simple network components instead. For now, we have a concrete network interface, but I expect we should be able to abstract these out with traits and provide implementations of these traits using Batfish structures. However, that is beyond the scope of this project.

TODO
----
- High level properties -- create mapping to MS
- Delete Z3
- Write testing framework
- Dead code elimination
- Hoisting

Bonus
-----
- Additional optimizations: I'm not confident that we will be able to find any notable new optimizations since the language defined is quite simple.
- Evaluation testing -- how do we generate network graphs so that we can evaluate our implementation?
- Input interface for filters
- pretty-print the graph
- clean up prog
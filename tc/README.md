# About

Implementation of the tiger language described in "modern compiler implementation in ML", using rust.

Currently x86_64 programs can be generated. However there are no optimizations applied. Trivial register allocation is used to color registers, resulting in very bloated and inefficient assembly where every instruction reads and writes to memory.

If I had to rate the book, I'd give it a 6/10 to 7/10 for the writing quality. It could have used an editor and better examples. I think it suffers from the need to balance brevity/linearity and depth, especially in the first 12 chapters where it needs to usher along a specific implementation.

A nice supplement read is "Engineering a Compiler" by Cooper and Torzon. It is up-to-date (latest edition from 2023), has enough breadth in coverage of each topic, and is not as pedantic on the technical details as Dragon book. It also does a great job of providing an example to illustrate algorithms and concepts, which really aids with understanding.

The dragon book remains a nice reference. Beware of the "international" edition that omits chapter 12 on interprocedural optimization.

I find Muchnick's book comprehensive on the dataflow analysis techniques up to the time of its writing, but the writing style leave much to be desired. Basically sentences don't flow and the writing is on the dry side.

# Progress
- [x] 2 Lexical analysis
- [x] 3 Parsing
- [x] 4 Abstract syntax
- [x] 5 Semantic analysis
- [x] 6 Activation records
- [x] 7 Translation to IR
- [x] 8 Basic blocks and traces
- [x] 9 Instruction selection
- [ ] 10 Liveness analysis
- [ ] 11 Register allocation
- [ ] 12 Checkpoint

Optimization is a very interesting topic. I probably would come back to actually implementing liveness analysis and global register allocation later, but atm feel more interested in figuring out the general code organization needed to make a relatively generic dataflow analysis framework. Also there are other distractions, so I might not come back to do the implementation for some time.

[done][2 days 7/19, 7/20] trivial register allocation
[done][1 days 7/19] link it with the runtime - yup full of errors
[done][0.25 day 7/23] for test purpose, might want to allow tigermain to return int types. this way output can be checked on cmdline.
[done][3 days 7/24, 7/25, 7/26] experiment on building out the end to end test harness
    x add rules_rust to WS
    x convert project to build using bazel
    x current tests will just be run with cargo test, because i am not gonna write out a bunch of rust_library targets for no reason
[done][0.5 day] fix up the use and deps in the generated Instr objects for register allocation use. (no time spent as this should not work for trivial register allocation if we forgot to specify a source. at any rate, can revisit when implementing register allocation)
[done][0.25 days 7/26] get all the e2e tests to work (not merge.tig or queens.tig as those are the big ones)
[done][2 day 7/27, 7/28] get merge.tig and queens.tig to work

# Build
```
cargo build --release
```

# How to run tests

## end to end tests
```
bazel test //tc::e2e_test
```
## unit tests
```
cargo test
```
non-trivial instruction selection, what is the cost of using complex addressing mode of the form [t1 + t2*k + c]? is there actual speed gain from using this besides a smaller code size (ok so less pressure on code caches is one).

segways into microarchitecture for x86 and understanding the cost of assembly instructions! turtles all the way down.

want to take a bit of time to look through https://www.agner.org/optimize/optimizing_assembly.pdf

everything from liveness onwards is classic compiler optimization techniques involving dataflow analysis.

non-trivial instruction selection, what is the cost of using complex addressing mode of the form [t1 + t2*k + c]? is there actual speed gain from using this besides a smaller code size (ok so less pressure on code caches is one).

segways into microarchitecture for x86 and understanding the cost of assembly instructions! turtles all the way down.

want to take a bit of time to look through https://www.agner.org/optimize/optimizing_assembly.pdf

everything from liveness onwards is classic compiler optimization techniques involving dataflow analysis.


# Other topics
- [ ] Pipelining, scheduling
- [ ] Memory hierarchies
- [ ] Garbage collection
- [ ] Functional features
- [ ] Polymorphic types
- [ ] Garbage collection
- [ ] Oop features
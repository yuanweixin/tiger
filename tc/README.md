# About

Implementation of the tiger language described in "modern compiler implementation in ML", using rust.

Currently x86_64 programs can be generated. However there are no optimizations applied. Trivial register allocation is used to color registers, resulting in very bloated and inefficient assembly where every instruction reads and writes to memory.

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

non-trivial instruction selection, what is the cost of using complex addressing mode of the form [t1 + t2*k + c]? is there actual speed gain from using this besides a smaller code size (ok so less pressure on code caches is one).

segways into microarchitecture for x86 and understanding the cost of assembly instructions! turtles all the way down.

want to take a bit of time to look through https://www.agner.org/optimize/optimizing_assembly.pdf

everything from liveness onwards is classic compiler optimization techniques involving dataflow analysis.

can mentally split into the analyzer vs optimizer step. the analyzer does the analysis but the optimizer is the one that does the side effect of changing the code. i don't think they are necessarily separate; each type of optimization would do analysis then modify the cfg (there is probably no good reason at all, to do it "functional" style where the effects are reified and separate from their application; if anything this in theory turns the transformation into a protocol, but we don't need that here).

also, the program representation matters. namely,
* quads (textbooks assume use of triples; quads is a form of triples)
* explicit control flow graph (model it after the module in appel book)
* basic blocks in control flow graph containing quads
* convert to ssa on the get to (might as well, doesn't make sense to do this later)

then each individual optimization needs to be expressed in terms of the data flow framework (meet operator, direction, transfer function).

other considerations:
* "patching up" dataflow results due to modification of cfg.
* running dataflow on basic blocks to speed it up instead of on individual statements, requiring composing the transfer function of the statements in the basic block.
* interface for dataflow analysis (start with the one in appel book)

testing
this is def a tough one to test, input will be cfg, and minimal test should contain straightline code, conditionals and loops. for checking analysis, the associated data of the cfg nodes will be checked. for code transforms, the actual cfg itself needs to be matched against expected value.

for each optimization, determine this matrix of info:
* direction
* meet function
* transfer function
* is it monotonic?
* is it distributive?

algorithms used
* depth first ordering
* nonreducible graph detection using back edge deletion + cycle detection
* natural loop construction (assume reducible graph)

[2-3 days] understand liveness
[2-3 days] implement liveness.
[2-3 days] understand register allocation.
[2-3 days] impl register allocation
[1+1 days] plug in register allocation.
[0.5 day] fix up the compiler options.

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
# Other topics
- [ ] Dataflow analysis
- [ ] Loop optimization
- [ ] SSA
- [ ] Pipelining, scheduling
- [ ] Memory hierarchies
- [ ] Garbage collection
- [ ] Functional features
- [ ] Polymorphic types
- [ ] Garbage collection
- [ ] Oop features
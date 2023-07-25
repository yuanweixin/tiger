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
[3 days] experiment on building out the end to end test harness
    * add rules_py to workspace and test out
    x add rules_rust to WS
    x convert project to build using bazel
    * be able to bazel test the existing tests
    * add stubs for the python e2e test harness, capturing stdout and return code
    * figure out how to invoke the assembler on this thing
    * add the build rule for the runtime.c
    * figure out the build rule for: build tc, take output, link it against runtime.s
[1-2 day] get merge.tig and queens.tig to work!
[0.5 day] fix up the use and deps in the generated Instr objects for register allocation use.
[2-3 days] understand liveness
[2-3 days] implement liveness.
[2-3 days] understand register allocation.
[2-3 days] impl register allocation
[1+1 days] plug in register allocation.
[0.5 day] fix up the compiler options.


# Task list
Use a standard error format <line>:<column> error:<description>

--irgen dump the ir into a file

build irrun to interpret the ir, should be good for debugging.

--irrun run and interpret the ir using the irrun subprogram.

bring in the runtime lib.

--target <linux|windows|macos>, only linux be supported.

--report-opts output the list of optimizations supported by compiler.

--optir <phase> : output the ir of <phase>. phase minimally supports [begin|final]. in file_<phase>.ir, where `file` is name of the input file.

--optcfg TODO

-d <path> where to place generated assembly files.

-O<opt>: enable optimization <opt>. option can be repeated. if this is specified then other optimizations are off.

    <opt> can be:
    – cf: Constant folding
    – reg: Register allocation
    – mc: Move coalescing (and register allocation)
    – cse: Common subexpression elimination
    – alg: Algebraic optimizations (identities and reassociation)
    – copy: Copy propagation
    – dce: Dead code elimination
    – inl: Inlining
    – sr: Strength reduction
    – lu: Loop unrolling
    – licm: Loop-invariant code motion
    – pre: Partial redundancy elimination
    – cp: Constant propagation
    – vn: Local value numbering
    – sa: Stack-allocate non-escaping records and arrays

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
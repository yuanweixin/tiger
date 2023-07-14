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

# Task list
Use a standard error format <line>:<column> error:<description>

-O option to disable optimizations

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
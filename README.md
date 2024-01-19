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

Optimization is a very interesting topic. I probably would come back to actually implementing liveness analysis and global register allocation later, but was sidetracked by impulsive need to figure out the general code organization needed, to create a relatively generic dataflow analysis framework. Alas there are not a single framework but multiple ones exist. 

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

# Other topics in the book 
- [ ] Pipelining, scheduling
- [ ] Memory hierarchies
- [ ] Garbage collection
- [ ] Functional features
- [ ] Polymorphic types
- [ ] Garbage collection
- [ ] Oop features

# Review of Rust language 

## Tooling - top notch
This is top notch. An actual package manager exists. There's ONE package manager that dominate, so none of the bullshit in e.g. JS. 

## Ecosystem - okay to decent 
There's 2-3 LR parser generator libraries, but the one that is supposed to imitate YACC refuses to allow rule precedence for their own cynical aesthetic reasons (irrc they added some support for that, but it is at best an undocumented feature). The lib I ended up using is actually an academic implementation of a parser generator with automatic error recovery. I find it less of a hassle to use than the shitty yacc imitator, and it is nice that somebody finally decided to implement a (better imo) parser generator algorithm. So, this one experience tells me ymmv with Rust libs depending on what you trying to do. 

The package versioning in rust tend to err on side of immaturity i.e. a lot of things are pre 1.0. Idk why this is the case, seems to be the culture. In practice this makes it hard to pick out "mature libraries" to use in prod scenario. 

## Borrow checker 

It's not really that difficult to grok for me. Initially I feared it would be very tricky, but if you understand multi-reader single-writer locks, then it's more or less the same thing but for variables. 

## Language UX - okay but language mechanisms start to grate on my nerves and drive me up a wall as time goes by 

The major saving grace is the super friendly and helpful error messages from compiler that mostly tell you where things went wrong with borrow checking. I don't feel confused by what is going on. 

Over time, I get a hunch about where borrow checker might blow up. But it is a mental overhead for me to do this. 

I understand the reason they have all these smart objects e.g. Rc to track lifetimes, but frankly it start to get really irritating having to type out long ass type signatures. 

The language is very low level. It does not make things easy such as "take this vector of stuff A and transform it to vector of stuff B". Maybe that's not the best example, but basically I cannot effortlessly translate a high level idea into code. I have to spend extra effort to just write it out in a Rust way. This pain started to get searing when I got to thinking about how to implement dataflow analysis framework. I find there is cognitive dissonance between how I want to think about the problem (if I was using a functional language, I'd say that has the most natural affinity with dataflow analysis as it is fundamentally just a series of transformations on some in-memory representation), and how it needs to be implemented (mutation, on maybe vectors of structs or whatever, and probably have to be written out in a very clumsy way). Maybe it's because I didn't have the concepts being implemented clear in my head, but in no way did "thinking in terms of the Rust language" help. If anything it just made me feel like doing mental juggling between understanding high level concept and the ugliness of my implementation.  

## Language scalability to larger projects - decent 
Ad hoc polymorphism using Haskell style trait classes work reasonably, I more or less pretend it is glorified interface file. Here, implementation details/language mechanisms creep in, such as the differentiation between trait objects (dispatch overhead) vs generics, which you have to keep in mind in some cases. 

I read about compile times getting longer on larger projects but obviously this is a tiny one, so haven't experienced that myself. 

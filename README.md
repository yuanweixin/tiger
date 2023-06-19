# tc command line 

Taken from [EPITA website, accessed March 9, 2023](https://assignments.lrde.epita.fr/reference_manual/implementation/invoking_tc/invoking_tc.html) but removed some of the flags that don't make sense to me or are not applicable. 

$ tc --help
Tiger Compiler, Copyright (C) 2004-2021  LRDE.:

0. Tasks:
  
  --task-list                      list registered tasks
  
  --task-graph                     show task graph
  
  --task-selection                 list tasks to be run
  
  --time-report                    report execution times

1. Parsing:

  ~~--scan-trace                     trace the scanning (use nimbleparse)~~

  ~~--parse-trace                    trace the parse (use nimbleparse)~~

  ~~--enable-extensions              enable extensions (extension to be implemented as a fork/branch because there are 3 extensions possible: oo, fp and polymorphic types; this is just the base version of the language as described in Appel)~~

  --prelude 
  STRING                 name of the prelude.  Defaults to "builtin" 
                                   denoting the builtin prelude
  
  -X [ --no-prelude ]              don't include prelude

  ~~--parse                          parse a file (it happens anyway, what's the point of this)~~
  
  --library-display                display library search path
  
  -P [ --library-append ] DIR      append directory DIR to the search path
  
  -p [ --library-prepend ] DIR     prepend directory DIR to the search path

2. Abstract Syntax Tree:
  -A [ --ast-display ]             display the AST
  
  ~~--ast-dump                       dump the AST (already have option to display it)~~
  
  --explicit-wrapping              explicit parenthesis wrapping in AST display
  
  --tikz-style                     enable TikZ-style output in AST dumping
  
  --dot-style                      enable Graphviz-style output in AST dumping

~~2.5 Cloning:
  --clone                          clone the Ast (why is this a thing?)~~

3. Bind:
  --bound                          default the computation of bindings to Tiger
                                   (without objects nor overloading)
  -b [ --bindings-compute ]        bind the identifiers
  -B [ --bindings-display ]        enable bindings display in the AST
  --rename                         rename identifiers to unique names

3. Callgraph:
  --escapes-sl-compute             compute the escaping static links and the 
                                   functions requiring a static link
  
  --escapes-sl-display             enable static links' escapes in the AST
  
  --callgraph-compute              build the call graph
  --callgraph-dump                 dump the call graph
  --parentgraph-compute            build the parent graph
  --parentgraph-dump               dump the parent graph

3. Escapes:
  -e [ --escapes-compute ]         compute the escaping variables and the 
                                   functions requiring a static link
  -E [ --escapes-display ]         enable escape display in the AST
  --escapes-check                  check that escape tags are correct
  --escapes-necessary-check        check that tagged variables are escaping
  --escapes-sufficient-check       check that escaping variables are tagged
  --escapes-tags-display           enable escape tags display in the AST

4. Type checking:
  -T [ --typed ]                   default the type-checking to Tiger (without 
                                   objects nor overloading)
  --types-compute                  check for type violations

4.5 Type checking with overloading:
  --overfun-bindings-compute       bind the identifiers, allowing function 
                                   overloading
  -O [ --overfun-types-compute ]   check for type violations, allowing function
                                   overloading

5. Translation to High Level Intermediate Representation:
  --hir-compute                    translate to HIR
  -H [ --hir-display ]             display the HIR
  --hir-without-desugar            display the HIR considering that desugar for
                                   ForExp and string comparison should already 
                                   be done
  --hir-naive                      don't use "Ix" during the translation

5.5. Translation to LLVM Intermediate Representation:
  --llvm-compute                   translate to LLVM IR
  --llvm-runtime-display           enable runtime displayingalong with the LLVM
                                   IR
  --llvm-display                   display the LLVM IR

6. Translation to Low Level Intermediate Representation:
  --canon-compute                  canonicalize
  --canon-trace                    trace the canonicalization of the LIR
  -C [ --canon-display ]           display the canonicalized IR
  --traces-compute                 make traces
  --traces-trace                   trace the traces computation
  --lir-compute                    translate to LIR (alias for --trace-compute)
  -L [ --lir-display ]             display the low level intermediate 
                                   representation

7. Target selection:
  -i [ --inst-compute ]            select the instructions
  -R [ --runtime-display ]         display the runtime
  --inst-debug                     enable instructions verbose display
  --rule-trace                     enable rule reducing display
  --garbage-collection             enable garbage collection
  -I [ --inst-display ]            display the instructions
  -Y [ --nolimips-display ]        display Nolimips compatible instructions 
                                   (i.e., allocate the frames and then display 
                                   the instructions
  --targeted                       default the target to MIPS
  --target-mips                    select MIPS as target
  --target-ia32                    select IA-32 as target
  --target-arm                     select ARM as target
  --target-display                 display the current target
  --callee-save NUM                max number of callee save registers
  --caller-save NUM                max number of caller save registers
  --argument NUM                   max number of argument registers

8. Liveness:
  -F [ --flowgraph-dump ]          dump the flowgraphs
  -V [ --liveness-dump ]           dump the liveness graphs
  -N [ --interference-dump ]       dump the interference graphs

9. Register Allocation:
  --asm-coalesce-disable           disable coalescence
  --asm-trace                      trace register allocation
  -s [ --asm-compute ]             allocate the registers
  -S [ --asm-display ]             display the final assembler

Combine:
  --c-object                       combine objects
  --c-bounds                       combine bounds checking
  --c-escapes                      combine escapes
  --c-desugar                      combine for and string comparison desugaring
  --c-inline                       combine inlining
  --c-prune                        combine pruning
  --c-overload                     combine overloading
  -a [ --c-all ]                   combine all compiler options
  --combine-bindings-compute       bind the identifiers, allowing various 
                                   compiler options
  --combine-types-compute          check for type violations, allowing various 
                                   compiler options
  --combine-rename                 rename identifiers to unique names, allowing
                                   various compiler options
  -c [ --combine-desugar ]         remove object and complex constructs from 
                                   the programallowing various compiler options

Desugaring and bounds-checking:
  --desugar-for                    desugar `for' loops
  --desugar-string-cmp             desugar string comparisons
  --desugared                      Default the removal of syntactic sugar from 
                                   the AST to Tiger (without overloading)
  --desugar                        desugar the AST
  --raw-desugar                    desugar the AST without recomputing bindings
                                   nor types
  --bounds-checks-add              add dynamic bounds checks
  --raw-bounds-checks-add          add bounds-checking to the AST without 
                                   recomputing bindings nor types

Inlining:
  --inline                         inline functions
  --inline-check                   check if functions are correctly inlined
  --prune                          prune unused functions
  --prune-check                    check if unused functions are correctly 
                                   pruned

Object:
  -o [ --object ]                  enable object extensions
  --object-parse                   parse a file, allowing objects
  --object-bindings-compute        bind the identifiers, allowing objects
  --object-types-compute           check for type violations, allowing objects
  --object-rename                  rename identifiers to unique names, allowing
                                   objects
  --object-desugar                 remove object constructs from the program
  --raw-object-desugar             remove object constructs from the program 
                                   without recomputing bindings nor types

Temporaries:
  --tempmap-display                display the temporary table

  -? [ --help ]                    Give this help list
  --usage                          Give a short usage message
  --version                        Print program version
  --license                        Print program license
$ echo $?
0



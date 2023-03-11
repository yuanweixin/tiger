# tc command line 

Taken from [EPITA website, accessed March 9, 2023](https://assignments.lrde.epita.fr/reference_manual/implementation/invoking_tc/invoking_tc.html)

TODO denote which ones are actually implemented.

## Synopsis
tc option… file
where file can be -, denoting the standard input.

Information about options supported by TC can be accessed through tc --help.

## Global options
```
-?, --help
Display the help message, and exit successfully.

--version
Display the version, and exit successfully.

--license
Display the license, and exit successfully.

--task-list
List the registered tasks.

--task-graph
Show the task graph.

--task-selection
Report the order in which the tasks will be run.
```

## Options relating to the file library (TC-1)
```
-p, --library-prepend
Prepend a directory to include path.

-P, --library-append
Append a directory to include path.

--library-display
Report the include search path.
```

## Options relating to scanning and parsing (TC-1)
```
--scan-trace
Enable RE/Flex scanners traces.

--parse-trace
Enable Bison parsers traces.

--parse
Parse the file given as argument (objects forbidden).

-o, --object
Enable object constructs of the language (class and method declarations, object creation, method calls, etc.).

--object-parse
Same as --object --parse, i.e. parse the file given as argument, allowing objects.

--prelude=prelude
Load the definitions of the file prelude before the actual argument. The result is equivalent to parsing:

let
  import "prelude"
in
  /* The argument file. */
end
To disable any prelude file, use --no-prelude. The default value is builtin, denoting the builtin prelude.

-X, --no-prelude
Don’t include prelude.
```

## Options relating to the AST (TC-2)
```
-A, --ast-display
Display the AST.

--explicit-wrapping
Explicit parenthesis wrapping in AST display.

--ast-dump
Dump the AST.

--tikz-style
Enable TikZ-style output in AST dumping.

--dot-style
Enable Graphviz-style output in AST dumping.

--clone
Clone the AST.
```

## Options relating to escapes computation (TC-3)
```
--bound
Make sure bindings (regular or taking overloading or objects constructs into account) are computed.

-b, --bindings-compute
Bind the name uses to their definitions (objects forbidden).

-B, --bindings-display
Enable the bindings display in the next --ast-display invocation. This option does not imply --bindings-compute.

--object-bindings-compute
Bind the name uses to their definitions, allowing objects.
```

## Options relating to identifiers renaming (TC-R)
```
--rename
Rename identifiers (objects forbidden).

--object-rename
Rename identifiers, allowing objects.
```

## Options relating to escapes computation (TC-E)
```
-e, --escapes-compute
Compute the escapes.

-E, --escapes-display
Enable the escape display. This option does not imply --escapes-compute, so that it is possible to check that the defaults (everybody escapes) are properly implemented. Pass -A afterward to see its result.
```

## Options relating to type checking (TC-4)
```
-T, --typed
Make sure types (regular or taking overloading or objects constructs into account) are computed.

--types-compute
Compute and check (regular) types (objects forbidden).

--object-types-compute
Compute and check types, allowing objects.

Options relating to desugaring (TC-D)
--desugar-for
Enable the translation of for loops into while loops.

--desugar-string-cmp
Enable the desugaring of string comparisons.

--desugared
Make sure syntactic sugar (regular or taking overloading into account) has been removed from the AST.

--desugar
Remove syntactic sugar from the AST. Desired translations must be enabled beforehand (e.g. with --desugar-for or --desugar-string-cmp).

--raw-desugar
Remove syntactic sugar without recomputing bindings nor types.
```

## Options relating to the inlining optimization (TC-I)
```
--inline
Inline bodies of (non overloaded) functions at call sites.

--prune
Remove unused (non overloaded) functions.
```

## Options relating to the bounds checking instrumentation (TC-B)
```
--bounds-checks-add
Add dynamic bounds checks.

--raw-bounds-checks-add
Add bounds checking to the AST without recomputing bindings nor types.

Options relating to the callgraph (TC-A)
--callgraph-compute
Build the callgraph.

--callgraph-dump
Dump the callgraph.

--parentgraph-compute
Build the parent graph.

--parentgraph-dump
Dump the parent graph.
```

## Options relating to overloading support (TC-A)
```
--overfun-bindings-compute
Binding variables, types, and breaks as usual, by bind function calls to the set of function definitions baring the same name.

-O, --overfun-types-compute
Type-check and resolve (bind) overloaded function calls. Implies --overfun-bindings-compute.
```

## Options relating to the desugaring of object constructs (TC-O)
```
--object-desugar
Translate object constructs from the program into their non object counterparts, i.e., transform a Tiger program into a Panther one.

--raw-object-desugar
Translate object constructs from the program into their non object counterparts, i.e., transform a Tiger program into a Panther one, without recomputing bindings nor types.
```

## Options relating to the combination of extensions (TC-C)
```
--c-object
Enable the object extension to combine it with others.

--c-bounds
Enable the bounds-checking extension to combine it with others.

--c-escapes
Enable the escapes extension to combine it with others.

--c-desugar
Enable the desugar extension to combine it with others.

--c-inline
Enable the inlining extension to combine it with others.

--c-prune
Enable the pruning extension to combine it with others.

--c-overload
Enable the function overloading extension to combine it with others.

-a, --c-all
Enable all extensions.

--combine-bindings-compute
Process bindings with all enabled extensions.

--combine-types-compute
Process type-checking with all enabled extensions.

--combine-rename
Process renaming with all enabled extensions.

-c, --combine-desugar
Process all enabled extensions and desugar them to core Tiger.
```

## Options relating to the high level intermediate representation (TC-5)
```
--hir-compute
Translate to HIR (objects forbidden). Implies --typed.

-H, --hir-display
Display the high level intermediate representation. Implies --hir-compute.
```

# Options relating to the LLVM IR translation (TC-L)
```
--llvm-compute
Translate to LLVM IR.

--llvm-runtime-display
Enable runtime displaying along with the LLVM IR.

--llvm-display
Display the LLVM IR.
```

## Options relating to the low level intermediate representation (TC-6)
```
--canon-trace
Trace the canonicalization of HIR to LIR.

--canon-compute
Canonicalize the LIR fragments.

-C, --canon-display
Display the canonicalized intermediate representation before basic blocks and traces computation. Implies --lir-compute. It is convenient to determine whether a failure is due to canonicalization, or traces.

--traces-trace
Trace the basic blocks and traces canonicalization of HIR to LIR.

--traces-compute
Compute the basic blocks from canonicalized HIR fragments. Implies --canon-compute.

--lir-compute
Translate to LIR. Implies --traces-compute. Actually, it is nothing but a nice looking alias for the latter.

-L, --lir-display
Display the low level intermediate representation. Implies --lir-compute.
```

## Options relating to the instruction selection (TC-7)
```
-i, --inst-compute
Convert from LIR to pseudo assembly with temporaries. Implies --lir-compute.

-I, --inst-display
Display the pseudo assembly, (without the runtime prologue). Implies --inst-compute.

-Y, --nolimips-display
Display the Nolimips assembly.

-R, --runtime-display
Display the assembly runtime prologue for the current target.

--inst-debug
Enable the verbose display of the instructions.

--rule-trace
Enable the display of the rule reducing.
```

## Options relating to the target (TC-7)
```
--targeted
Default the target to MIPS. This option is triggered by all the options that need a target.

--target-mips
Set the target to MIPS.

--target-ia32
Set the target to IA-32.

--target-arm
Set the target to ARM.

--target-display
Report information about the current target.

--callee-save=num, --caller-save=num
Set the maximum number of callee/caller save registers to num, a positive number. Note that (currently) this does not reset the current target, hence to actually change the behavior, one needs --callee-save=0 --target-mips.

--argument=num
Set the maxmum number of argument registers to num, a positive number.
```

## Options relating to the liveness analysis (TC-8)
```
-F, --flowgraphs-dump
Save each function flow graph in a Graphviz file. Implies --inst-compute.

-V, --liveness-dump
Save each function flow graph enriched with liveness information in a Graphviz file. Implies --inst-compute.

-N, --interference-dump
Save each function interference graph in a Graphviz file. Implies --inst-compute.
```

## Options relating to register allocation (TC-9)
```
--asm-coalesce-disable
Disable coalescence.

--asm-trace
Trace register allocation.

-s, --asm-compute
Allocate the registers.

-S, --asm-display
Display the final assembler, runtime included.

--tempmap-display
Display the table of temporaries.
```



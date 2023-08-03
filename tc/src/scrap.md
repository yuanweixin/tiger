
# After canonicalizing

IrExp (no Seq or Eseq)
    Const(TigerInt),
    Name(temp::Label),
    Temp(temp::Temp),
    Binop(IrBinop, Box<IrExp>, Box<IrExp>),
    Mem(Box<IrExp>),
    Call(Box<IrExp>, Vec<IrExp>),

IrStm
    Move(Box<IrExp>, Box<IrExp>),
    Exp(Box<IrExp>),
    Jump(Box<IrExp>, Vec<temp::Label>),
    Cjump(IrRelop, Box<IrExp>, Box<IrExp>, temp::Label, temp::Label),
    Label(temp::Label),

Next thing to do is to tile these things.

# list of x86 asms to be output in the tiling

taken from [cs4120 notes](https://www.cs.cornell.edu/courses/cs4120/2022sp/notes.html?id=tiles)

mov	move data from src to dest
add, sub, mul, div	arithmetic operations
inc, dec	increment or decrement a single operand (single operand)
and, or, xor, not	bitwise logical operators
shl, shr, sar	bitwise shift operators
jmp	unconditional jump
jz/je, jnz/jne	conditional jump on zero/equal or non-zero/unequal
jl, jle, jg, jge	conditional jump on numeric comparison
jb, jbe, ja, jae	conditional jump on unsigned numeric comparison
push, pop	stack operations (single operand)
test, cmp	perform ALU operations (and, sub) but only set condition codes.
call	subroutine call
ret	subroutine return

ret -> epilogue
call -> prologue

use intel syntax by default. the ATT syntax can be added later if desired, but to KISS just do intel syntax.

CONST(17) -> 17
TEMP(rax) -> rax
MEM(TEMP(rax)) -> [rax]
MEM(ADD(TEMP(rax), CONST(32)))  -> [rax + 32]
MEM(ADD(TEMP(rax), MUL(CONST(4), TEMP(rbx)))) ->[rax + rbx * 8]


<mem|imm|register> <imm|register|mem> but cannot be mem at both places.
https://web.stanford.edu/class/archive/cs/cs107/cs107.1216/guide/x86-64.html
MOVE(t, CONST)
movl $1, 0x604892         # direct (address is constant value)


MOVE(t, MEM(ADD(e, CONST))) // order
movl $1, -24(%rbp)        # indirect with displacement
                            (adoffsetdress = base %rbp + displacement -24)

MOVE(t, MEM(ADD(e2, ADD(CONST, MUL(e3, CONST))))) // account for the other order of args too...
movl $1, 8(%rsp, %rdi, 4) # indirect with displacement and scaled-index
                            (address = base %rsp + displ 8 + index %rdi * scale 4)

MOVE(t, MEM(e2, MUL(e3, CONST))) // order of args...
movl $1, (%rax, %rcx, 8) # (special case scaled-index, displ assumed 0)

MOVE(t, ADD(const, MEM(MUL(e2, CONST)))) // order of args..
movl $1, 0x8(, %rdx, 4)  # (special case scaled-index, base assumed 0)

MOVE(t, MEM(ADD(e2, e3)))
movl $1, 0x4(%rax, %rcx) # (special case scaled-index, scale assumed 1)

MOVE(t, MEM(e))
if e is t, then let t2 = munch(e), adds nothing, then Move(t, Mem(t2)) translates to mov t, [t2]
movl $1, (%rax)           # indirect (address is in register %rax)

// special patterns x CONST
// special patterns x whatever

MOVE(Mem(e), CONST)
    let t = munch(e)
    mov [t], const_val

MOVE(Mem(e), e2)
    let t = munch(e)
    let t2 = munch(e2)
    mov [t], t2

// this should catch all.
MOVE(e1, e2)
    let t1 = munch(e1) in
    let t2 = munch(e2) in
    mov t1, t2

add, sub, mul, div // ugh, dst, src and all the same combos of special casing apply
BINOP(PLUS, e1, e2)


inc
dec


# Example usage
## mov	move data from src to tdest
mov [t+t2*k+d] t2|imm

MOVE(e1, e2)
    t1 = munch(e1)
    t2 = munch(e2)
    mov t1, t2

## add, sub, mul, div	arithmetic operations
op <dst>, <src>
    t1 = munch(dst)
    t2 = munch(src)
    op t1, t2

## inc, dec	increment or decrement a single operand (single operand)
inc does not affect CF flag, add does
dec ditto

    t1 = munch(e)
    op t1

## and, or, xor, not	bitwise logical operators
and <dst>, <src>

    t1 = munch(dst)
    t2 = munch(src)
    op t1, t2

## shl, shr, sar	bitwise shift operators
    sar r/m, imm8|1|CL - signed divide by 2, this many times

    shl r/m, imm8|CL - multiply by 2 this many times

    shr r/m, imm8|CL - UNSIGNED divide by 2, this many times

## jmp	unconditional jump

    jmp r/m64


## jz/je, jnz/jne	conditional jump on zero/equal or non-zero/unequal
jz rel32 (jump near if zero, ZF=1, ZF=zero flag)
je rel32 (jump if equal, ZF=1, same as jz)
jnz/jne rel32 (jump if ZF=0)

## jl, jle, jg, jge	 SIGNED conditional jump on numeric comparison
jl rel32 (if less, SF!=OF; SF = signed flag, OF=overflow flag)
jle rel32 (if lte)

jg rel32
jge rel32

## jb, jbe, ja, jae	 UNSIGNED conditional jump on unsigned numeric comparison
jb rel32 (jump if below,CF=1, CF = carry flag)
jbe rel32 (if below or eq)
ja rel32 (if above)
jae rel32 (if above or eq)


## push, pop	stack operations (single operand)
push imm8/16/32
push r/m16,32,64
pop r/m16,64

## test, cmp	perform ALU operations (and, sub) but only set condition codes.

## lea
lea r, m

## call	subroutine call
    due to segments, 4 kinds of calls
        near call - inside same segment, eip relative or cs absolute
        far call - intersegment
        inter-privilege far call
        task switch

    call rel32 (relative)
    call r/m64 (absolute)

## ret	subroutine return

# instruction selection in real compilers (llvm, gcc) for x86

llvm models it as (very high level) map from llvm ir to mir (machine ir)

mir is the input to the backend, so instruction selection is the dividing line btw front/back end of a compiler.

global isel thing replaces earlier architecture. it operates on entire function instead of a single basic block which give it more optimization opps.

https://www.youtube.com/watch?v=rGDSKGhOiRw
constraints like register banks, register classes (subset of bank), target specific shit like cost of transferring btw register banks and legal direction of such copies, abi lowering (seems to mean calling conventions).

some sort of legalizing step where they eliminate instructions that don't work for the target and also for stuff that don't map to instructions they find some way to make it work e.g. drop-in a cpp library.

# references

https://wiki.osdev.org/X86-64_Instruction_Encoding#RIP.2FEIP-relative_addressing
covers x86 instructions

https://www.youtube.com/watch?v=rGDSKGhOiRw
llvm

http://unixwiz.net/techtips/x86-jumps.html
jumps

https://www.cs.cmu.edu/~410/doc/segments/segments.html#:~:text=Intel%20intended%20x86%20programmers%20to%20think%20of%20every,as%20being%20in%20one%20of%20four%20data%20segments.
segments

https://www.cs.uaf.edu/2012/fall/cs301/lecture/09_12_jmp.html#:~:text=You%20use%20the%20%22je%22%20instruction%20%28Jump%20if%20Equal%29.,because%20they%20both%20do%20entirely%20the%20same%20thing.
je vs jz

https://www.cs.uaf.edu/2008/fall/cs301/lecture/10_17_signed_unsigned.html
signed and unsigned jumps

https://stackoverflow.com/questions/15017659/how-to-read-the-intel-opcode-notation?answertab=trending#tab-top
explanation of the various abbreviations used in the intel doc

https://inst.eecs.berkeley.edu/~cs164/fa21/resources/Assembly-Reference.html#orgfef68e8
talks about using lea to load label into a register.

https://stackoverflow.com/questions/8997044/x86-64-linux-3-0-invalid-memory-addresses#:~:text=If%20you%20try%20to%20read%20or%20write%20memory,from%20a%20user%20mode%20application%2C%20you%27ll%20get%20%23PF.
explains what non-canonical memory address means in x86 context.

https://cs.brown.edu/courses/csci1310/2020/notes/l08.html
has useful section on addressing mode which also talks about the rip addressing patterns

https://stackoverflow.com/questions/56465415/how-do-i-push-the-equivalent-of-null-in-c-to-the-stack-in-assembly#:~:text=In%20all%20x86%20calling%20conventions%20%2F%20ABIs%2C%20the,including%20all%20x86-64%20conventions%2C%20pass%20args%20in%20registers.%29
it is safe to use value 0 if we need to return null (i.e. translate nil to 0)

https://stackoverflow.com/questions/2463150/what-is-the-fpie-option-for-position-independent-executables-in-gcc-and-ld
not very high quality, just like the cmd-fu in one of the answers

https://stackoverflow.com/questions/5469274/what-does-plt-mean-here?answertab=scoredesc#tab-top
simple explanation of plt, links to some blog about how [glibc is loaded](http://dustin.schultz.io/how-is-glibc-loaded-at-runtime.html)

https://reverseengineering.stackexchange.com/questions/1992/what-is-plt-got
link to gold author long ass writeup about linkers

https://www.technovelty.org/linux/plt-and-got-the-key-to-code-sharing-and-dynamic-libraries.html
dive into how plt works in shared libraries

https://stackoverflow.com/questions/76681795/gnu-as-escape-symbol-names-in-intel-syntax
as is a piece of shit and now forced to use the annoying att syntax. fuck att and its legacy.

https://stackoverflow.com/questions/37925143/x86-64-is-imul-faster-than-2x-shl-2x-add/37925245#37925245
contains links to a better cost model of instructions

https://cs.stackexchange.com/questions/80859/what-is-instruction-throughput-and-instruction-latency
throughput: how many cycles an instruction ties up the execution unit
latency: if B depends on A, B needs to wait this many cycles after A starts.

https://www.icsa.inf.ed.ac.uk/cgi-bin/hase/dlx-scb.pl?/depend-t.html,depend-f.html,menu.html
instruction hazards: due to the instruction taking multiple cycles
read after write :
    r3 = r1 * r2
    r5 = r3 + r2 // r3 is not ready yet!
write after write
    r3 = r1 * 3 // this finishes later and clobbers the "later" instruction's output
    r3 = r2 + 3 // this finishes first
write after read (manifests when you got instruction issued and waiting in a queue)
    r3 = r1 / r2  // takes a long time
    r4 = r3 * r5  // data dep on first instr; on some machine it could be issued before its dep is finished
    r5 = r0 + r7  // this runs independent of previous two and finishes first, result in race on r5

scoreboard: basically a hardware DAG for book keeping the instruction dependencies.

different types of hazards: data, structural, control
    data is already described above (RAW, WAW, WAR)

    structural is something like, take 2 instructions I1, I2, say I1 takes multiple clock periods. the conflict comes from both I1, I2 trying to use the same hardware unit in the same cycle.

    control: bad branch prediction, need to act like no side effects.

https://en.wikipedia.org/wiki/Hazard_(computer_architecture)
out of order execution algorithms
    scoreboarding
    tomasulo

https://en.wikipedia.org/wiki/Register_renaming
logical register maps to multiple physical registers. this avoids false data dependencies from write after write hazards. so basically independent sequences of writes to the same register can be split up by renaming the register.

https://stackoverflow.com/questions/38966919/do-complex-addressing-modes-have-extra-overhead-for-loads-from-memory
diff microarchitecture have diff perf characteristics (duh!)

https://www.agner.org/optimize/optimizing_assembly.pdf
good stuff to read, ok maybe not good for the soul but def useful, also makes more sense now that i have messed with assembly for codegen

## llvm instruction selection
### llvm papers that mention instruction selection
https://llvm.org/pubs/2008-CGO-DagISel.html
https://llvm.org/pubs/2007-04-SCOPES-ChainRulePlacement.html
https://llvm.org/pubs/2008-06-LCTES-ISelUsingSSAGraphs.html

there is a dag-based isel algorithm in use today. but there's also a new design "global isel" that wants to replace the previous. it claims it avoids the dag representation and directly goes to machine ir (mir), and can operate at the function level, instead of only at basic block level.https://www.llvm.org/docs/GlobalISel/index.html#id3

selection dag doc
https://llvm.org/docs/CodeGenerator.html#instruction-selection-section
this thing in theory can read from a text file that specifies the machine instruction details (such as dag pattern that instruction matches, the use/defs, and a bunch of target specific parameters) then somehow there is a tablegen tool that would give you the code to manipulate these structure, therefore reducing your need to write more custom c++.

global isel
https://www.llvm.org/docs/GlobalISel/index.html#id3

so llvm's isel is more sophisticated than the tree-based one implemented here. the tree-based one can only work on one IR-thing at a time, not even basic blocks.

# Questions

is the [t1 + t2 * k + c] faster than a regular multiplication due to the restriction in the value of k to {1,2,4,8}?

# Other

The use of different physical registers for the same logical register
enables the CPU to make the last three instructions in example 9.1b independent of the first
three instructions. The CPU must have a lot of physical registers for this mechanism to work
efficiently. The number of physical registers is different for different microprocessors, but
you can generally assume that the number is sufficient for quite a lot of instruction
reordering.
Partial registers
Some CPUs can keep different parts of a register separate, while other CPUs always treat a
register as a whole. If we change example 9.1b so that the second part uses 16-bit registers
then we have the problem of a false dependence:

; Example 9.1c, False dependence of partial register
mov eax, [mem1] ; 32 bit memory operand
imul eax, 6
mov [mem2], eax
mov ax, [mem3] ; 16 bit memory operand
add ax, 2
mov [mem4], ax

Here the instruction mov ax,[mem3] changes only the lower 16 bits of register eax, while
the upper 16 bits retain the value they got from the imul instruction. Some CPUs from both
Intel, AMD and VIA are unable to rename a partial register. The consequence is that the mov
ax,[mem3] instruction has to wait for the imul instruction to finish because it needs to
combine the 16 lower bits from [mem3] with the 16 upper bits from the imul instruction.
Other CPUs are able to split the register into parts in order to avoid the false dependence,
but this has another disadvantage in case the two parts have to be joined together again.
Assume, for example, that the code in example 9.1c is followed by PUSH EAX. On some
processors, this instruction has to wait for the two parts of EAX to retire in order to join them
together, at the cost of 5-6 clock cycles. Other processors will generate an extra μop for
joining the two parts of the register together.

These problems are avoided by replacing mov ax,[mem3] with movzx eax,[mem3]. This
resets the high bits of eax and breaks the dependence on any previous value of eax. In 64-
bit mode, it is sufficient to write to the 32-bit register because this always resets the upper
part of a 64-bit register. Thus, movzx eax,[mem3] and movzx rax,[mem3] are doing
exactly the same thing. The 32-bit version of the instruction is one byte shorter than the 64-
bit version. *Any use of the high 8-bit registers AH, BH, CH, DH should be avoided because it
can cause false dependences and less efficient code.*

The flags register can cause similar problems for instructions that modify some of the flag
bits and leave other bits unchanged. For example, the INC and DEC instructions leave the
carry flag unchanged but modifies the zero and sign flags


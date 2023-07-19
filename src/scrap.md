
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
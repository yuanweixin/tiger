// TODO this seems unnecessary.
#[allow(dead_code)]
pub enum ReturnCode {
    Ok = 0,

    OtherErrors = 1,

    SyntaxError = 2,

    /// Identifier binding errors such as duplicate name definition, or undefined name use.
    BindingError = 3,

    /// Type checking errors (such as type incompatibility).
    TypeError = 4,

    /// The command was used incorrectly, e.g., with the wrong number of arguments, a bad flag, a bad syntax in a parameter, etc…
    ExUsage = 64,

    /// In addition to compiler errors, the compiled programs may have to raise a runtime error, for instance when runtime functions received improper arguments. In that case use the exit code 120, and issue a clear diagnostic. For MIPS, which does not provide the standard error output, the message is to be output onto the standard output.
    RuntimeError = 120
}

pub fn exit(rc: ReturnCode) {
    std::process::exit(rc as i32);
}
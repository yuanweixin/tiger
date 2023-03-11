/// Compile errors must be reported on the standard error stream with precise error location. The error output must exactly follow the format specified below where location includes the file name, initial position, and final position.
/// 
/// location: error message
/// 
/// Example error messages include 
/// ```
/// $ echo "1 + + 2" | ./tc -
/// error→standard input:1.4: syntax error, unexpected "+"
/// error→Parsing Failed
/// ```
/// and
/// 
/// ```
/// $ echo "1 + () + 2" | ./tc -T -
/// error→standard input:1.0-5: type mismatch
/// error→  right operand type: void
/// error→  expected type: int
/// ```
pub enum ReturnCode {
    /// Everything is all right.
    /// The exit status and the standard error output must be consistent: the exit status is 0 if and only if there is no output at all on the standard error output. There are actually some exceptions: when tracing is enabled (such as during scanning, parsing, etc.)
    Ok = 0,

    /// Some error which does not fall into the other categories occurred. For instance, malloc or fopen failed, a file is missing, etc…
    /// An unsupported option must cause tc to exit with 64 (EX_USAGE) even if related to a stage option otherwise these optional features will be tested, and it will most probably have 0. For instance, a TC-5 delivery that does not support bounds checking must not accept --bounds-checking.
    OtherErrors = 1,

    /// Error detected during the scanning, e.g., invalid character.
    ScannerError = 2,

    /// Parse error
    ParseError = 3,

    /// Identifier binding errors such as duplicate name definition, or undefined name use.
    BindingError = 4,

    /// Type checking errors (such as type incompatibility).
    TypeError = 5,

    /// The command was used incorrectly, e.g., with the wrong number of arguments, a bad flag, a bad syntax in a parameter, etc…
    ExUsage = 64,

    /// In addition to compiler errors, the compiled programs may have to raise a runtime error, for instance when runtime functions received improper arguments. In that case use the exit code 120, and issue a clear diagnostic. For MIPS, which does not provide the standard error output, the message is to be output onto the standard output.
    RuntimeError = 120
}

pub fn exit(rc: ReturnCode) {
    std::process::exit(rc as i32);
}
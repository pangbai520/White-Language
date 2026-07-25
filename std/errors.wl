@CompilerLink
enum Error {
    None,
    Unknown,
    OutOfMemory,
    NotSupported,
    Interrupted,
    EndOfFile,
    WriteZero,
    InvalidArgument,
    OutOfBounds,
    TypeMismatch,
    IllegalState,

    DivisionByZero,
    Overflow,
    Underflow,

    FileNotFound,
    PermissionDenied,
    AlreadyExists,
    BrokenPipe,
    DiskFull,
    NotFound,
}

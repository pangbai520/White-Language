@CompilerLink
enum Error {
    None,
    Unknown,
    OutOfMemory,
    NotSupported,
    Interrupted,
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
}
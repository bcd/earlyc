
The following is a summarized change history of the C compiler evolution as seen in the files here.

# General Comments

* Assignment operators are written as =+ instead of +=, etc.
* No hexadecimal integers allowed in the earliest versions.
* Maximum 8 bytes per symbol name.
* Multiple ASCII characters can be combined into a single constant, e.g. 'xy'.  This allows packing two 8-bit characters into one 16-bit word.
* Machine-specific code is freely mixed by the machine independent code.  Some knowledge of PDP-11 assembler is helpful.
* Line numbers are tracked during scanning for use in error messages.
* Keep in mind that early PDP-11s have a maximum of 56KB RAM for the kernel and user code.  The earliest systems may have even less.  Later, split I+D space would essentially double this.

# V2 UNIX

* Runs on the PDP-11/20 without memory mapping hardware.
* Trees are allocated from the first 500 bytes of the program area, overwriting its initialization code.
* Type keywords: char, int, float, double
* Storage keywords: auto, extern, static
* Control flow keywords: goto, return, if, while, else, switch, case, break, continue, do, default.

# V3 UNIX

* Trees are now allocated starting from address 0, indicating the availability of memory mapping hardware.
* Fixed off-by-one error in detecting symbol table overflow.
* Added default initialization to zero, when an initial value is not given.  This saves a couple of bytes
  per data declaration that can be zero-initialized.
* Changed the hashing logic for switch statements; pssiz increased from 8 to 9.
* Added the struct keyword, replacing a commented-out slot for long.  Added the . and -> operators.  Note the compiler itself is not using struct yet, hence this is the "prestruct-c" branch.  Struct sizes are always rounded up to an even/word boundary.  Struct tags are in the same namespace as everything else; x->a is essentially just turned into x[a], replacing the name with an offset.
* error() argument count expanded from 2 to 6, to allow more context to be written out.
* Added many more calls to error().
* Added debug flag that, when set, dumps the symbol table at the end of every declaration.  This can be set at compile time.
* Support signed decimal constants; e.g. 0177776 was changed to -2.
* Removed the 8th bit from some characters before passing to putchar().
* Added the bitwise complement operator ~.
* Added the register keyword.  A maximum of 3 register variables per function are supported (r2, r3, and r4).  Exceeding that generates a compiler error.  r0 and r1 are always reserved by the implementation for temporaries; if register variables are not defined, it may use r2-r4 also.  r5-r7 are system defined, for the frame pointer, stack pointer, and program counter.
* Use double within the compiler itself, when working with floating-point constants.
* Code generator routine printn() split into two, printo() for octals and printd() for decimals.  Each specialization is more optimized than the general version where the base is a variable, at the cost of slightly more code.
* Added "%c" formatter to printf().
* Moved scanning of numbers into an assembler routine, getnum().
* Implicit function declaration allowed.  It is no longer required to declare "extern foo;" for a function foo().

# V5 UNIX

* Note that the compiler itself is using struct now.
* Moved tree allocation onto the program stack.  (Memory mapping hardware will cause a trap when the stack needs to be expanded.)
* Buffered I/O functions, similar to stdio, used instead of system calls to read/write files.  For example, creat becomes fcreat, and open becomes fopen.  The FILE object is not explicitly present; instead, 518 byte buffers are used.  Presumably the first few words are equivalent to FILE, followed by a data buffer.
* Signed octal constants supported, e.g. -0777.
* Added sizeof keyword.  This is evaluated early during parsing and replaced with an integer constant.
* Added && and || operators, different from & and |.
* Keyword initialization rewritten to use a keyword table instead of init() calls.
* Return types added to many functions that return pointers.
* Many constants replaced by preprocessor names.  The preprocessor itself used at this point is not included, but it seems to only support simple substitutions via #define.
* Support \0 for the NUL character; used by the compiler itself.
* Support \xxx octal character constants.
* Added the for() statement.
* Defer code generation for switch statements until pass 2; add various strategies to evalulate them.
* Utilize the PDP-11 "nand" instruction.
* Code optimizer uses alloc() for heap allocation, using sbrk.
* Added a third pass to optimize the code generated in pass 2.  This re-reads the assembler code, one function at a time, and performs peephole optimization.

# V7 UNIX

* Added keywords: long, unsigned, union, short, typedef, enum.  This essentially completes what would become known as K&R C.  The rumored "entry" keyword is not present.
* When register specified but no registers available, fall back to "auto" storage.
* FILE pointers are used for writing to stdout and stderr.  The buffering is now transparent.

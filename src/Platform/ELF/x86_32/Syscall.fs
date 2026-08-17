module PointerAnalyzer.Platform.ELF.X86_32.Syscall

open B2R2
open B2R2.FrontEnd
open PointerAnalyzer.Platform.PlatformTypes

let private regId register = Intel.Register.toRegID register

let private eax = regId Intel.Register.EAX
let private ebx = regId Intel.Register.EBX
let private ecx = regId Intel.Register.ECX
let private edx = regId Intel.Register.EDX
let private esi = regId Intel.Register.ESI
let private edi = regId Intel.Register.EDI
let private ebp = regId Intel.Register.EBP

let private signature name arguments returns isNoReturn =
  { Name = name
    Arguments = Map.ofList arguments
    Returns = Map.ofList returns
    IsNoReturn = isNoReturn }

let private valueReturn = [ eax, SyscallValue ]
let private addressReturn = [ eax, SyscallAddress ]

/// Linux i386 syscall signatures used to map register types at int 0x80.
let private signatures =
  [ 1UL, signature "exit" [ ebx, SyscallValue ] [] true
    2UL, signature "fork" [] valueReturn false
    3UL,
    signature
      "read"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    4UL,
    signature
      "write"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    5UL,
    signature
      "open"
      [ ebx, SyscallAddress; ecx, SyscallValue; edx, SyscallValue ]
      valueReturn
      false
    6UL, signature "close" [ ebx, SyscallValue ] valueReturn false
    7UL,
    signature
      "waitpid"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    8UL,
    signature
      "creat"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    9UL,
    signature
      "link"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    10UL, signature "unlink" [ ebx, SyscallAddress ] valueReturn false
    11UL,
    signature
      "execve"
      [ ebx, SyscallAddress; ecx, SyscallAddress; edx, SyscallAddress ]
      valueReturn
      false
    12UL, signature "chdir" [ ebx, SyscallAddress ] valueReturn false
    13UL, signature "time" [ ebx, SyscallAddress ] valueReturn false
    15UL,
    signature
      "chmod"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    19UL,
    signature
      "lseek"
      [ ebx, SyscallValue; ecx, SyscallValue; edx, SyscallValue ]
      valueReturn
      false
    20UL, signature "getpid" [] valueReturn false
    33UL,
    signature
      "access"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    37UL,
    signature "kill" [ ebx, SyscallValue; ecx, SyscallValue ] valueReturn false
    38UL,
    signature
      "rename"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    39UL,
    signature
      "mkdir"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    40UL, signature "rmdir" [ ebx, SyscallAddress ] valueReturn false
    41UL, signature "dup" [ ebx, SyscallValue ] valueReturn false
    42UL, signature "pipe" [ ebx, SyscallAddress ] valueReturn false
    45UL, signature "brk" [ ebx, SyscallAddress ] addressReturn false
    54UL,
    signature
      "ioctl"
      [ ebx, SyscallValue; ecx, SyscallValue; edx, SyscallUnknown ]
      valueReturn
      false
    55UL,
    signature
      "fcntl"
      [ ebx, SyscallValue; ecx, SyscallValue; edx, SyscallUnknown ]
      valueReturn
      false
    63UL,
    signature "dup2" [ ebx, SyscallValue; ecx, SyscallValue ] valueReturn false
    78UL,
    signature
      "gettimeofday"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    82UL,
    signature
      "select"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallAddress
        esi, SyscallAddress
        edi, SyscallAddress ]
      valueReturn
      false
    85UL,
    signature
      "readlink"
      [ ebx, SyscallAddress; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    90UL, signature "mmap" [ ebx, SyscallAddress ] addressReturn false
    91UL,
    signature
      "munmap"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    102UL,
    signature
      "socketcall"
      [ ebx, SyscallValue; ecx, SyscallAddress ]
      valueReturn
      false
    106UL,
    signature
      "stat"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    107UL,
    signature
      "lstat"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    108UL,
    signature
      "fstat"
      [ ebx, SyscallValue; ecx, SyscallAddress ]
      valueReturn
      false
    114UL,
    signature
      "wait4"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallValue
        esi, SyscallAddress ]
      valueReturn
      false
    120UL,
    signature
      "clone"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallAddress
        esi, SyscallAddress
        edi, SyscallAddress ]
      valueReturn
      false
    122UL, signature "uname" [ ebx, SyscallAddress ] valueReturn false
    125UL,
    signature
      "mprotect"
      [ ebx, SyscallAddress; ecx, SyscallValue; edx, SyscallValue ]
      valueReturn
      false
    140UL,
    signature
      "_llseek"
      [ ebx, SyscallValue
        ecx, SyscallValue
        edx, SyscallValue
        esi, SyscallAddress
        edi, SyscallValue ]
      valueReturn
      false
    141UL,
    signature
      "getdents"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    145UL,
    signature
      "readv"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    146UL,
    signature
      "writev"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    162UL,
    signature
      "nanosleep"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    168UL,
    signature
      "poll"
      [ ebx, SyscallAddress; ecx, SyscallValue; edx, SyscallValue ]
      valueReturn
      false
    174UL,
    signature
      "rt_sigaction"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallAddress
        esi, SyscallValue ]
      valueReturn
      false
    175UL,
    signature
      "rt_sigprocmask"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallAddress
        esi, SyscallValue ]
      valueReturn
      false
    180UL,
    signature
      "pread64"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallValue
        esi, SyscallValue
        edi, SyscallValue ]
      valueReturn
      false
    181UL,
    signature
      "pwrite64"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallValue
        esi, SyscallValue
        edi, SyscallValue ]
      valueReturn
      false
    183UL,
    signature
      "getcwd"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    192UL,
    signature
      "mmap2"
      [ ebx, SyscallAddress
        ecx, SyscallValue
        edx, SyscallValue
        esi, SyscallValue
        edi, SyscallValue
        ebp, SyscallValue ]
      addressReturn
      false
    195UL,
    signature
      "stat64"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    196UL,
    signature
      "lstat64"
      [ ebx, SyscallAddress; ecx, SyscallAddress ]
      valueReturn
      false
    197UL,
    signature
      "fstat64"
      [ ebx, SyscallValue; ecx, SyscallAddress ]
      valueReturn
      false
    220UL,
    signature
      "getdents64"
      [ ebx, SyscallValue; ecx, SyscallAddress; edx, SyscallValue ]
      valueReturn
      false
    221UL,
    signature
      "fcntl64"
      [ ebx, SyscallValue; ecx, SyscallValue; edx, SyscallUnknown ]
      valueReturn
      false
    240UL,
    signature
      "futex"
      [ ebx, SyscallAddress
        ecx, SyscallValue
        edx, SyscallValue
        esi, SyscallAddress
        edi, SyscallAddress
        ebp, SyscallValue ]
      valueReturn
      false
    252UL, signature "exit_group" [ ebx, SyscallValue ] [] true
    258UL, signature "set_tid_address" [ ebx, SyscallAddress ] valueReturn false
    265UL,
    signature
      "clock_gettime"
      [ ebx, SyscallValue; ecx, SyscallAddress ]
      valueReturn
      false
    295UL,
    signature
      "openat"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallValue
        esi, SyscallValue ]
      valueReturn
      false
    300UL,
    signature
      "fstatat64"
      [ ebx, SyscallValue
        ecx, SyscallAddress
        edx, SyscallAddress
        esi, SyscallValue ]
      valueReturn
      false
    311UL,
    signature
      "set_robust_list"
      [ ebx, SyscallAddress; ecx, SyscallValue ]
      valueReturn
      false
    340UL,
    signature
      "prlimit64"
      [ ebx, SyscallValue
        ecx, SyscallValue
        edx, SyscallAddress
        esi, SyscallAddress ]
      valueReturn
      false
    355UL,
    signature
      "getrandom"
      [ ebx, SyscallAddress; ecx, SyscallValue; edx, SyscallValue ]
      valueReturn
      false ]
  |> Map.ofList

let create () =
  { NumberRegister = eax
    ClobberedRegisters = Set.ofList [ eax; ecx; edx ]
    TryFindSignature = fun number -> Map.tryFind number signatures }

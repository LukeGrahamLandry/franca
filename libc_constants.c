//! Alas, if I want to target a new thing,
//! it's more likely that it spawns from the
//! ether with a C compiler than a Rust compiler.

#include <assert.h>
#include <dirent.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
// #include <sys/dirent.h>
#include <sys/mman.h>
#include <sys/signal.h>
// #include <sys/syslimits.h>
// #include <sys/unistd.h>
#include <pthread.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#include <sys/utsname.h>

int main() {
  // TODO: this should be a bitflag. presumably PROT_NONE is always 0.
  //       distinguish between flags whose value is shifting (i.e. 0/1/2 vs
  //       1/2/4)
  printf("// START @generated by libc_constants.c\n");
  printf("OpenFlag :: @enum(i64) (Read = %d, Write = %d, ReadWrite = %d, "
         "Create = %d, Truncate = %d, Append = %d);\n",
         O_RDONLY, O_WRONLY, O_RDWR, O_CREAT, O_TRUNC, O_APPEND);
  printf("MapProt :: @enum(i64) (Exec = %d, Read = %d, Write = %d);\n",
         PROT_EXEC, PROT_READ, PROT_WRITE);
  printf("MapFlag :: @enum(i64) (Private = %d, Anonymous = %d, Jit = %d, Fixed = %d);\n",
         MAP_PRIVATE, MAP_ANONYMOUS, MAP_JIT, MAP_FIXED);
  printf("Whence :: @enum(i64) (Set = %d, Cur = %d, End = %d);\n", SEEK_SET,
         SEEK_CUR, SEEK_END);
  printf("DlFlag :: @enum(i64) (Lazy = %d, Now = %d);\n", RTLD_LAZY, RTLD_NOW);

  printf("DirEntType :: @enum(u8) (Directory = %d, File = %d, "
         "SymbolicLink "
         "= %d);\n",
         DT_DIR, DT_REG, DT_LNK);

  printf("TermConstants :: @struct(_parsehack: void, $ICANON := %d, $ "
         "ECHO := "
         "%d, $VMIN := %d, $VTIME := %d, $ TCSANOW := %d, $"
         "TCSADRAIN := %d);\n",
         ICANON, ECHO, VMIN, VTIME, TCSANOW, TCSADRAIN);
  printf("AccessMode :: @enum(i64) (Exists = %d, Readable = %d, Writable = %d, "
         "Executable = "
         "%d);\n",
         F_OK, R_OK, W_OK, X_OK);
  printf("WaitPidOptions :: @enum(i64) (NoHang = %d);\n", WNOHANG);
  printf("PollEvents :: @enum(i64) (In = %d);\n", POLLIN);
  printf("SignalNum :: @enum(i32) (Interupt = %d, Quit = %d, "
         "IllegalInstruction = %d, TraceTrap = %d, Abort = %d, Kill = %d, Bus "
         "= %d, Segfault = %d);\n",
         SIGINT, SIGQUIT, SIGILL, SIGTRAP, SIGABRT, SIGKILL, SIGBUS, SIGSEGV);
  printf("SignalFlag :: @enum(i32) (Info = %d);\n", SA_SIGINFO);
  printf("FileMode :: @enum(u16) (\n"
        "    OwnerAll = %d, OwnerRead = %d, OwnerWrite = %d, OwnerExecute = %d, \n"
        "    GroupAll = %d, GroupRead = %d, GroupWrite = %d, GroupExecute = %d, \n"
        "    OthersAll = %d, OthersRead = %d, OthersWrite = %d, OthersExecute = %d\n"
        "    AnyoneExecute = %d,\n"
        ");\n", 
        S_IRWXU, S_IRUSR, S_IWUSR, S_IXUSR,
        S_IRWXG, S_IRGRP, S_IWGRP, S_IXGRP,
        S_IRWXO, S_IROTH, S_IWOTH, S_IXOTH,
        S_IXUSR | S_IXGRP | S_IXOTH
        // TODO: S_ISUID S_ISGID S_ISVTX
  );
  printf("ClockId :: @enum(i64) (ThreadCpuTime = %d, RealTime = %d);\n", CLOCK_THREAD_CPUTIME_ID, CLOCK_REALTIME);
  
  printf("// END @generated by libc_constants.c\n");
}

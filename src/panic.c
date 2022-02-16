#include "rsm.h"

#ifdef R_WITH_LIBC
  #include <stdlib.h> // free
  #include <unistd.h> // STDERR_FILENO
  #include <execinfo.h> // backtrace*
#endif // R_WITH_LIBC

NORETURN void _rpanic(const char* file, int line, const char* fun, const char* fmt, ...) {
  #ifdef R_WITH_LIBC
    FILE* fp = stderr;
    flockfile(fp);
    fprintf(fp, "\npanic: ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(fp, fmt, ap);
    va_end(ap);
    fprintf(fp, " in %s at %s:%d\n", fun, file, line);
    void* buf[32];
    int framecount = backtrace(buf, countof(buf));
    if (framecount > 1) {
      char** strs = backtrace_symbols(buf, framecount);
      if (strs != NULL) {
        for (int i = 1; i < framecount; ++i) {
          fwrite(strs[i], strlen(strs[i]), 1, fp);
          fputc('\n', fp);
        }
        free(strs);
      } else {
        fflush(fp);
        backtrace_symbols_fd(buf, framecount, fileno(fp));
      }
    }
    funlockfile(fp);
    fflush(fp);
    fsync(STDERR_FILENO);
  #else
    log("panic");
  #endif
  abort();
}

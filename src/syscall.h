// system calls
// SPDX-License-Identifier: Apache-2.0
#pragma once

#define RSM_FOREACH_SYSCALL(_) /* _(name, code, args, description) */ \
_( SC_EXIT,  0, "status i32", "exit program" )\
_( SC_SLEEP, 1, "nsec u64", "sleep for up to nsec; returns remaining time or error" )\
\
_( SC_TEXIT, _SC_MAX, "", "exit task" )\
// end RSM_FOREACH_SYSCALL

enum syscall_op {
  _SC_MAX = RSM_MAX_Au,
  #define _(name, code, ...) name = code,
  RSM_FOREACH_SYSCALL(_)
  #undef _
};

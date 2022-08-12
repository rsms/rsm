// virtual machine
// SPDX-License-Identifier: Apache-2.0
#pragma once
#include "sched.h"
RSM_ASSUME_NONNULL_BEGIN

// rmachine_t: state of an entire virtual machine instance
struct rmachine_ {
  rmm_t*       mm;     // memory manager
  rmemalloc_t* malloc; // memory allocator
  rsched_t     sched;  // scheduler
};

RSM_ASSUME_NONNULL_END

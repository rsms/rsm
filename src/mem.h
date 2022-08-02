// Memory API
// SPDX-License-Identifier: Apache-2.0
#pragma once
RSM_ASSUME_NONNULL_BEGIN

// --- moved to rsm.h ---

// DEPRECATED (use rmemalloc_t); TODO remove
inline static void* nullable rmm_allocpages_bytes(rmm_t* mm, usize minsize) {
  return rmm_allocpages(mm, ALIGN2(minsize, PAGE_SIZE) / PAGE_SIZE);
}

//———————————————————————————————————————————————————————————————————————————————————————
RSM_ASSUME_NONNULL_END

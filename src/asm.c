// assembler internals, shared by all asm*.c files and rom_build
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "asm.h"
#include "abuf.h"

#ifndef RSM_NO_ASM

const char* tokname(rtok t) {
  switch (t) {
  #define _(name, ...) case name: return &#name[3]; // [3] to skip "RT_" prefix
  RSM_FOREACH_TOKEN(_)
  RSM_FOREACH_BINOP_TOKEN(_)
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  }
  return "?";
}

rnode_t* nullable nlastchild(rnode_t* n) {
  rnode_t* child = n->children.head;
  if (child) while (child->next)
    child = child->next;
  return child;
}

static u32 u32log10(u32 u) {
  return u >= 1000000000 ? 10 :
         u >= 100000000 ? 9 :
         u >= 10000000 ? 8 :
         u >= 1000000 ? 7 :
         u >= 100000 ? 6 :
         u >= 10000 ? 5 :
         u >= 1000 ? 4 :
         u >= 100 ? 3 :
         u >= 10 ? 2 :
         1;
}

// nposrange computes the source position range for AST node n
rposrange_t nposrange(rnode_t* n) {
  rposrange_t pr = { .start=n->pos, .focus=n->pos };
  switch (n->t) {
    case RT_ASSIGN:
      if (n->children.head) {
        pr.start = n->children.head->pos;
        rnode_t* lastn = n->children.head;
        while (lastn->next)
          lastn = lastn->next;
        rposrange_t lastpr = nposrange(lastn);
        lastpr.end = lastpr.end.line ? lastpr.end : lastpr.focus;
        pr.end = pr.focus;
        if (lastpr.end.line >= pr.end.line && lastpr.end.col > pr.end.col)
          pr.end = lastpr.end;
      }
      break;
    case RT_SINTLIT2:
    case RT_INTLIT2:
      pr.end.line = pr.focus.line;
      pr.end.col = pr.focus.col + ILOG2(n->ival) + 3;
      break;
    case RT_SINTLIT:
    case RT_INTLIT:
      pr.end.line = pr.focus.line;
      pr.end.col = pr.focus.col + u32log10(n->ival);
      break;
    case RT_SINTLIT16:
    case RT_INTLIT16:
      pr.end.line = pr.focus.line;
      pr.end.col = pr.focus.col + ((u32)ILOG2(n->ival) >> 2) + 3;
      break;
    case RT_CONST:
      pr.end.line = pr.focus.line;
      pr.end.col = pr.focus.col + 5 + 1 + n->sval.len; // assume single space sep
      if (n->children.head) {
        rsrcpos_t endpos = nposrange(n->children.head).end;
        if (endpos.line) // note: inferred type has no srcpos
          pr.end = endpos;
      }
      break;
    case RT_DATA:
      if (n->children.head) {
        rnode_t* type = n->children.head;
        pr.end = nposrange(type).end;
      } else {
        pr.end.line = pr.focus.line;
        pr.end.col = pr.focus.col + 4;
      }
      break;
    case RT_I1:
    case RT_I8:
    case RT_I16:
    case RT_I32:
    case RT_I64:
      pr.end.line = pr.focus.line;
      pr.end.col = pr.focus.col;
      switch (n->t) {
        case RT_I1:  pr.end.col += strlen("i1"); break;
        case RT_I8:  pr.end.col += strlen("i8"); break;
        case RT_I16: pr.end.col += strlen("i16"); break;
        case RT_I32: pr.end.col += strlen("i32"); break;
        case RT_I64: pr.end.col += strlen("i64"); break;
      }
      break;
    default: if (tokhasname(n->t) || n->t == RT_STRLIT) {
      pr.end.line = pr.focus.line;
      pr.end.col = pr.focus.col + n->sval.len;
    }
  }
  return pr;
}

static void diag_add_srclines(rasm_t* a, rposrange_t pr, abuf_t* s) {
  a->diag.srclines = "";
  if (abuf_avail(s) < 4 || pr.focus.line == 0 || a->srclen == 0)
    return;

  rsrcpos_t pos = pr.focus; // TODO: use start & end

  u32 nlinesbefore = 1;
  u32 nlinesafter = 1;
  u32 startline = pos.line - MIN(pos.line - 1, nlinesbefore);
  u32 endline = pos.line + nlinesafter + 1;
  u32 line = startline;

  const char* start = a->srcdata, *end = start; // start & end of line
  const char* srcend = start + a->srclen;

  // forward to startline
  for (;;) {
    if (*end == '\n') {
      if (--startline == 0) break; // found
      start = end + 1;
    }
    end++;
    if (end == srcend) {
      if (--startline == 0) break; // no trailing LF
      return; // not found
    }
  }

  a->diag.srclines = s->p;
  int ndigits = u32log10(endline);

  for (;;) {
    int len = (int)(end - start);
    if (line != pos.line) {
      abuf_fmt(s, "%*u   │ %.*s", ndigits, line, len, start);
    } else if (pr.end.line) {
      assert(pr.start.line > 0);
      assert(pr.start.col > 0);
      assert(pr.focus.col > 0);
      assert(pr.end.col > 0);
      abuf_fmt(s, "%*u → │ %.*s\n"
                  "%*s   │ %*s",
        ndigits, line, len, start, ndigits, "", (int)(pr.start.col-1), "");
      u32 c = pr.start.col - 1;
      if (pr.focus.col == pr.start.col || pr.focus.col == pr.end.col) {
        // focus point is at either start or end extremes; just draw a line
        while (c++ < pr.end.col-1)
         abuf_str(s, "~");
      } else {
        while (c++ < pr.focus.col-1)
          abuf_str(s, "~");
        abuf_str(s, "↑");
        for (; c < pr.end.col-1; c++)
          abuf_str(s, "~");
      }
    } else {
      abuf_fmt(s, "%*u → │ %.*s\n"
                  "%*s   │ %*s↑",
        ndigits, line, len, start, ndigits, "", (int)pos.col - 1, "");
    }

    if (end == srcend || ++line == endline)
      break;
    abuf_c(s, '\n');
    // find next line
    start = ++end;
    while (end != srcend) {
      if (*end == '\n')
        break;
      end++;
    }
  }
}

void reportv(rasm_t* a, rposrange_t pr, int code, const char* fmt, va_list ap) {
  if (rasm_stop(a))
    return; // previous call to diaghandler has asked us to stop

  char msgbuf[4096];
  msgbuf[0] = 0;

  a->diag.code = code;
  a->diag.msg = msgbuf;
  a->diag.srcname = a->srcname;
  a->diag.line = pr.focus.line;
  a->diag.col = pr.focus.col;

  if (code)
    a->errcount++;

  abuf_t s = abuf_make(msgbuf, sizeof(msgbuf));
  if (pr.focus.line > 0) {
    abuf_fmt(&s, "%s:%u:%u: ", a->srcname, pr.focus.line, pr.focus.col);
  } else {
    abuf_fmt(&s, "%s: ", a->srcname);
  }
  abuf_str(&s, code ? "error: " : "warning: ");
  a->diag.msgshort = s.p;
  abuf_fmtv(&s, fmt, ap);
  abuf_c(&s, '\0'); // separate message from srclines
  diag_add_srclines(a, pr, &s);
  abuf_terminate(&s);

  bool keepgoing = a->diaghandler(&a->diag, a->userdata);
  rasm_stop_set(a, !keepgoing);
}

void errf(rasm_t* a, rposrange_t pr, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reportv(a, pr, 1, fmt, ap);
  va_end(ap);
}

void warnf(rasm_t* a, rposrange_t pr, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reportv(a, pr, 0, fmt, ap);
  va_end(ap);
}

void gstate_dispose(gstate* g);
void pstate_dispose(pstate* p);

void rasm_dispose(rasm_t* a) {
  gstate* g = rasm_gstate(a);
  if (g) gstate_dispose(g);

  pstate* p = rasm_pstate(a);
  if (p) pstate_dispose(p);

  #ifdef DEBUG
  memset(a, 0, sizeof(rasm_t));
  #endif
}

rerror rsm_loadfile(const char* filename, rmem_t* data_out) {
  return mmapfile(filename, data_out);
}

void rsm_unloadfile(rmem_t m) {
  unmapfile(m);
}

// ————————————————————————————————————————
// bufslab

static void bufslab_remove(bufslab* s) {
  assert(s->prev != s);
  assert(s->next != s);
  assert(s->prev != s->next);
  if (s->prev) s->prev->next = s->next;
  if (s->next) s->next->prev = s->prev;
  #if DEBUG
  s->prev = NULL;
  s->next = NULL;
  #endif
}

static bufslab* bufslab_insertafter(bufslab* s, bufslab* prev) {
  assert(prev != s);
  assert(s->next != prev);
  s->prev = prev;
  s->next = prev->next;
  prev->next = s;
  return s;
}

static bufslab* bufslab_insertbefore(bufslab* s, bufslab* next) {
  assert(next != s);
  assert(s->prev != next);
  s->next = next;
  s->prev = next->prev;
  next->prev = s;
  return s;
}

static void* nullable bufslab_alloc_more(bufslabs* slabs, rmemalloc_t* ma, u32 len) {
  // try to find a free slab of adequate size:
  bufslab* free = slabs->tail->next;
  for (; free; free = free->next) {
    if (free->cap < len)
      continue;
    bufslab_remove(free);
    if (len >= (BUFSLAB_MIN_CAP/4)*3) {
      // immediately retire the slab when the allocation is 3/4ths of a slab's capacity
      slabs->head = bufslab_insertbefore(free, slabs->head);
      free->len = len;
      return free->data;
    }
    goto finalize;
  }
  // no free space found -- allocate new slab
  u32 cap = MAX(BUFSLAB_MIN_CAP, ALIGN2(len, 16));
  rmem_t m = rmem_alloc_aligned(ma, sizeof(bufslab) + cap, _Alignof(bufslab));
  if UNLIKELY(!m.p)
    return NULL;
  free = m.p;
  free->cap = m.size - sizeof(bufslab);
finalize:
  // retire slabs->tail and make the free slab the new "partial" slabs->tail
  slabs->tail = bufslab_insertafter(free, slabs->tail);
  free->len = len;
  return free->data;
}

void* nullable bufslab_alloc(bufslabs* slabs, rmemalloc_t* ma, usize nbyte) {
  if UNLIKELY(slabs->tail->cap - slabs->tail->len < nbyte)
    return bufslab_alloc_more(slabs, ma, nbyte);
  void* ptr = slabs->tail->data + slabs->tail->len;
  slabs->tail->len += nbyte;
  return ptr;
}

void bufslabs_reset(bufslabs* slabs) {
  for (bufslab* s = slabs->head; s; s = s->next)
    s->len = 0;
  slabs->tail = slabs->head;
}

void bufslab_freerest(bufslab* s, rmemalloc_t* ma) {
  s = s->next;
  while (s) {
    bufslab* tmp = s->next;
    rmem_free(ma, RMEM(s, sizeof(bufslab) + s->cap));
    s = tmp;
  }
}


#endif // RSM_NO_ASM

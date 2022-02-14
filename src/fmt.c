// string formatting
#include "rsm.h"

const char* rop_name(rop op) {
  switch (op) {
    #define _(name, ...) case rop_##name: return #name;
    DEF_RSM_OPS(_)
    #undef _
  }
  return "?";
}


const char* rtype_name(rtype t) {
  switch (t) {
    #define _(name, ...) case rtype_##name: return #name;
    DEF_RSM_TYPES(_)
    #undef _
  }
  return "?";
}

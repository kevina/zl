#ifndef GC__HPP
#define GC__HPP

#ifndef NO_GC

#include <gc/gc_cpp.h>
#include <gc/gc_allocator.h>

#else // NO_GC

struct gc {};
struct gc_cleanup {};

#define traceable_allocator std::allocator

#define GC_disable()
#define GC_MALLOC(size) calloc(size, 1)
#define GC_MALLOC_UNCOLLECTABLE(size) calloc(size, 1)
#define GC_MALLOC_ATOMIC(size) malloc(size)
#define GC_FREE(ptr) free(ptr)
#define GC_REALLOC realloc
#define GC_gcollect() 

#endif // NO_GC

#endif

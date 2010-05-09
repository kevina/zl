//
// Fixed bug #1
//

// the combination of const and "struct Foo" didn't parse correctly..
const struct Foo foo;

//
// Fixed bug #2
//

typedef struct // this struct is anon$1
{
  int val;
} __quad_t;

typedef struct // anon$2
{
  int val;
} __u_quad_t;

typedef __quad_t __off64_t;

// the top evel environ is now
// anon$1 // from __off64_t typedef
// anon$2
// anon$1

// now this will become anon$2 unless the entire env is searched for
// the maximum number anon...
typedef struct { 
  int val;
} __fsid_t;

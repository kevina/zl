Syntax * parse_myclass(Syntax * p, Environ * env) {
  Mark * mark = new_mark();
  
  // to avoid duplicate work
  p = partly_expand_class(p, mark, env);

  Match * m = match_args(0, raw_syntax (name @ (pattern ({...} @body)) :(fix_size fix_size) @rest), p);

  Syntax * body = match_var(m, syntax body);
  Syntax * fix_size_s = match_var(m, syntax fix_size);

  if (!body || !fix_size_s) return parse_class(p, env);


  size_t fix_size = ct_value(fix_size_s, env);

  m = match(m, syntax dummy_decl, replace(syntax {char dummy;}, NULL, mark));
  Syntax * r = replace(raw_syntax (@ (class name ({...} body dummy_decl) @rest) 
                                     (import name)),
                                   m, mark);
  
  Environ * lenv = temp_environ(env);
  pre_parse(r, lenv);

  size_t size = ct_value(replace(syntax (offsetof(name, dummy)), m, mark), lenv);

  size_t ptr_size = ct_value(replace(syntax(sizeof(void *)), m, mark), lenv);

  if (fix_size % ptr_size != 0) {
    fix_size += ptr_size - (fix_size % ptr_size);
  } else if (fix_size == 0) {
    fix_size = ptr_size;
  }
  
  if (size == fix_size) {

    return replace(raw_syntax (class name ({...} body @rest)), m, mark);

  } else if (size < fix_size) {

    char buf[32];
    snprintf(buf, 32, "{char dummy[%u];}", fix_size - size);
    m = match(m, syntax buffer, replace(string_to_syntax(buf), NULL, mark));
    return replace(raw_syntax (class name ({...} body buffer) @rest), m, mark);

  } else {
    // now go throgh and get offset of each member until it is under
    // the size

    SyntaxList * main = new_syntax_list();
    SyntaxList * overflow = new_syntax_list();

    SyntaxList * ip = main;
    unsigned overflow_pos = -1;

    SyntaxEnum * itr = m->varl(syntax body);
    Syntax * member;
    while ((member = itr->next)) {
      Match * m0 = match(m, raw_syntax (what n @ type @_), member);
      const char * what = syntax_to_string(m0->var(syntax what));
      if (strcmp(what, "var")==0) {
        size_t offset = ct_value(replace(syntax (offsetof(name, n)), m0, mark), lenv);
        size_t size_of = ct_value(replace(raw_syntax (sizeof (.type type)), m0, mark), lenv);
        if (ip != overflow && offset + size_of > fix_size - ptr_size) {
          overflow_pos = main->append(NULL);
          ip = overflow;
        }
        ip->append(member);
        if (ip == overflow)
          main->append(replace(raw_syntax(alias n type (-> (member overflow ptr) n) :need_constructor), m0, mark));
      } else {
        main->append(member);
      }
    }

    Syntax * of = replace(syntax {
        class Overflow {
          class Data {
            $1;
          };
          Data * ptr;
          Overflow() {ptr = malloc(sizeof(Data));}
          Overflow(const Overflow & o) {ptr = malloc(sizeof(Data));}
          ~Overflow() {free(ptr);}
        };
        Overflow overflow;
      }, match_local(m, overflow, NULL), mark);
    main->replace(overflow_pos, of);
    
    Syntax * res =  replace(raw_syntax (class name ({...} $1) @rest), 
                            match_local(m, main, NULL), mark);
    //printf("===\n");
    //dump_syntax(res);
    //printf("^^^\n");
    return res;
  }
}

make_syntax_macro class parse_myclass;

class X;

class X : fix_size(16) {
  int x;
  char c;
};

class Y : public X : fix_size(32) {
  int j;
};

class Z : fix_size(8) {
  class I {
    int x;
  };
  I i;
  int x;
  int y;
  int z;
};

class Z2 : fix_size(8) {
  class I {
    int x;
  };
  I i;
  int x;
  int y;
  int z;
};


int main() {
  printf("%d\n", sizeof(X));
  Y y;
  y.x;
  printf("%d\n", sizeof(Y));
  Z z;
  z.x = 10;
  z.y = 20;
  z.z = 30;
  printf("%d %d %d %d\n", sizeof(Z), z.x, z.y, z.z);
  Z z2;
  printf("%d\n", sizeof(Z2));
}


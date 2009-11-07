#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "config.h"

#include "parse.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "peg.hpp"
#include "expand.hpp"

extern "C" {
//#include <gc_backptr.h>
  void GC_dump();
  extern unsigned GC_alloc_count;
}

//#include "symbol_table.hpp"

void parse_maps(ast::Environ & env) {
  SourceFile * code = new_source_file(SOURCE_PREFIX "grammer.ins");
  const char * s = code->begin();
  try {
    while (s != code->end()) {
      parse_parse::Res r = parse_parse::parse(SourceStr(code, s, code->end()));
      read_macro(r.parse, env);
      s = r.end;
    }
  } catch (Error * err) {
    fprintf(stderr, "%s\n", err->message().c_str());
    exit(1);
  }
}

// FIXME: No global
ParsePeg::Parse parse;
SourceFile * file = 0;

int main(int argc, const char *argv[])
{
  //GC_disable();
  //GC_free_space_divisor = 32;

  assert(setvbuf(stdin, 0, _IOLBF, 0) == 0); 
  assert(setvbuf(stdout, 0, _IOLBF, 0) == 0);
  parse_exp_->init();
  file = new_source_file(SOURCE_PREFIX "grammer.in");
  ast::Environ env;
  try {
    parse.top(file->begin(), file->end());
  } catch (Error * err) {
    err->source = file;
    fprintf(stderr, "%s\n", err->message().c_str());
    exit(1);
  }
  SourceFile * prelude = new_source_file(SOURCE_PREFIX "prelude.zlh");
  SourceFile * code = NULL;
  try {
    unsigned offset = 1;
    bool debug_mode = false;
    bool zls_mode = false;
    bool for_ct = false;
    bool load_prelude = true;
    if (argc > offset && strcmp(argv[offset], "-d") == 0) {
      debug_mode = true;
      offset++;
    } 
    if (argc > offset && strcmp(argv[offset], "-s") == 0) {
      zls_mode = true;
        offset++;
    }
    if (argc > offset && strcmp(argv[offset], "-C") == 0) {
      for_ct = true;
        offset++;
    }
    if (argc > offset && strcmp(argv[offset], "-P") == 0) {
      load_prelude = false;
      offset++;
    }
    String base_name;
    String output_fn;
    if (argc > offset) {
      code = new_source_file(argv[offset]);
      const char * dot = strrchr(argv[offset], '.');
      if (!dot) {
          base_name = argv[offset];
          output_fn = base_name;
      } else {
        StringBuf buf;
        buf.append(argv[offset], dot);
        base_name = buf.freeze(); // will also reset buf
        if (strcmp(dot, ".zls") == 0)
          buf.append(argv[offset]);
        else
          buf.append(argv[offset], dot);
        buf.append(".zls");
        output_fn = buf.freeze();
      }
    } else {
        code = new_source_file(STDIN_FILENO);
        output_fn = "a.out.zls";
    }
    //const Syntax * res = parse_str("TEST", SourceStr(code->entity(), code->begin(), code->end()));
    //res->print();
    //printf("\n");
    //exit(0);
    //printf("%d\n%s", ast::MACRO_PRELUDE_END - ast::MACRO_PRELUDE, ast::MACRO_PRELUDE);
    if (zls_mode) {
      printf("ZLS MODE\n");
      parse_stmts_raw(SourceStr(code, code->begin(), code->end()), env);
    } else {
      parse_maps(env);
      parse_stmts(parse_str("SLIST", SourceStr(prelude, prelude->begin(), prelude->end())),env);
      if (debug_mode && load_prelude) {
        SourceFile * prelude_body = new_source_file(SOURCE_PREFIX "prelude.zl");
        parse_stmts(parse_str("SLIST", SourceStr(prelude_body, prelude_body->begin(), prelude_body->end())), env);
        //SourceFile * class_body = new_source_file(SOURCE_PREFIX "class.zl");
        //parse_stmts(parse_str("SLIST", SourceStr(class_body, class_body->begin(), class_body->end())), env);
      } else if (load_prelude) {
        load_macro_lib(SOURCE_PREFIX "prelude-fct.so", env);
      }
      //if (load_prelude && !for_ct)
        //ast::import_file(SOURCE_PREFIX "test/class-new_abi.zl", env);
        //ast::include_file(SOURCE_PREFIX "test/class-this_reg.zlh", env);
      parse_stmts(parse_str("SLIST", SourceStr(code, code->begin(), code->end())),env);
    }
    ast::CompileWriter out;
    out.open(output_fn, "w");
    if (for_ct) 
      out.for_macro_sep_c = new ast::CompileWriter::ForMacroSepC;
    //printf("FORCING COLLECTION\n");
    //GC_gcollect();
    //GC_dump();
    ast::compile(env.top_level_symbols, out);
    //ast::CompileWriter out2(ast::CompileWriter::ZLE);
    //out2.open("a.out.zle", "w");
    //ast::compile(env.top_level_symbols, out2);
    //AST::ExecEnviron env;
    //ast->eval(env);
    if (for_ct) {
      out.close();
      StringBuf buf;
      buf.printf("zls -g -fexceptions -shared -fpic -o %s-fct.so %s", ~base_name, ~output_fn);
      String line = buf.freeze();
      printf("%s\n", ~line);
      int res = system(~line);
      if (res == -1) {
        perror("system(\"zls ...\")");
        exit(2);
      } else if (res != 0) {
        exit(1);
      }
    }
    out.for_macro_sep_c = NULL;

    //fprintf(stderr, "---------------------------------------------\n");
    //GC_generate_random_backtrace();
    //fprintf(stderr, "---------------------------------------------\n");
    //GC_generate_random_backtrace();
    //fprintf(stderr, "---------------------------------------------\n");
    //GC_generate_random_backtrace();
    //fprintf(stderr, "---------------------------------------------\n");
    //GC_generate_random_backtrace();
    //fprintf(stderr, "---------------------------------------------\n");
    //GC_generate_random_backtrace();
    //fprintf(stderr, "---------------------------------------------\n");

  } catch (Error * err) {
    //if (!err->source)
    //  err->source = code->entity();
    fprintf(stderr, "%s\n", err->message().c_str());
    exit(2);
  }

  parse = ParsePeg::Parse();
  file = NULL;

    
  //sleep(600);
}

// int main(int argc, const char *argv[])
// {
//   printf("%u %u %u\n", sizeof(Syntax), sizeof(Syntax::D), sizeof(Syntax::Data));
//   printf("XXX %u\n", sizeof(SymbolName));
//   printf("XXX %u\n", sizeof(SourceStr));
//   Syntax syn;

//   int res = main2(argc,argv);

//   printf("FORCING COLLECTION 2\n");
//   GC_gcollect();

// //   fprintf(stderr, "---------------------------------------------\n");
// //   GC_generate_random_backtrace();
// //   fprintf(stderr, "---------------------------------------------\n");
// //   GC_generate_random_backtrace();
// //   fprintf(stderr, "---------------------------------------------\n");
// //   GC_generate_random_backtrace();
// //   fprintf(stderr, "---------------------------------------------\n");
// //   GC_generate_random_backtrace();
// //   fprintf(stderr, "---------------------------------------------\n");
// //   GC_generate_random_backtrace();
// //   fprintf(stderr, "---------------------------------------------\n");


//   return res;
// }
 


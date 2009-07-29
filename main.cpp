#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "parse.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "peg.hpp"
#include "expand.hpp"

//#include "symbol_table.hpp"

void parse_maps(ast::Environ & env) {
  SourceFile * code = new_source_file("grammer.ins");
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
  assert(setvbuf(stdin, 0, _IOLBF, 0) == 0); 
  assert(setvbuf(stdout, 0, _IOLBF, 0) == 0);
  parse_exp_->init();
  file = new_source_file("grammer.in");
  ast::Environ env;
  try {
    parse.top(file->begin(), file->end());
  } catch (Error * err) {
    err->source = file;
    fprintf(stderr, "%s\n", err->message().c_str());
    exit(1);
  }
  parse_maps(env);
  SourceFile * prelude = new_source_file("prelude.zlh");
  SourceFile * code = NULL;
  try {
    if (argc == 2 && strcmp(argv[1], "-p") == 0) {
      SourceFile * prelude_body = new_source_file("prelude.zl");
      parse_stmts(parse_str("SLIST", SourceStr(prelude, prelude->begin(), prelude->end())),env);
      parse_stmts(parse_str("SLIST", SourceStr(prelude_body, prelude_body->begin(), prelude_body->end())), env);
      //system("gcc -g -O -fexceptions -shared -fpic -o prelude.so prelude.c");
      ast::CompileWriter out;
      out.for_macro_sep_c = new ast::CompileWriter::ForMacroSepC;
      out.open("prelude.zls", "w");
      ast::compile(env.top_level_symbols, out);
      out.close();
      system("zls -O -S -fexceptions -o prelude.zls.s prelude.zls");
      system("zls -g -O -fexceptions -shared -fpic -o prelude.so prelude.zls");
      //load_macro_lib("./prelude.so", env);
    } else {
      unsigned offset = 1;
      bool debug_mode = false;
      bool zls_mode = false;
      if (argc > offset && strcmp(argv[offset], "-d") == 0) {
        debug_mode = true;
        offset++;
      } 
      if (argc > offset && strcmp(argv[offset], "-s") == 0) {
        zls_mode = true;
        offset++;
      } 
      if (argc > offset) {
        code = new_source_file(argv[offset]);
      } else {
        code = new_source_file(STDIN_FILENO);
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
        parse_stmts(parse_str("SLIST", SourceStr(prelude, prelude->begin(), prelude->end())),env);
        if (debug_mode) {
          SourceFile * prelude_body = new_source_file("prelude.zl");
          parse_stmts(parse_str("SLIST", SourceStr(prelude_body, prelude_body->begin(), prelude_body->end())), env);
        } else {
          load_macro_lib("./prelude.so", env);
        }
        parse_stmts(parse_str("SLIST", SourceStr(code, code->begin(), code->end())),env);
      }
      ast::CompileWriter out;
      out.open("a.out.zls", "w");
      ast::compile(env.top_level_symbols, out);
      ast::CompileWriter out2(ast::CompileWriter::ZLE);
      out2.open("a.out.zle", "w");
      ast::compile(env.top_level_symbols, out2);
      //AST::ExecEnviron env;
      //ast->eval(env);
    }
  } catch (Error * err) {
    //if (!err->source)
    //  err->source = code->entity();
    fprintf(stderr, "%s\n", err->message().c_str());
    exit(2);
  }
  //sleep(600);
#if 0
  SourceFile code;
  code->read(STDIN_FILENO);
  Parts dummy;
  const char * s = code->begin();
  try {
    ParseSExp::Res r = ParseSExp::parse_s_exp(s, code->end());
    assert(s != r.end);
    printf(">>%.*s<<\n", r.end-s, s);
    r.parse->print();
    printf("\n");
    AST::AST * ast = AST::parse(r.parse);
    ast->eval(new AST::Environ);
  } catch (ParseCommon::Error * err) {
    puts(err->message().c_str());
  }
#endif
}

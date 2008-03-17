#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "parse.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "peg.hpp"
#include "expand.hpp"

void parse_maps(ast::Environ & env) {
  SourceFile * code = new_source_file("grammer.ins");
  const char * s = code->begin();
  try {
    while (s != code->end()) {
      parse_parse::Res r = parse_parse::parse(SourceStr(code->entity(), s, code->end()));
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
    err->source = file->entity();
    fprintf(stderr, "%s\n", err->message().c_str());
    exit(1);
  }
  parse_maps(env);
  SourceFile * code;
  if (argc >= 2) {
    code = new_source_file(argv[1]);
  } else {
    code = new_source_file(STDIN_FILENO);
  }
  try {
    const Parse * to_expand = parse_str("TOP", SourceStr(code->entity(), code->begin(), code->end()));
    ast::AST * ast = expand_top(to_expand, env);
    //printf("\n*************** EXPANDED *********************\n");
    //expanded->print();
    //printf("\n");
    //exit(0);
    ast::CompileEnviron cenv;
    ast::CompileWriter out;
    out.open("a.out.c", "w");
    ast->compile(out, cenv);
    //AST::ExecEnviron env;
    //ast->eval(env);
  } catch (Error * err) {
    err->source = code->entity();
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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "parse.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "peg.hpp"
#include "expand.hpp"

void parse_maps() {
  SourceFile * code = new_source_file("grammer.ins");
  const char * s = code->begin();
  try {
    while (s != code->end()) {
      parse_parse::Res r = parse_parse::parse(SourceStr(code->entity(), s, code->end()));
      printf(">>%.*s<<\n", r.end-s, s);
      r.parse->print();
      printf("\n");
      read_macro(r.parse);
      s = r.end;
    }
  } catch (Error * err) {
    puts(err->message().c_str());
  }
}

// FIXME: No global
ParsePeg::Parse parse;
SourceFile * file = 0;

int main()
{
  parse_exp_->init();
  file = new_source_file("grammer.in");
  try {
    parse.top(file->begin(), file->end());
  } catch (Error * err) {
    err->source = file->entity();
    puts(err->message().c_str());
    exit(1);
  }
  parse_maps();
  SourceFile * code = new_source_file(STDIN_FILENO);
  try {
    const Parse * to_expand = parse_str("TOP", SourceStr(code->entity(), code->begin(), code->end()));
    const Parse * expanded = expand_top(to_expand);
    printf("\n*************** EXPANDED *********************\n");
    expanded->print();
    printf("\n");
    //exit(0);
    AST::AST * ast = AST::parse_top(expanded);
    AST::CompileEnviron cenv;
    AST::CompileWriter out;
    out.open("a.out.c", "w");
    ast->compile(out, cenv);
    //AST::ExecEnviron env;
    //ast->eval(env);
  } catch (Error * err) {
    err->source = code->entity();
    printf("::\n");
    puts(err->message().c_str());
    exit(2);
  }
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


int main() {
  int x = ({int y = 2; loop_ y + 3; /*goto loop0;*/ 22;});
bla: x = 20;
}

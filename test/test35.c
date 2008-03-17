
map try_forever(c,g) {
 label:
  if (c) goto g;
  else goto label;
}

int main() {
  try_forever(1, label);
 label:
  return 10;
}



struct A {
  int a;
  int b;
};

int main() {
  struct A a = { 1, 2};
  struct A b = a;
  return 0;
}
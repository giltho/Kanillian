int x;
long y;

int __nondet_int();
int __nondet_long();

int assign_x() {
  x = __nondet_int();
  return 0;
}

int assign_y() {
  y = __nondet_long();
  return 0;
}

int main(void) {
  assign_x();
  assign_y();
  __CPROVER_assert(x >= y, "what");
  return 0;
}
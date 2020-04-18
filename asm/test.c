
long int global;

long int simple(long int a, long int b) {
  return a + b;
}

int main(int argc, char ** argv) {
  global = simple(5, 47);
  return 0;
}


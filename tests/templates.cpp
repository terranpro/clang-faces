#include <vector>
#include <iostream>

template<int T>
struct fib 
{
  static int const value = fib<T - 2>::value + fib<T - 1>::value;
};

template<>
struct fib<0>
{
  static const int value = 0;
};

template<>
struct fib<1>
{
  static const int value = 1;
};

int main(int argc, char *argv[])
{
  fib<5> f;
  auto x = fib<5>::value;
  char32_t i = 13;
  
  return 0;
}

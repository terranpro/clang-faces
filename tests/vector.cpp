#include <vector>
#include <iostream>

#include <stdarg.h>

int f( int x )
{return x + 1;}

struct functor 
{
  int g(long a) const
  {
    return a / 2;
  }
  
  int operator()(int x, int y, int z) const
  {
    return x + f(y)*2 + 3*g(z);
  }
};

int main(int argc, char *argv[])
{
  std::vector<int> v { 1, 2, 3, 4, 5 };
  auto geez = 0;
  decltype( geez ) gazz = 1;
  
  v[ geez ] = 1;
  v[ 4 ] = 3;
  v[ f( 3 ) ] = 3;
  
  f( v[ geez ] );

  functor()( v[geez], v[geez] + 1, v[geez + 2 ] );
  
  return 0;
}

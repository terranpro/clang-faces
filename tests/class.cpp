struct dog 
{
  int x;
  void bark() const
  {
    this->x = 1;
  }
};

int main(int argc, char *argv[])
{
  dog d;
  dog().bark();
  return 0;
}

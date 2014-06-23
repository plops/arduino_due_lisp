#include "pylon_wrapper.h"

int main()
{
  pylon_wrapper_initialize();
  void*c=pylon_wrapper_create(2);
  pylon_wrapper_start_grabbing(c);
  pylon_wrapper_grab(c);
  pylon_wrapper_terminate();
}

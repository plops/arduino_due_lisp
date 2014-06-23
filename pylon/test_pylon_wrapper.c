#include "pylon_wrapper.h"

int main()
{
  pylon_wrapper_initialize();
  void*c=pylon_wrapper_create(2);
  pylon_wrapper_start_grabbing(c);
  unsigned char buf[2000*2000];
  int success_p=0,w=0,h=0;
  pylon_wrapper_grab(c,2000,2000,buf,&success_p,&w,&h);
  //pylon_wrapper_terminate();
}

#include "pylon_wrapper.h"

int main()
{
  pylon_wrapper_initialize();
  void*c=pylon_wrapper_create(2);
  pylon_wrapper_start_grabbing(c);
  unsigned char buf[2000*2000];
  int success_p=0,w=0,h=0,cam=-1;
  pylon_wrapper_grab(c,2000,2000,buf,&cam,&success_p,&w,&h);
  printf("%d\n", pylon_wrapper_get_max_i(c,0,"Width"));

  pylon_wrapper_terminate();
}

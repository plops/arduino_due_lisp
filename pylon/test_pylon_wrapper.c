#include "pylon_wrapper.h"
#include <stdio.h>
int main()
{
  pylon_wrapper_initialize();
  {
    void*f=pylon_wrapper_factory();
    void*c=pylon_wrapper_create(f,2);
    pylon_wrapper_start_grabbing(c);
    unsigned char buf[2000*2000];
    int success_p=0,w=0,h=0,cam=-1;
    pylon_wrapper_grab(c,2000,2000,buf,&cam,&success_p,&w,&h);
    pylon_wrapper_grab(c,2000,2000,buf,&cam,&success_p,&w,&h);
    pylon_wrapper_grab(c,2000,2000,buf,&cam,&success_p,&w,&h);
    printf("%d\n", pylon_wrapper_get_max_i(c,0,"Width"));

    printf("%d\n", pylon_wrapper_get_min_i(c,0,"Width"));
    printf("%d\n", pylon_wrapper_get_inc_i(c,0,"Width"));
    
    printf("%d\n", pylon_wrapper_get_value_e(c,0,"PixelFormat"));
    pylon_wrapper_get_symbolics_e(c,0,"PixelFormat");
    pylon_wrapper_to_string_e(c,0,"PixelFormat");
    pylon_wrapper_terminate(c,f);
  }
}

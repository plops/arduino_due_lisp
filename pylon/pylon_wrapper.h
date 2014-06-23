void pylon_wrapper_initialize();
void pylon_wrapper_terminate();
void*pylon_wrapper_create(unsigned int maxCamerasToUse);
void pylon_wrapper_start_grabbing(void*cams);
// cams .. pointer handle as returned by create
// buf .. pointer to foreign allocated array
// ww, hh .. size of buf
// success_p .. !=0 if grab was successful
// w .. returns image width
// h .. returns image height
void pylon_wrapper_grab(void*cams,int ww,int hh,unsigned char * buf, int *camera,int*success_p,int*w,int*h);

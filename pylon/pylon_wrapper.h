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
int pylon_wrapper_get_max_i(void*cams,int cam,const char*node);
int pylon_wrapper_get_min_i(void*cams,int cam,const char*node);
int pylon_wrapper_get_inc_i(void*cams,int cam,const char*node);
int pylon_wrapper_get_value_i(void*cams,int cam,const char*node,int verify, int ignore_cache);
void pylon_wrapper_set_value_i(void*cams,int cam,const char*node,int value);
void pylon_wrapper_get_symbolics_e(void*cams,int cam,const char*node); 
void pylon_wrapper_set_value_e(void*cams,int cam,const char*node,int value);
int pylon_wrapper_get_value_e(void*cams,int cam,const char*node);
void pylon_wrapper_to_string_e(void*cams,int cam,const char*node);
void pylon_wrapper_from_string_e(void*cams,int cam,const char*node,char*value);

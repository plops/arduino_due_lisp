void pylon_wrapper_initialize();
void pylon_wrapper_terminate(void*cams);
void*pylon_wrapper_factory();
void*pylon_wrapper_create(void*factory,unsigned int maxCamerasToUse);
void pylon_wrapper_start_grabbing(void*cams);
void pylon_wrapper_stop_grabbing(void*cams);
void pylon_wrapper_cams_open(void*cams);
void pylon_wrapper_cams_close(void*cams);
void pylon_wrapper_cam_open(void*cams,int cam);
void pylon_wrapper_cam_close(void*cams,int cam);
// cams .. pointer handle as returned by create
// buf .. pointer to foreign allocated array
// ww, hh .. size of buf
// success_p .. !=0 if grab was successful
// w .. returns image width
// h .. returns image height
void pylon_wrapper_grab(void*cams,int ww,int hh,unsigned char * buf, int *camera,int*success_p,int*w,int*h);
void pylon_wrapper_grab_cdf(void*cams,int ww,int hh,double * buf,int*camera,int*success_p,int*w,int*h);
int pylon_wrapper_get_max_i(void*cams,int cam,const char*node);
int pylon_wrapper_get_min_i(void*cams,int cam,const char*node);
int pylon_wrapper_get_inc_i(void*cams,int cam,const char*node);
float pylon_wrapper_get_value_f(void*cams,int cam,const char*node,int verify, int ignore_cache);
int pylon_wrapper_get_value_b(void*cams,int cam,const char*node,int verify, int ignore_cache);
int pylon_wrapper_get_value_i(void*cams,int cam,const char*node,int verify, int ignore_cache);
void pylon_wrapper_set_value_i(void*cams,int cam,const char*node,int value);
void pylon_wrapper_get_symbolics_e(void*cams,int cam,const char*node); 
void pylon_wrapper_set_value_e(void*cams,int cam,const char*node,int value);
int pylon_wrapper_get_value_e(void*cams,int cam,const char*node);
void pylon_wrapper_to_string_e(void*cams,int cam,const char*node);
void pylon_wrapper_from_string_e(void*cams,int cam,const char*node,char*value);
const char* pylon_wrapper_cam_get_serial_number(void*cams,int cam);
const char* pylon_wrapper_cam_get_full_name(void*cams,int cam);

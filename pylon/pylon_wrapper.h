void pylon_wrapper_initialize();
void pylon_wrapper_terminate();
void*pylon_wrapper_create(unsigned int maxCamerasToUse);
void pylon_wrapper_start_grabbing(void*cams);
void pylon_wrapper_grab(void*cams);

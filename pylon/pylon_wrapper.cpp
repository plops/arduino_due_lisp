#include <pylon/PylonIncludes.h>

using namespace Pylon;
using namespace std;

extern "C" {
  void pylon_wrapper_initialize()
  {
    try{
      PylonInitialize();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
    }
  }
  void pylon_wrapper_terminate()
  {
    try{
      PylonTerminate();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
    }
  }
  void*pylon_wrapper_create(unsigned int maxCamerasToUse)
  {
    try{
      CTlFactory& tlFactory = CTlFactory::GetInstance();
      // Get all attached devices
      DeviceInfoList_t devices;
      if ( tlFactory.EnumerateDevices(devices) == 0 )
	printf("%s finds no cameras: %d\n",__func__,devices.size());       
      
      CInstantCameraArray *cameras =
	new CInstantCameraArray(min(size_t(maxCamerasToUse),devices.size()));
      
      // Create and attach all Pylon Devices.
      for ( size_t i = 0; i < cameras->GetSize(); ++i) {
	(*cameras)[ i ].Attach( tlFactory.CreateDevice( devices[ i ]));
	// Print the model name of the camera.
	cout << "Using device " << (*cameras)[ i ].GetDeviceInfo().GetModelName() << endl;
      }
      return cameras;
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
      return NULL;
    }
  }
  void pylon_wrapper_start_grabbing(void*cams)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      cameras->StartGrabbing();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
    }
  }
  // cams .. pointer handle as returned by create
  // buf .. pointer to foreign allocated array
  // ww, hh .. size of buf
  // success_p .. !=0 if grab was successful
  // w .. returns image width
  // h .. returns image height
  void pylon_wrapper_grab(void*cams,int ww,int hh,unsigned char * buf,int*camera,int*success_p,int*w,int*h)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      if(cameras->IsGrabbing()){
	CGrabResultPtr ptrGrabResult;
	cameras->RetrieveResult( 5000, ptrGrabResult, TimeoutHandling_ThrowException);
	// context allows to determine which camera produced the grab result
	intptr_t cameraContextValue = ptrGrabResult->GetCameraContext();
	*camera = cameraContextValue;
	cout << "Camera " <<  cameraContextValue << ": " << (*cameras)[ cameraContextValue ].GetDeviceInfo().GetModelName() << endl;
	cout << "GrabSucceeded: " << ptrGrabResult->GrabSucceeded() << endl;
	*success_p = ptrGrabResult->GrabSucceeded();
	if(!ptrGrabResult->GrabSucceeded()){
	  std::cout << "Error: " << ptrGrabResult->GetErrorCode() << " " << ptrGrabResult->GetErrorDescription();
	  return;
	}
	cout << "SizeX: " << ptrGrabResult->GetWidth() << endl;
	cout << "SizeY: " << ptrGrabResult->GetHeight() << endl;
	const uint8_t *pImageBuffer = (uint8_t *) ptrGrabResult->GetBuffer();
	cout << "Gray value of first pixel: " << (uint32_t) pImageBuffer[0] << endl << endl;

	*w = ptrGrabResult->GetWidth();
	*h = ptrGrabResult->GetHeight();
	if (ww<*w)
	  printf("width of input array not sufficient");
	if (hh<*h)
	  printf("height of input array not sufficient");

	int i,j;
	for(j=0;j< (*h);j++)
	  for(i=0;i< (*w);i++)
	    buf[i+ (*w) * j] = pImageBuffer[i+(*w)*j];
      }
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
    }
  }
}

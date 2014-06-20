#include <pylon/PylonIncludes.h>

using namespace Pylon;
using namespace std;

static const uint32_t c_countOfImagesToGrab = 10;
static const size_t c_maxCamerasToUse = 2;

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
  void pylon_wrapper_create()
  {
    try{
      CTlFactory& tlFactory = CTlFactory::GetInstance();
      
      // Get all attached devices
      DeviceInfoList_t devices;
      if ( tlFactory.EnumerateDevices(devices) == 0 )
	printf("%s finds no cameras: %d\n",__func__,devices.size()); 

      CInstantCameraArray cameras( min( devices.size(), c_maxCamerasToUse));

      // Create and attach all Pylon Devices.
      for ( size_t i = 0; i < cameras.GetSize(); ++i) {
	cameras[ i ].Attach( tlFactory.CreateDevice( devices[ i ]));
	
	// Print the model name of the camera.
	cout << "Using device " << cameras[ i ].GetDeviceInfo().GetModelName() << endl;
      }
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
    }
  }
}

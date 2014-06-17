// Grab_MultipleCameras.cpp
/*
   The main purpose of the CInstantCameraArray is to simplify waiting for images and
   camera events of multiple cameras in one thread. This is done by providing a single
   RetrieveResult method for all cameras in the array.
   Alternatively, the grabbing can be started using the internal grab loop threads
   of all cameras in the CInstantCameraArray. The grabbed images can then be processed by one or more
   image event handlers. Please note that this is not shown in this example.
*/
#include <pylon/PylonIncludes.h>

using namespace Pylon;
using namespace std;

static const uint32_t c_countOfImagesToGrab = 10;
static const size_t c_maxCamerasToUse = 2;
/*
pylon-3.2.1-x86_64/doc/pylon_programmingguide.html
PylonInitialize();
PylonTerminate();
You should guard pylon calls with exception handlers catching GenICam::GenericException like so:
catch (GenICam::GenericException& e) {
  throw RUNTIME_EXCEPTION( "Exception caught in OnOpened method msg=%hs", e.what()); }
IPylonDevice* pDevice = CTlFactory::GetInstance().CreateFirstDevice();
Before reading or writing parameters of a camera apply Open(), after use Close()
*/
int main(int argc, char* argv[])
{
    int exitCode = 0;

    // Automagically call PylonInitialize and PylonTerminate to ensure the pylon runtime system.
    // is initialized during the lifetime of this object
    Pylon::PylonAutoInitTerm autoInitTerm;

    try
    {
        // Get the transport layer factory.
        CTlFactory& tlFactory = CTlFactory::GetInstance();

        // Get all attached devices and exit application if no device is found.
        DeviceInfoList_t devices;
        if ( tlFactory.EnumerateDevices(devices) == 0 )
        {
	  //   throw RUNTIME_EXCEPTION( "No camera present.");
	  printf("no cameras: %d\n",devices.size());
        }

        // Create an array of instant cameras for the found devices and avoid exceeding a maximum number of devices.
        CInstantCameraArray cameras( min( devices.size(), c_maxCamerasToUse));

        // Create and attach all Pylon Devices.
        for ( size_t i = 0; i < cameras.GetSize(); ++i)
        {
            cameras[ i ].Attach( tlFactory.CreateDevice( devices[ i ]));

            // Print the model name of the camera.
            cout << "Using device " << cameras[ i ].GetDeviceInfo().GetModelName() << endl;
        }

        // Starts grabbing for all cameras starting with index 0. The grabbing
        // is started for one camera after the other. That's why the images of all
        // cameras are not taken at the same time.
        // However, a hardware trigger setup can be used to cause all cameras to grab images synchronously.
        // According to their default configuration, the cameras are
        // set up for free-running continuous acquisition.
        cameras.StartGrabbing();

        // This smart pointer will receive the grab result data.
        CGrabResultPtr ptrGrabResult;

        // Grab c_countOfImagesToGrab from the cameras.
        for( int i = 0; i < c_countOfImagesToGrab && cameras.IsGrabbing(); ++i)
        {
            cameras.RetrieveResult( 5000, ptrGrabResult, TimeoutHandling_ThrowException);

            // When the cameras in the array are created the camera context value
            // is set to the index of the camera in the array.
            // The camera context is a user settable value.
            // This value is attached to each grab result and can be used
            // to determine the camera that produced the grab result.
            intptr_t cameraContextValue = ptrGrabResult->GetCameraContext();

            // Print the index and the model name of the camera.
            cout << "Camera " <<  cameraContextValue << ": " << cameras[ cameraContextValue ].GetDeviceInfo().GetModelName() << endl;

            // Now, the image data can be processed.
            cout << "GrabSucceeded: " << ptrGrabResult->GrabSucceeded() << endl;
            cout << "SizeX: " << ptrGrabResult->GetWidth() << endl;
            cout << "SizeY: " << ptrGrabResult->GetHeight() << endl;
            const uint8_t *pImageBuffer = (uint8_t *) ptrGrabResult->GetBuffer();
            cout << "Gray value of first pixel: " << (uint32_t) pImageBuffer[0] << endl << endl;
        }
    }
    catch (GenICam::GenericException &e)
    {
        // Error handling
        cerr << "An exception occurred." << endl
        << e.GetDescription() << endl;
        exitCode = 1;
    }

    return exitCode;
}

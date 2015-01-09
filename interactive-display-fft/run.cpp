#include "api.h"
#include <stdio.h>
#include <malloc.h>
#include <rfb/rfb.h>
#include <pylon/PylonIncludes.h>
#include "radon.h"

using namespace Pylon;
using namespace GenApi;
using namespace std;
 
#define f(e) do{if(0)(e);}while(0)
#define d(cmd)  do{ try{ cmd }			\
  catch (GenICam::GenericException& e) {        \
printf( "Exception caught in %s msg=%s",__func__, e.what());	\
}} while(0)

struct run_state{
  rfbScreenInfoPtr server;
  CInstantCameraArray *cameras;
};

const  int w=280,h=280;

struct run_state * r_init()
{
  /* initialize VNC server */
  struct run_state *state = (run_state*)malloc(sizeof(*state));
  printf("init\n");
  state->server=rfbGetScreen(0,NULL,w,h,8,3,4);
  if(!state->server)
    return 0;
  state->server->frameBuffer=(char*)malloc(w*h*4);
  state->server->alwaysShared=(1==1);
  rfbInitServer(state->server);
  

  /* initialize camera */
  d(PylonInitialize(););
  CTlFactory& tlFactory = CTlFactory::GetInstance();

  // Get all attached devices and exit application if no device is found.
  DeviceInfoList_t devices;
  d(
  if ( tlFactory.EnumerateDevices(devices) == 0 ) {
    printf("no cameras: %ld\n",devices.size());
  });
    
  CInstantCameraArray *cameras= new CInstantCameraArray( min( devices.size(), (long unsigned int) 1));
  d(
    // Create and attach all Pylon Devices.
    for ( size_t i = 0; i < cameras->GetSize(); ++i){
      (*cameras)[ i ].Attach( tlFactory.CreateDevice( devices[ i ]));
    // Print the model name of the camera.
    cout  << "FullName " << (*cameras)[ i ].GetDeviceInfo().GetFullName()
	  << " Serial " << (*cameras)[ i ].GetDeviceInfo().GetSerialNumber() << endl;
  }
    );
  d(cameras->StartGrabbing(););
  state->cameras = cameras;

  return state;
}
void r_finalize(struct run_state *state)
{
  printf("finalize\n");
  /* close VNC server */
  rfbShutdownServer(state->server,TRUE);
  free(state->server->frameBuffer);
  rfbScreenCleanup(state->server);

  /* close camera */
  if(state->cameras){
    delete state->cameras;
    state->cameras = 0;
  }
  d(PylonTerminate(););
  
  free(state);
}
static int count = 0;
int r_step(struct run_state *state)
{
  //  printf("step\n");
  if(!rfbIsActive(state->server))
    return 0;
  
  if(state->cameras && state->cameras->IsGrabbing()){
    CGrabResultPtr res;
    state->cameras->RetrieveResult( 5000, res, TimeoutHandling_ThrowException);
    // When the cameras in the array are created the camera context value
    // is set to the index of the camera in the array.
    // The camera context is a user settable value.
    // This value is attached to each grab result and can be used
    // to determine the camera that produced the grab result.
    intptr_t cameraContextValue = res->GetCameraContext();
    // Print the index and the model name of the camera.
    cout << "Camera " << cameraContextValue << ": " 
	 << (*(state->cameras))[ cameraContextValue ].GetDeviceInfo().GetFullName() << endl;
    // Now, the image data can be processed.
    f(cout << "GrabSucceeded: " << res->GrabSucceeded() << endl);
    int ww = res->GetWidth(), hh = res->GetHeight();
    f(cout << "Size: " << ww << "x" << hh << endl);
  
    const uint8_t *im = (uint8_t *) res->GetBuffer();
    int i,j;
    char *b=state->server->frameBuffer;
    /// convert mono12p into real part of complex double float
    // i .. index for byte
    // j .. index for 12bit
    for(i=0,j=0;j< ww*hh;i+=3,j+=2) {
      unsigned char
	ab = im[i],
	c = im[i+1] & 0x0f,
	d = (im[i+1] & 0xf0)>>4,
	ef = im[i+2];
      int
	p=4*((j%ww)+ w * (j/ww)), 
	q=4*(((j+1)%ww)+ w * ((j+1)/ww));
      b[p+0]=b[p+1]=b[p+2]=(unsigned char)(255./4095.*((ab<<4)+d));
      b[q+0]=b[q+1]=b[q+2]=(unsigned char)(255./4095.*((ef<<4)+c));
    }
  }
  char s[100];
  snprintf(s,100,"count: %d\n",count++);
  rfbDrawString(state->server,&radonFont,20,100,s,0xffffff);
  rfbMarkRectAsModified(state->server,0,0,w,h);
  long usec = state->server->deferUpdateTime*1000;
  rfbProcessEvents(state->server,usec);
  
  return 1; 
}
void r_reload(struct run_state *state)
{
  //  printf("reload\n");
}
void r_unload(struct run_state *state)
{
  //  printf("unload\n");
}

const struct run_api RUN_API = {
  r_init,
  r_finalize,
  r_reload,
  r_unload,
  r_step
};



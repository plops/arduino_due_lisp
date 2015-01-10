#include "api.h"
#include <stdio.h>
#include <malloc.h>
#include <rfb/rfb.h>
#include <pylon/PylonIncludes.h>
#include "radon.h"
#include <signal.h>
#include <stdlib.h>

using namespace Pylon;
using namespace GenApi;
using namespace std;
 
// this is for setenv
#define e(q) do{if(0!=(q)) printf("error in %s",__func__);}while(0)

// this is to optionally comment out code
#define f(e) do{if(0)(e);}while(0)

// this is to call pylon functions:
#define d(cmd)  do{ try{ cmd }			\
  catch (GenICam::GenericException& e) {        \
printf( "Exception caught in %s msg=%s",__func__, e.what());	\
}} while(0)

struct run_state{
  rfbScreenInfoPtr server;
  CInstantCameraArray *cameras;
  int count;
};
struct run_state * global_state;

const  int w=512,h=512;

void r_finalize(struct run_state *state);
void r_reload(struct run_state *state);
void r_unload(struct run_state *state);
int r_step(struct run_state *state);

void signalHandler(int a)
{
  // in case i press Ctrl+c
  r_finalize(global_state);
}
    

  
struct run_state * r_init()
{
  /* define environment */
  e(setenv("PYLON_ROOT","/home/martin/pylon-4.0.0.62-x86_64/pylon4",1));
  e(setenv("GENICAM_ROOT_V2_3","/home/martin/pylon-4.0.0.62-x86_64/pylon4/genicam",1));
  e(setenv("GENICAM_CACHE_V2_3","/home/martin/genicam_xml_cache",1));
  e(setenv("LANG","C",1));

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
  
  global_state = state;
  signal(SIGTERM, signalHandler);

  return state;
}

void r_reload(struct run_state *state)
{
  state->count = 0;
  
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

}

void r_unload(struct run_state *state)
{
  /* close camera */
  if(state->cameras){
    state->cameras->StopGrabbing();
    delete state->cameras;
    state->cameras = 0;
  }
}

void r_finalize(struct run_state *state)
{
  printf("finalize\n");
  /* close VNC server */
  rfbShutdownServer(state->server,TRUE);
  free(state->server->frameBuffer);
  rfbScreenCleanup(state->server);

  r_unload(state);
  d(PylonTerminate(););
  
  free(state);
}


int r_step(struct run_state *state)
{
  //  printf("step\n");
  if(!rfbIsActive(state->server))
    return 0;
  int ma=0, mi=5000;
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
    f(cout << "Camera " << cameraContextValue << ": " 
      << (*(state->cameras))[ cameraContextValue ].GetDeviceInfo().GetFullName() << endl);
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
      int v1 = (ab<<4)+d, v2 = (ef<<4)+c;
      mi = min(mi,min(v1,v2));
      ma = max(ma,max(v1,v2));
      b[p+0]=b[p+1]=b[p+2]=(unsigned char)(255./4095.*v1);
      b[q+0]=b[q+1]=b[q+2]=(unsigned char)(255./4095.*v2);
    }
  }
  char s[100];
  snprintf(s,100,"count: %12d max %12d min %12d\n",state->count++,ma,mi);
  rfbDrawString(state->server,&radonFont,20,290,s,0xffffff);
  rfbMarkRectAsModified(state->server,0,0,w,h);
  long usec = state->server->deferUpdateTime*1000;
  rfbProcessEvents(state->server,usec);

  if(0)
  if(state->cameras && state->cameras->GetSize()!=0){
     INodeMap &control = (*(state->cameras))[0].GetNodeMap();
     d(const CIntegerPtr nod=control.GetNode("ExposureTimeRaw");
       int inc = nod->GetInc();
       nod->SetValue(inc*(200/inc));
       cout << "ExposureTimeRaw: " <<  nod->GetValue(1,1) << " " << endl;);
   }

  return 1; 
}

const struct run_api RUN_API = {
  r_init,
  r_finalize,
  r_reload,
  r_unload,
  r_step
};



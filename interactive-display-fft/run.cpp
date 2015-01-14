#include "myinc.h"

// http://cimg.sourceforge.net/reference/group__cimg__storage.html
// matrices are seen as images, so first index is column
// CImgList can apparently create nice mosaics
// get_FFT(true) inverse 
// get_FFT returns and CImgList of real and imaginary data
// first element is number of lists: eg CImgList<float> a(2,100,100) would make 2 100x100 images
// the cimg header makes compilation very small. i read this:
// https://gcc.gnu.org/onlinedocs/gcc/Precompiled-Headers.html

// apparently stl usage could be the reason why my program now crashes with the call to dlclose
// http://stackoverflow.com/questions/6450828/segmentation-fault-when-using-dlclose-on-android-platform

// i'm looking at glibc-2.19/dlfcn $ less dlopen.c 

struct run_state * global_state;

const int pylon = 1,
  w=512+512,h=512,
//  w=280+280,h=280,
  current_camera= 2;

extern "C" void signalHandler(int a)
{
  // clean up in case i press Ctrl+c 
  r_finalize(global_state);
}

  
extern "C" struct run_state * r_init()
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
  
  
  global_state = state;
  signal(SIGTERM, signalHandler);

  return state;
}

void set_exposure_time(struct run_state *state,unsigned int cam,unsigned int exptime)
{
  if(state->cameras && state->cameras->GetSize()>cam){
    INodeMap &control = (*(state->cameras))[cam].GetNodeMap();
    d(const CIntegerPtr nod=control.GetNode("ExposureTimeRaw");
      int inc = nod->GetInc();
      nod->SetValue(inc*(exptime/inc));
      cout << "ExposureTimeRaw: " <<  nod->GetValue(1,1) << " " << endl;
      );
  }
}

extern "C" void r_reload(struct run_state *state)
{
  state->count = 0;

  if(pylon){
    /* initialize camera */

    cout << "pylon initialize" << endl;
    d(PylonInitialize(););
    
    CTlFactory& tlFactory = CTlFactory::GetInstance();
    
    cout << "pylon enumerate devices" << endl;
    // Get all attached devices and exit application if no device is found.
    DeviceInfoList_t devices;
    d(
      if ( tlFactory.EnumerateDevices(devices) == 0 ) {
  	printf("no cameras: %ld..\n",devices.size());
      });
    
    CInstantCameraArray *cameras= new CInstantCameraArray( min( devices.size(), (long unsigned int) 3));
    d(
      // Create and attach all Pylon Devices.
      for ( size_t i = 0; i < cameras->GetSize(); ++i){
  	if(0==devices[i].GetSerialNumber().compare("21433565")){
  	  (*cameras)[ 0 ].Attach( tlFactory.CreateDevice( devices[ i ])); // transmission with polrot (top)
	} else if(0==devices[i].GetSerialNumber().compare("21433540")){ 
  	  (*cameras)[ 1 ].Attach( tlFactory.CreateDevice( devices[ i ])); // transmission same pol
	} else if(0==devices[i].GetSerialNumber().compare("21433566")){
  	  (*cameras)[ 2 ].Attach( tlFactory.CreateDevice( devices[ i ])); // reflection with polrot
	}
      }
      );

    for(size_t i=0;i<cameras->GetSize() ; i++)
      cout << (*cameras)[i].GetDeviceInfo().GetSerialNumber() << endl;
    
    state->cameras = cameras;
    
    d(cameras->Open(););
    
    if(1){
      set_exposure_time(state,0,281);
      set_exposure_time(state,1,153);
      set_exposure_time(state,2,10401);
    }
    cout << "StartGrabbing" << endl; 
    d(cameras->StartGrabbing(GrabStrategy_OneByOne););
  }
}

extern "C" void r_unload(struct run_state *state)
{
  if(pylon){
    /* close camera */
    cout << "close camera .." << endl;
    if(state->cameras){
      if(state->cameras->IsGrabbing()){
	cout << "  stop grabbing" << endl;
	state->cameras->StopGrabbing();
      }
      cout << "  deleting cameras" << endl;
      delete state->cameras;
      state->cameras = NULL;
    }
    cout << "  pylon terminate" << endl;  
    d(PylonTerminate(););
  }
}

extern "C" void r_finalize(struct run_state *state)
{
  printf("finalize\n");
  /* close VNC server */
  rfbShutdownServer(state->server,TRUE);
  free(state->server->frameBuffer);
  rfbScreenCleanup(state->server);

  r_unload(state);
  //cout << "  pylon terminate" << endl;    d(PylonTerminate(););

  
  free(state);
}



const struct run_api RUN_API = {
  r_init,
  r_finalize,
  r_reload,
  r_unload,
  r_step
};



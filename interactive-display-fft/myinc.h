#include "/home/martin/src/CImg/CImg.h" // in order to use as precompiled header this
		  // inclusion must come before the first line of c
#undef None
#undef Status
#include <pylon/PylonIncludes.h>

#include "api.h"
 
#include <stdio.h>
#include <malloc.h>
#include <rfb/rfb.h>
#include "radon.h"
#include <signal.h>
#include <stdlib.h>


using namespace Pylon;
using namespace GenApi;
using namespace std;
using namespace cimg_library;


// this is for setenv, only works in c99
#define e(q) do{if(0!=(q)) printf("error in %s:%d",__func__,__LINE__);}while(0)

// this is to optionally comment out code
#define f(e) do{if(0)(e);}while(0)

// this is to call pylon functions: (works only in g++)
#define d(cmd)  do{ try{ cmd }						\
    catch (GenICam::GenericException& e) {				\
      printf( "Exception caught in %s:%d msg=%s\n",__PRETTY_FUNCTION__,__LINE__, e.what()); \
    }									\
  } while(0)

extern "C" struct run_state{
  rfbScreenInfoPtr server;
  CInstantCameraArray *cameras;
  int count;
};



extern "C" void r_finalize(struct run_state *state);
extern "C" void r_reload(struct run_state *state);
extern "C" void r_unload(struct run_state *state);
extern "C" int r_step(struct run_state *state);

extern const int pylon,current_camera,w,h;

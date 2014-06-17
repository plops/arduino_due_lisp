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
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%hs",__func__, e.what());
    }
  }
}

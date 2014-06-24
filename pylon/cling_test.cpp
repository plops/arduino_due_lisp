/* #!
export PYLON_ROOT=/home/martin/pylon-3.2.1-x86_64/pylon3
export GENICAM_ROOT=$PYLON_ROOT/genicam
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/genicam/bin/Linux64_x64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${PYLON_ROOT}/lib64
export GENICAM_ROOT_V2_3=$PYLON_ROOT/genicam
  
/dev/shm/cling-Fedora20-x86_64-d42f77918d/bin/cling -DUSE_GIGE -L$PYLON_ROOT/lib64 -L$GENICAM_ROOT/bin/Linux64_x64 -L$GENICAM_ROOT/bin/Linux64_x64/GenApi/Generic -Wl,-E -lpylonbase -lGenApi_gcc40_v2_3 -lGCBase_gcc40_v2_3 -lLog_gcc40_v2_3 -lMathParser_gcc40_v2_3 -lXerces-C_gcc40_v2_7 -llog4cpp_gcc40_v2_3 
*/

.I /home/martin/pylon-3.2.1-x86_64/pylon3/genicam/library/CPP/include
.I /home/martin/pylon-3.2.1-x86_64/pylon3/include
.L Base/GCString.h
.L pylon/TlFactory.h
.L pylon/InstantCamera.h
.L pylon/PylonIncludes.h

using namespace Pylon;
using namespace std;

PylonInitialize()

CTlFactory &tlFactory = CTlFactory::GetInstance()
DeviceInfoList_t devices;
tlFactory.EnumerateDevices(devices)
CInstantCameraArray cameras(3);

#include <pylon/PylonIncludes.h>

using namespace Pylon;
using namespace GenApi;
using namespace std;

extern "C" {
  void pylon_wrapper_initialize()
  {
    try{
      PylonInitialize();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s",__func__, e.what());
    }
  }
  void pylon_wrapper_terminate(void*cams,void*factory)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      delete cameras;
      PylonTerminate();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s",__func__, e.what());
    }
  }
  void*pylon_wrapper_factory()
  {
    cout << "creating factory" << endl;
    CTlFactory &tlFactory = CTlFactory::GetInstance();
    return (void*) &tlFactory;
  }
  void*pylon_wrapper_create(void*factory,unsigned int maxCamerasToUse)
  {
    try{
      CTlFactory &tlFactory = (CTlFactory&) ((CTlFactory*)factory)[0];
      
      // Get all attached devices
      DeviceInfoList_t devices;
      cout << "finding devices" << endl;
      if ( tlFactory.EnumerateDevices(devices) == 0 )
	printf("%s finds no cameras: %d\n",__func__,(int)devices.size());       

      cout << "preparing camera array" << endl;
      CInstantCameraArray *cameras =
	new CInstantCameraArray(min(size_t(maxCamerasToUse),devices.size()));
      cout << "attaching cameras" << endl;
      // Create and attach all Pylon Devices.
      for ( size_t i = 0; i < cameras->GetSize(); ++i) {
	cout << "camera " << i << endl;
	(*cameras)[ i ].Attach( tlFactory.CreateDevice( devices[ i ]));
	// Print the model name of the camera.
	cout << "Using device " << (*cameras)[ i ].GetDeviceInfo().GetFullName() << endl;
      }
      return cameras;
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return NULL;
    }
  }
  int pylon_wrapper_get_max_i(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CIntegerPtr nod=control.GetNode(node);
      cout << "max(" << node << ")=" << nod->GetMax() << endl;
      return nod->GetMax();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  int pylon_wrapper_get_min_i(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CIntegerPtr nod=control.GetNode(node);
      cout << "min(" << node << ")=" << nod->GetMin() << endl;
      return nod->GetMin();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  int pylon_wrapper_get_inc_i(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CIntegerPtr nod=control.GetNode(node);
      cout << "inc(" << node << ")=" << nod->GetInc() << endl;
      return nod->GetInc();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  int pylon_wrapper_get_value_i(void*cams,int cam,const char*node,int verify, int ignore_cache)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CIntegerPtr nod=control.GetNode(node);
      return nod->GetValue(verify,ignore_cache);
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  void pylon_wrapper_set_value_i(void*cams,int cam,const char*node,int value)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CIntegerPtr nod=control.GetNode(node);
      nod->SetValue(value);
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_get_symbolics_e(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;


      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CEnumerationPtr nod=control.GetNode(node);
      StringList_t symb;
      nod->GetSymbolics(symb);
      int i;
      for(i=0;i<symb.size();i++)
	std::cout << i << " " << symb[i] << endl;
      // FIXME i can't decide how to return it, therefore, i just print the values
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_set_value_e(void*cams,int cam,const char*node,int value)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;


      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CEnumerationPtr nod=control.GetNode(node);
      nod->SetIntValue(value);
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  int pylon_wrapper_get_value_e(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CEnumerationPtr nod=control.GetNode(node);
      return nod->GetIntValue();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  void pylon_wrapper_to_string_e(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CEnumerationPtr nod=control.GetNode(node);
      cout << nod->ToString() << endl;
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_from_string_e(void*cams,int cam,const char*node,char*value)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CEnumerationPtr nod=control.GetNode(node);
      nod->FromString(value);
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_start_grabbing(void*cams)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      cameras->StartGrabbing();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_stop_grabbing(void*cams)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      cameras->StopGrabbing();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_cams_open(void*cams)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      cameras->Open();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_cams_close(void*cams)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      cameras->Close();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_cam_open(void*cams,int cam)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
      cameras[cam].Open();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  void pylon_wrapper_cam_close(void*cams,int cam)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      if(cam>=cameras->GetSize())
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
      cameras[cam].Close();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
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
	cout << "Camera " <<  cameraContextValue << ": " << (*cameras)[ cameraContextValue ].GetDeviceInfo().GetFullName() << endl;
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
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      *camera = -1;
      *w = -1;
      *h = -1;
      *success_p = -1;
    }
  }
  // buf is complex double float, fill only real part
  void pylon_wrapper_grab_cdf(void*cams,int ww,int hh,double * buf,int*camera,int*success_p,int*w,int*h)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      if(cameras->IsGrabbing()){
	CGrabResultPtr ptrGrabResult;
	cameras->RetrieveResult( 5000, ptrGrabResult, TimeoutHandling_ThrowException);
	// context allows to determine which camera produced the grab result
	intptr_t cameraContextValue = ptrGrabResult->GetCameraContext();
	*camera = cameraContextValue;
	cout << "Camera " <<  cameraContextValue << ": " << (*cameras)[ cameraContextValue ].GetDeviceInfo().GetFullName() << endl;
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
	    buf[2*(i+ (*w) * j)] = 1.0*pImageBuffer[i+(*w)*j];
      }
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      *camera = -1;
      *w = -1;
      *h = -1;
      *success_p = -1;
    }
  }
}

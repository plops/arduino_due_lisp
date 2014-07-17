#include <pylon/PylonIncludes.h>
#include <unistd.h>
#include <errno.h>

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
  void pylon_wrapper_terminate(void*cams)
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
    try{
      cout << "creating factory" << endl;
      CTlFactory &tlFactory = CTlFactory::GetInstance();
      return (void*) &tlFactory;
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s",__func__, e.what());
      return NULL;
    }
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
	cout << "camera " << i << ":";
	(*cameras)[ i ].Attach( tlFactory.CreateDevice( devices[ i ]));
	// Print the model name of the camera.
	cout << " FullName " << (*cameras)[ i ].GetDeviceInfo().GetFullName()
	     << " serial " << (*cameras)[ i ].GetDeviceInfo().GetSerialNumber()
	     << endl;
      }
      return cameras;
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return NULL;
    }
  }
  const char* pylon_wrapper_cam_get_serial_number(void*cams,int cam)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return NULL;
      }
      return (*cameras)[cam].GetDeviceInfo().GetSerialNumber().c_str();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return NULL;
    }
  }
  const char* pylon_wrapper_cam_get_full_name(void*cams,int cam)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return NULL;
      }
      return (*cameras)[cam].GetDeviceInfo().GetFullName().c_str();
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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }

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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }
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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }

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
  float pylon_wrapper_get_value_f(void*cams,int cam,const char*node,int verify, int ignore_cache)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1.0;
      }

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CFloatPtr nod=control.GetNode(node);
      return nod->GetValue(verify,ignore_cache);
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1.0;
    }
  }
  int pylon_wrapper_get_value_b(void*cams,int cam,const char*node,int verify, int ignore_cache)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1.0;
      }

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CBooleanPtr nod=control.GetNode(node);
      return nod->GetValue(verify,ignore_cache);
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1.0;
    }
  }
  int pylon_wrapper_get_value_i(void*cams,int cam,const char*node,int verify, int ignore_cache)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }

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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return ;
      }

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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return ;
      }


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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return ;
      }


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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CEnumerationPtr nod=control.GetNode(node);
      return nod->GetIntValue();
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  int pylon_wrapper_command_execute(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CCommandPtr nod=control.GetNode(node);
      nod->Execute();
      return 0;
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
      return -1;
    }
  }
  int pylon_wrapper_command_isdone(void*cams,int cam,const char*node)
  {
    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams; 
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return -1;
      }

      INodeMap &control = (*cameras)[cam].GetNodeMap();
      const CCommandPtr nod=control.GetNode(node);
      return nod->IsDone();
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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return ;
      }

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
      if(cam>=cameras->GetSize()) {
      	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return ;
      }

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
      if(cam>=cameras->GetSize()){
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return;
      }
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
      if(cam>=cameras->GetSize()){
	cout << "there are not as many cameras available as requested: "
	     << cam << ">=" << cameras->GetSize() << endl;
	return ;
      }
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
	  *camera = -1;
	  *w = -1;
	  *h = -1;
	  *success_p = -1;

	  return;
	}
	cout << "SizeX: " << ptrGrabResult->GetWidth() << endl;
	cout << "SizeY: " << ptrGrabResult->GetHeight() << endl;
	const uint8_t *pImageBuffer = (uint8_t *) ptrGrabResult->GetBuffer();
	cout << "Gray value of first pixel: " << (uint32_t) pImageBuffer[0] << endl << endl;

	*w = ptrGrabResult->GetWidth();
	*h = ptrGrabResult->GetHeight(); // FIXME this doesnt work with 12 bit
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
  // store image into file
  void pylon_wrapper_grab_store(void*cams,int nfd,int*fd,int*camera,int*success_p,
				int*w,int*h,int*framenr)
  {
    *camera = -1;
    *w = -1;
    *h = -1;
    *success_p = -1;
    *framenr = -1;

    try{
      CInstantCameraArray *cameras = (CInstantCameraArray*)cams;
      if(cameras->IsGrabbing()){
	CGrabResultPtr ptrGrabResult;
	cameras->RetrieveResult( 5000, ptrGrabResult, TimeoutHandling_ThrowException);
	// context allows to determine which camera produced the grab result
	intptr_t cameraContextValue = ptrGrabResult->GetCameraContext();
	*camera = cameraContextValue;
	cout << "Camera " <<  cameraContextValue << ": " << (*cameras)[ cameraContextValue ].GetDeviceInfo().GetFullName() << endl;
	cout << "GrabSucceeded: " << ptrGrabResult->GrabSucceeded() 
	     << " fnr=" << ptrGrabResult->GetFrameNumber() 
	     << " ts=" << ptrGrabResult->GetTimeStamp()  
	     << " id=" << ptrGrabResult->GetID()  
	     << " inr=" << ptrGrabResult->GetImageNumber()  
	     << " skip=" << ptrGrabResult->GetNumberOfSkippedImages()  << endl;
	*success_p = ptrGrabResult->GrabSucceeded();
	if(!ptrGrabResult->GrabSucceeded()){
	  std::cout << "Error: " << ptrGrabResult->GetErrorCode() << " " << ptrGrabResult->GetErrorDescription();
	  return;
	}
	cout << "SizeX: " << ptrGrabResult->GetWidth() << endl;
	cout << "SizeY: " << ptrGrabResult->GetHeight() << endl;
	const uint8_t *pImageBuffer = (uint8_t *) ptrGrabResult->GetBuffer();
	cout << "Gray value of first pixel: " << (uint32_t) pImageBuffer[0] << endl << endl;

	int n = ptrGrabResult->GetPayloadSize();
	if(0<=cameraContextValue && cameraContextValue < nfd){
	  int ret = write(fd[cameraContextValue],pImageBuffer,n);
	  if(n != ret){
	    cout << "write error: fd["<< cameraContextValue << "]=" << fd[cameraContextValue] <<  " n=" << n << " != " << ret << endl;
	    if(ret==-1)
	      cout << "errno: " << errno << " " << strerror(errno) << endl;
	    *success_p = -3;
	  }
	}
	else
	  *success_p = -2;
      }
    }
    catch (GenICam::GenericException& e) {
      printf( "Exception caught in %s msg=%s\n",__func__, e.what());
    }
  }
  // buf is complex double float, fill only real part
  void pylon_wrapper_grab_cdf(void*cams,int ww,int hh,double * buf,int*camera,int*success_p,int*w,int*h,int*framenr)
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
	cout << "GrabSucceeded: " << ptrGrabResult->GrabSucceeded() 
	     << " fnr=" << ptrGrabResult->GetFrameNumber() 
	     << " ts=" << ptrGrabResult->GetTimeStamp()  
	     << " id=" << ptrGrabResult->GetID()  
	     << " inr=" << ptrGrabResult->GetImageNumber()  
	     << " skip=" << ptrGrabResult->GetNumberOfSkippedImages()  << endl;

	*success_p = ptrGrabResult->GrabSucceeded();
	*framenr = ptrGrabResult->GetFrameNumber();
	if(!ptrGrabResult->GrabSucceeded()){
	  std::cout << "Error: " << ptrGrabResult->GetErrorCode() << " " << ptrGrabResult->GetErrorDescription();
	  *camera = -1;
	  *w = -1;
	  *h = -1;
	  *success_p = -1;
	  *framenr = -1;
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

              // (loop for byte below (get-payload cam) by 3
	      // 	and short from 0 below n by 2
	      // 	do
	      // 	;; 3 bytes in the data stream correspond to two data
	      // 	;; elements: AB CD EF -> ABD, EFC 
	      // 	;; A is the most signifcant nibble
	      // 	  (let ((ab (%get-unsigned-byte data byte))
	      // 		(c (ldb (byte 4 0) (%get-unsigned-byte data (+ 1 byte))))
	      // 		(d (ldb (byte 4 4) (%get-unsigned-byte data (+ 1 byte))))
	      // 		(ef (%get-unsigned-byte data (+ 2 byte))))
	      // 	    (setf (aref a1 short) (ash (+ (ash ab 4) d) 4)
	      // 		  (aref a1 (1+ short)) (ash (+ (ash ef 4) c) 4))))

	int i,j;
	// convert mono12p into real part of complex double float
	// i .. index for byte
	// j .. index for 12bit
	for(i=0,j=0;j< (*w)*(*h);i+=3,j+=2) {
	  unsigned char 
	    ab = pImageBuffer[i],
	    c = pImageBuffer[i+1] & 0x0f,
	    d = (pImageBuffer[i+1] & 0xf0)>>4,
	    ef = pImageBuffer[i+2];
	  buf[2*((j%(*w))+ (*w) * (j/(*w)))] = 1.0*((ab<<4)+d);
	  buf[2*(((j+1)%(*w))+ (*w) * ((j+1)/(*w)))] = 1.0*((ef<<4)+c);
	}
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
  void pylon_wrapper_grab_sf(void*cams,int ww,int hh,float * buf,int*camera,int*success_p,int*w,int*h,int*framenr)
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
	cout << "GrabSucceeded: " << ptrGrabResult->GrabSucceeded() 
	     << " fnr=" << ptrGrabResult->GetFrameNumber() 
	     << " ts=" << ptrGrabResult->GetTimeStamp()  
	     << " id=" << ptrGrabResult->GetID()  
	     << " inr=" << ptrGrabResult->GetImageNumber()  
	     << " skip=" << ptrGrabResult->GetNumberOfSkippedImages()  << endl;

	*success_p = ptrGrabResult->GrabSucceeded();
	*framenr = ptrGrabResult->GetFrameNumber();
	if(!ptrGrabResult->GrabSucceeded()){
	  std::cout << "Error: " << ptrGrabResult->GetErrorCode() << " " << ptrGrabResult->GetErrorDescription();
	  *camera = -1;
	  *w = -1;
	  *h = -1;
	  *success_p = -1;
	  *framenr = -1;
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
	// convert mono12p into real part of complex double float
	// i .. index for byte
	// j .. index for 12bit
	for(i=0,j=0;j< (*w)*(*h);i+=3,j+=2) {
	  unsigned char 
	    ab = pImageBuffer[i],
	    c = pImageBuffer[i+1] & 0x0f,
	    d = (pImageBuffer[i+1] & 0xf0)>>4,
	    ef = pImageBuffer[i+2];
	  buf[j] = 1.0*((ab<<4)+d);
	  buf[j+1] = 1.0*((ef<<4)+c);
	}
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

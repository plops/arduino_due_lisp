#include "myinc.h"
extern "C" int r_step(struct run_state *state)
{
  //  printf("step\n");
  if(!rfbIsActive(state->server))
    return 0;
  int ma=0, mi=5000;
  if(pylon){
    if(state->cameras && state->cameras->IsGrabbing()){
      CGrabResultPtr res;
      int ret,gi=0;
      do{
  	gi++;
  	d(ret = state->cameras->RetrieveResult( 90, res, TimeoutHandling_ThrowException);
  	if(ret==0){
  	  printf(".");
  	  fflush(stdout);
  	});
      } while (ret != 0 && gi<10);
      if(!res.IsValid()){
  	printf("error no image grabbed\n");
  	return 1;
      }
      
      // When the cameras in the array are created the camera context value
      // is set to the index of the camera in the array.
      // The camera context is a user settable value.
      // This value is attached to each grab result and can be used
      // to determine the camera that produced the grab result.
      intptr_t cameraContextValue = res->GetCameraContext();
      // Print the index and the model name of the camera.
      //f(cout << "Camera " << cameraContextValue << ": " 
      //<< (*(state->cameras))[ cameraContextValue ].GetDeviceInfo().GetFullName() << endl);
      // Now, the image data can be processed.
      if(cameraContextValue==current_camera){
	//f(cout << "GrabSucceeded: " << res->GrabSucceeded() << endl);
	int ww = res->GetWidth(), hh = res->GetHeight();
	//f(cout << "Size: " << ww << "x" << hh << endl);
	
	const uint8_t *im = (uint8_t *) res->GetBuffer();
	int i,j;
	char *b=state->server->frameBuffer;
	/// convert mono12p into real part of complex double float
	// i .. index for byte
	// j .. index for 12bit
	static int oma,omi;

	CImg<float> img(ww,hh,1);
	float* imgp = img.data();
	for(i=0,j=0;j< ww*hh;i+=3,j+=2) {
	  unsigned char
	    ab = im[i],  	  c = im[i+1] & 0x0f,
	    d = (im[i+1] & 0xf0)>>4,
	    ef = im[i+2];
	  int
	    p0= ((j%ww)+ w * (j/ww)), 
	    q0= (((j+1)%ww)+ w * ((j+1)/ww)),
	    p=4*p0,
	    q=4*q0;
	  int v1 = (ab<<4)+d, v2 = (ef<<4)+c;
	  mi = min(mi,min(v1,v2));
	  ma = max(ma,max(v1,v2));
	  //omi = 8;
	  //oma = 4095;
	  b[p+0]=b[p+1]=b[p+2]=(unsigned char)min(255.0,max(0.0,(255.*(v1-omi)/(1.0*(oma-omi)))));
	  b[q+0]=b[q+1]=b[q+2]=(unsigned char)min(255.0,max(0.0,(255.*(v2-omi)/(1.0*(oma-omi)))));
	  imgp[((j%ww)+ ww * (j/ww))]=v1;
	  imgp[(((j+1)%ww)+ ww * ((j+1)/ww))]=v2;
	}
	oma = ma;
	omi = mi;

	CImgList<float> F = img.get_FFT();
        //cimglist_apply(F,shift)(img.width()/2,img.height()/2,0,0,2);
	// //	cout << "min " << ((F[0].get_pow(2) + F[1].get_pow(2)).sqrt() + 1).log().min()
	// //     << " max "  << (((F[0].get_pow(2) + F[1].get_pow(2)).sqrt() + 1).log()*-1).min()*-1 << endl;
	CImg<float> fmag = ((F[0].get_pow(2) + F[1].get_pow(2)).sqrt() + 1).blur_median(3).log().normalize(0,255);

	//cimg_rof(fmag,p,float) {
	//  const float m=8.3f, M=14.0f;
	//  float v = (float) 255.0f*(*p-m)/(M-m);
	//  *p = (v<0.0f)?0.0f:(v>255.0)?255.0:v;
	//};

	const float*buf=fmag.data();
	
	for(i=0;i<ww;i++)
	  for(j=0;j<hh;j++){
	    int p = i+ww+w*j;
	    b[4*p+0] = b[4*p+1] = b[4*p+2] = b[4*p+3] = (unsigned char)buf[i+ww*j];
	  }

	char s[100];
	snprintf(s,100,"count: %d max %d min %d\n",state->count++,ma,mi);
	rfbDrawString(state->server,&radonFont,20,270,s,0xffffff);

      }
    }
  }
  rfbMarkRectAsModified(state->server,0,0,w,h);
  long usec = state->server->deferUpdateTime*1000;
  rfbProcessEvents(state->server,usec);
  
  return 1; 
}

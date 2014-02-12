#include <rfb/rfb.h>

enum {W=512, H=512};

int main(int argc,char** argv)
{                                                                
  rfbScreenInfoPtr server=rfbGetScreen(&argc,argv,W,H,8,3,4);
  if(!server)
    return 0;
  server->frameBuffer=(char*)malloc(W*H*4);
  server->alwaysShared=(1==1);
  rfbInitServer(server);
  


#if !defined(LIBVNCSERVER_HAVE_LIBPTHREAD)
#error "I need pthreads for that."
#endif
  rfbRunEventLoop(server,-1,TRUE);
  fprintf(stderr, "Running background loop...\n");

  int i;
  while (rfbIsActive(server)) {
    i++;
    if(i>255)
      i=0;
    int j;
    for(j=0;j<W*H;j++)
      server->frameBuffer[4*j]=i;
    for(j=0;j<W;j++)
      server->frameBuffer[4*(j+W*i)]=255;
    rfbMarkRectAsModified(server,0,0,W,H);
    long usec = server->deferUpdateTime*1000;
    rfbProcessEvents(server,usec);
  }

  return(0);
}

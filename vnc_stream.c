#include <complex.h>
#include <math.h>
#include <fftw3.h>
#include <sys/inotify.h>
#include <rfb/rfb.h>
#include <errno.h>


/* size of the event structure, not counting name */
#define EVENT_SIZE  (sizeof (struct inotify_event))

/* reasonable guess as to size of 1024 events */
#define BUF_LEN        (1024 * (EVENT_SIZE + 16))

fftw_complex *fft_in[2], *fft_out[2];
fftw_plan fft_plan[2];
int width[2],height[2];
unsigned short*image[2];
unsigned char*kspace[2];

int initialized[]={0,0};


const int do_fft=1;

void read_pgm(char*fn,int i)
{
  printf("reading %s into image %d\n",fn,i);
  FILE*f=fopen(fn,"r");
  if(!f){
    usleep(2000); // sometimes the file does not exist
    f=fopen(fn,"r");
    if(!f)
      printf("error with fopen: '%s' %s\n",fn,strerror(errno));
  }
  rewind(f);
  if(2!=fscanf(f,"P5\n%d %d\n65535\n", width+i, height+i))
    printf("error with fscanf\n");
  if(!initialized[i]){
    printf("allocating image %d %dx%d\n",i,width[i],height[i]);
    image[i]=malloc(width[i]*height[i]*2);
    kspace[i]=malloc(width[i]*height[i]*2);
    initialized[i]=1;
  }
  printf("reading data ..");
  fread(image[i],width[i]*height[i],2,f);
  printf(". finished\n");
  fclose(f);
}

void pgm_init()
{
  read_pgm("/dev/shm/1.pgm",0);
  read_pgm("/dev/shm/2.pgm",1);
}

void fft_init()
{
  fftw_init_threads();
  fftw_plan_with_nthreads(6);

  int i;
  for(i=0;i<2;i++){
    fft_in[i] = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * width[i]*height[i]);
    fft_out[i] = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * width[i]*height[i]);
    fft_plan[i]=fftw_plan_dft_2d(height[i],width[i],fft_in[i],fft_out[i],
				 FFTW_FORWARD, FFTW_ESTIMATE);
  }
}

void fft_fill()
{
  int i; 
  double s=1/65535.0;
  read_pgm("/dev/shm/1.pgm",0);
  for(i=0;i<width[0]*height[0];i++){
    fft_in[0][i]=image[0][i]*s;
  }
 read_pgm("/dev/shm/2.pgm",1);
  for(i=0;i<width[1]*height[1];i++){
    fft_in[1][i]=image[1][i]*s;
  }
}

void fft_run()
{
  int i;
  for(i=0;i<2;i++)
    fftw_execute(fft_plan[i]);

  int j;
  for(j=0;j<2;j++)
    for(i=0;i<width[j]*height[j];i++){
      double v=10*cabs(fft_out[j][i]);
      kspace[j][i]=(v<255)?(unsigned char)v:255; 
    }
}

int main(int argc,char** argv)
{                                                                
  int fd=inotify_init();
  int in=inotify_add_watch(fd,"/dev/shm/",
			   IN_CLOSE_WRITE );
  char buf[BUF_LEN];
  pgm_init();
  fft_init();

  int w=width[0]+width[1], h=(int)fmax(height[0],height[1]);

  if(do_fft){
    h=2*h;
  }

  rfbScreenInfoPtr server=rfbGetScreen(&argc,argv,w,h,8,3,4);
  if(!server)
    return 0;
  server->frameBuffer=(char*)malloc(w*h*4);
  server->alwaysShared=(1==1);
  rfbInitServer(server);
  
  while (rfbIsActive(server)) {
    int len, i = 0;
    usleep(2000);
    len = read (fd, buf, BUF_LEN);
    
    if(len<=0)
      printf("error\n");
    
    while (i < len) {
      struct inotify_event *event;
      event = (struct inotify_event *) &buf[i];
      printf ("wd=%d mask=%u cookie=%u len=%u ",
	      event->wd, event->mask,
	      event->cookie, event->len);
      if (event->len)
	printf ("name=%s\n", event->name);
      else
	printf("\n");
      
      i += EVENT_SIZE + event->len;
    }

    fft_fill();
    if(do_fft){
      fft_run();
    }
    int j;
    for(j=0;j<height[0];j++)
      for(i=0;i<width[0];i++){
	int p=4*(i+w*j);
	server->frameBuffer[p+0]=server->frameBuffer[p+1]=server->frameBuffer[p+2]=
	  (int)(image[0][i+width[0]*j]/65535.0*255.0);
     } 

    if(do_fft)
      for(j=0;j<height[0];j++)
	for(i=0;i<width[0];i++){
	  int p=4*(i+w*(j+height[0]));
	  server->frameBuffer[p+0]=server->frameBuffer[p+1]=server->frameBuffer[p+2]=
	  kspace[0][i+width[0]*j];
	}
        
    for(j=0;j<height[1];j++)
      for(i=0;i<width[1];i++){
	int p=4*((width[0]+i)+w*j);
	server->frameBuffer[p+0]=server->frameBuffer[p+1]=server->frameBuffer[p+2]=
	  (int)(image[1][i+width[1]*j]/65535.0*255.0);
      }

    if(do_fft)
      for(j=0;j<height[1];j++)
	for(i=0;i<width[1];i++){
	  int p=4*((width[0]+i)+w*(j+height[1]));
	  server->frameBuffer[p+0]=server->frameBuffer[p+1]=server->frameBuffer[p+2]=
	  kspace[1][i+width[1]*j];
	}
    
    printf("new frame\n");
    rfbMarkRectAsModified(server,0,0,w,h);
    long usec = server->deferUpdateTime*1000;
    rfbProcessEvents(server,usec);
  }
  
  return(0);
}

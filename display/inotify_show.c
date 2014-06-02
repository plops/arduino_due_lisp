#include <GLFW/glfw3.h>
#include <complex.h>
#include <math.h>
#include <fftw3.h>
#include <string.h>

#include <sys/inotify.h>
#include <stdio.h>
#include <stdlib.h>
/* size of the event structure, not counting name */
#define EVENT_SIZE  (sizeof (struct inotify_event))

/* reasonable guess as to size of 1024 events */
#define BUF_LEN        (1024 * (EVENT_SIZE + 16))

fftw_complex *fft_in[2], *fft_out[2];
fftw_plan fft_plan[2];
int width[2],height[2];
unsigned short*image[2];
unsigned short*kspace[2];

int initialized[]={0,0};
void read_pgm(char*fn,int i)
{
  usleep(16000);
  printf("reading %s into image %d\n",fn,i);
  FILE*f=fopen(fn,"r");
  if(!f)
    printf("error with fopen\n");
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
      double v=1000*cabs(fft_out[j][i]);
      kspace[j][i]=(v<65535)?(unsigned short)v:65535; 
    }
}

void draw_quad(int obj, int x,int y,int w,int h){
  glBindTexture( GL_TEXTURE_2D, obj );
  glBegin(GL_QUADS);
  glVertex2i(x,y); glTexCoord2i(0,0);
  glVertex2i(x,h+y); glTexCoord2i(1,0);
  glVertex2i(x+w,h+y); glTexCoord2i(1,1);
  glVertex2i(x+w,y); glTexCoord2i(0,1);
  glEnd();
}

int main()
{
  int fd=inotify_init();
  int in=inotify_add_watch(fd,"/dev/shm",IN_CLOSE_WRITE);
  char buf[BUF_LEN];
  pgm_init();
  fft_init();
  if (!glfwInit())
    exit(EXIT_FAILURE);
  GLFWwindow* window = glfwCreateWindow(width[0]+width[1], (int)fmax(2*height[0],2*height[1]),
					"gig-e-camera", NULL, NULL);
  glfwMakeContextCurrent(window);
  const int n_tex=4;
  GLuint texture[n_tex];
  glGenTextures( n_tex, texture );
  int i;
  for(i=0;i<n_tex;i++){
    glBindTexture( GL_TEXTURE_2D, texture[i] );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                     GL_NEAREST );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    glTexImage2D(GL_TEXTURE_2D,0,GL_LUMINANCE,width[i/2],height[i/2],0,
		 GL_LUMINANCE,GL_UNSIGNED_SHORT,0);
  }  
  glEnable(GL_TEXTURE_2D);
 
  while (1){
    int len, i = 0;
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
    
          {
	int i;
	fft_fill(); fft_run();
	for(i=0;i<2;i++){
	  glBindTexture( GL_TEXTURE_2D, texture[2*i] );
	  glTexSubImage2D(GL_TEXTURE_2D,0,0,0,width[i],height[i],GL_LUMINANCE,GL_UNSIGNED_SHORT,kspace[i]);
	  glBindTexture( GL_TEXTURE_2D, texture[2*i+1] );
	  glTexSubImage2D(GL_TEXTURE_2D,0,0,0,width[i],height[i],GL_LUMINANCE,GL_UNSIGNED_SHORT,image[i]);
	}
	{
	  int win_width,win_height;
	  glfwGetFramebufferSize(window, &win_width, &win_height);
	  glViewport(0, 0, win_width, win_height);
	}
	
	glClear(GL_COLOR_BUFFER_BIT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, width[0]+width[1], 0,fmax(2*height[0],2*height[1]), 1.f, -1.f);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glColor3f(1.f,1.f,1.f);
	draw_quad(texture[0],0,0,width[0],height[0]);
	draw_quad(texture[1],0,height[0],width[0],height[0]);
	draw_quad(texture[2],width[0],0,width[1],height[1]);
	draw_quad(texture[3],width[0],height[1],width[1],height[1]);
	
	glfwSwapBuffers(window);
      }

  }
  glfwTerminate();
}




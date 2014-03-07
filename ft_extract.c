#include <complex.h>
#include <math.h>
#include <fftw3.h>
#include <stdio.h>

unsigned short *image;
int initialized_p=0;
int image_w=0,image_h=0;

int read_pgm(char*fn)
{
  printf("reading %s\n",fn);
  FILE*f=fopen(fn,"r");
  if(!f){
    usleep(2000); // sometimes the file does not exist
    f=fopen(fn,"r");
    if(!f)
      printf("error with fopen: '%s' %s\n",fn,strerror(errno));
  }
  rewind(f);
  if(2!=fscanf(f,"P5\n%d %d\n65535\n", &image_w, &image_h))
    printf("error with fscanf\n");
  if(!initialized_p){
    printf("allocating image %d %dx%d\n",i,image_w,image_h);
    image=(unsigned short*)malloc(image_w*image_h*2);
    initialized_p=1;
    image_w=image_w;
    image_h=image_h;
  }
  printf("reading data ..");
  if(image_w<image_w || image_h<image_h)
    return -1;
  fread(image,image_w*image_h,2,f);
  printf(". finished\n");
  fclose(f);
  return 0;
}

int cx,cy,cw,ch;

int
main(int argc,char**argv)
{
  if(argc<6)
    printf("usage: ft_extract x y w h file1 [file2 ...]\n");
  cx = atoi(argv[1]); // FIXME this is not safe at all, but i'm the only one using this
  cy = atoi(argv[2]);
  cw = atoi(argv[3]);
  ch = atoi(argv[4]);

  read_pgm(argv[5]);

  fftw_init_threads();
  fftw_plan_with_nthreads(4);
  fftw_complex fft_in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * image_w * image_h);
  fftw_complex fft_out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) *  image_w * image_h);
  fftw_plan fft_plan =fftw_plan_dft_2d(image_h,image_w,fft_in,fft_out,FFTW_FORWARD, FFTW_ESTIMATE);
  
  fftw_complex fft_in_b = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * cw * ch);
  fftw_complex fft_out_b = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * cw * ch);
  fftw_plan fft_plan_b =fftw_plan_dft_2d(ch,cw,fft_out_b,fft_in_b,FFTW_BACKWARD, FFTW_ESTIMATE);

  double s=1/65535.0;

  int v;
  for(v=5;v<argc;v++){
    int i,j;
    for(i=0;i<image_w*image_h;i++)
      fft_in[i]=image[i]*s;
    fftw_execute(fft_plan);
    
    for(i=0;i<cw;i++)
      for(j=0;j<ch;j++)
	fft_out_b[i+cw*j]=fft_out[(cx+i)+image_w*(cy+j)];
    
    fftw_execute(fft_plan_b); 
  }
}

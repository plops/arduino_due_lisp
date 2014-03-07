#include <complex.h>
#include <math.h>
#include <fftw3.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

// http://libics.sourceforge.net/
// sudo apt-get install libics-dev
#include <libics.h>

unsigned short *image;
int initialized_p=0;
int image_w=0,image_h=0;

//#define D(x) do{x}while(0)
#define D(x) do{if(0){x;}}while(0)

int read_pgm(char*fn)
{
  D(printf("reading %s\n",fn));
  FILE*f=fopen(fn,"r");
  if(!f){
    f=fopen(fn,"r");
    if(!f)
      printf("error with fopen: '%s' %s\n",
	       fn,
	       strerror(errno));
  }
  rewind(f);
  if(2!=fscanf(f,"P5\n%d %d\n65535\n", &image_w, &image_h))
    printf("error with fscanf\n");
  if(!initialized_p){
    printf("allocating image %dx%d\n",image_w,image_h);
    image=(unsigned short*)malloc(image_w*image_h*2);
    initialized_p=1;
    image_w=image_w;
    image_h=image_h;
  }
  D(printf("reading data .."));
  if(image_w<image_w || image_h<image_h)
    return -1;
  int n = fread(image,2,image_w*image_h,f);
  if(n<image_h*image_w)
    printf("fread read %d elements which is not the expected %d.\n",n,image_h*image_w);
  D(printf(". finished\n"));
  fclose(f);
  return 0;
}

int write_ics2(char*fn,int w, int h, int depth,void*buf)
{
  ICS* ip;
  Ics_DataType dt=Ics_complex32;
  int ndims=3;
  size_t dims[3]={w,h,depth};
  int retval = IcsOpen (&ip, fn, "w2");
  if (retval != IcsErr_Ok) {
    fprintf (stderr, "Could not open output file: %s\n", IcsGetErrorText (retval));
    return(-1);
  }
  IcsSetLayout (ip, dt, ndims, dims);
  IcsSetData (ip, buf, w*h*depth*sizeof(fftw_complex));
  IcsSetCompression (ip, IcsCompr_uncompressed, 0);
  retval = IcsClose (ip);
  if (retval != IcsErr_Ok) {
    fprintf (stderr, "Could not write output file: %s\n", IcsGetErrorText (retval));
    return(-1);
  }
  return 0;
}

// split at /
char *gnu_basename(char *path)
{
  char *base = strrchr(path, '/');
  return base ? base+1 : path;
}

// input: "i2087_j1967_2_000082.00.pgm"
// output: 000082.00 (as double)
double parse_exposure(char *file)
{
  char *last = strrchr(file, '.');
  char *first = strrchr(file,'_')+1;
  char s[30];
  char*p,*q;
  for(p=first,q=s;p<last;q++,p++){
    *q=*p;
  }
  return atof(s);
}

int cx,cy,cw,ch;

int
main(int argc,char**argv)
{
  if(argc<7)
    printf("usage: ft_extract x y w h output.ics file1 [file2 ...]\n");
  cx = atoi(argv[1]); // FIXME this is not safe at all, but i'm the only one using this
  cy = atoi(argv[2]);
  cw = atoi(argv[3]);
  ch = atoi(argv[4]);

  read_pgm(argv[6]);

  fftw_init_threads();
  fftw_plan_with_nthreads(6);
  fftw_complex*fft_in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * image_w * image_h);
  fftw_complex*fft_out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) *  image_w * image_h);
  fftw_plan fft_plan =fftw_plan_dft_2d(image_h,image_w,fft_in,fft_out,FFTW_FORWARD, FFTW_ESTIMATE);
  
  fftw_complex*fft_in_b = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * cw * ch);
  fftw_complex*fft_out_b = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * cw * ch);
  fftw_plan fft_plan_b =fftw_plan_dft_2d(ch,cw,fft_out_b,fft_in_b,FFTW_BACKWARD, FFTW_ESTIMATE);

  int depth=argc-6;
  int vol_size=sizeof(complex float)*cw*ch*depth;
  printf("allocating %d bytes (%g Gbytes) for %dx%dx%d complex float volume\n",
	 vol_size,(1.0*vol_size)/(1024*1024*1024),cw,ch,depth);
  complex float*vol=(complex float*) fftw_malloc(vol_size);

  int v;
  D(printf("argc=%d\n",argc));
  for(v=0;v<6;v++){
    D(printf("%s\n",argv[v]));
  }
  for(v=6;v<argc;v++){
    int i,j;
    read_pgm(argv[v]);
    double s=1.0/parse_exposure(argv[v]);
    for(i=0;i<image_w*image_h;i++)
      fft_in[i]=image[i]*s;

    fftw_execute(fft_plan);
   
    for(i=0;i<cw;i++)
      for(j=0;j<ch;j++)
	fft_out_b[i+cw*j]=fft_out[(cx+i-(int)floor(cw/2))+
				  image_w*(cy+j-(int)floor(ch/2))];
    
    fftw_execute(fft_plan_b); 
    for(i=0;i<cw*ch;i++)
      vol[i+cw*ch*(v-6)] = fft_in_b[i];
  }
  write_ics2(argv[5],cw,ch,depth,vol);
      
  return 0;
}

// 2 x=384-256 y=442-256 w=73

// cp ~/dat/0/i0827_j2047_2_000160.00.pgm /dev/shm/2.pgm
// ./ft_extract 99 140 80 84 /dev/shm/2.pgm

// uses 20% of processors, reads data with 8Mbyte/s
// uses 60% of processor when data is in ram

// for i in `ls ~/dat/0/i*_2_*.pgm|xargs -n1 basename|cut -d _ -f 1|uniq`;do time ./ft_extract 99 140 80 84 /media/b/$i.ics ~/dat/0/$i"_j"*"_2_"*.pgm;done 

// on the ssd pigz -1 runs at 40Mbytes/s but it only compress 2.5Gb to 1.9Gb

// with an empty institute i can copy at 11Mbytes/s from lab computer to notebook (too slow)

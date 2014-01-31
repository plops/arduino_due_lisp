//  gcc -g -O2 -o arv-example arvexample.c -MD -MP -MF -pthread -I/usr/include/aravis-0.4 -I/usr/lib/glib-2.0/include  -I/usr/include/glib-2.0  -lm -L/usr/lib -lgio-2.0 -lgobject-2.0 -lxml2 -lgthread-2.0 -pthread -lrt -lglib-2.0 -lz  -laravis-0.4 -lglfw
#include <GLFW/glfw3.h>
#include <complex.h>
#include <math.h>
#include <fftw3.h>
#include <string.h>
#include <arv.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>

enum { W = 658, H = 494, NFIFO=4};

int fifo=0;
unsigned short store[W*H*NFIFO],store2[W*H];

/* pthread_mutex_t mutex_texture = PTHREAD_MUTEX_INITIALIZER; */
/* pthread_cond_t  condition_new_image   = PTHREAD_COND_INITIALIZER; */
/* pthread_mutex_t mutex_dontquit = PTHREAD_MUTEX_INITIALIZER; */
/* int dontquit=1; */

/* void write_dontquit(int b) */
/* { */
/*   pthread_mutex_lock( &mutex_texture ); */
/*   dontquit=b; */
/*   pthread_mutex_unlock( &mutex_texture ); */
/* } */


/* int read_dontquit() */
/* { */
/*   pthread_mutex_lock( &mutex_texture ); */
/*   int b=dontquit; */
/*   pthread_mutex_unlock( &mutex_texture ); */
/*   return b; */
/* } */

fftw_complex *fft_in, *fft_out;
fftw_plan fft_plan;

void fft_init()
{
  fft_in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * W*H);
  fft_out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * W*H);
  fftw_init_threads();
  fftw_plan_with_nthreads(4);
  fft_plan=fftw_plan_dft_2d(H,W,fft_in, fft_out, FFTW_FORWARD, FFTW_ESTIMATE);
}

void fft_fill(int fifo_now)
{
  int i;
  double s=1/65535.0;
  for(i=0;i<W*H;i++){
    fft_in[i]=store[i+fifo_now*W*H]*s;
  }
}

void fft_run()
{
  fftw_execute(fft_plan);
  int i;
  for(i=0;i<W*H;i++){
    double v=1000*cabs(fft_out[i]); // (8000*log(1+cabs(fft_out[i])));
    store2[i]=(v<65535)?(unsigned short)v:65535; 
  }
}


void draw_quad(int obj, int y){
  glBindTexture( GL_TEXTURE_2D, obj );
  glBegin(GL_QUADS);
  glVertex2i(0,y); glTexCoord2i(0,0);
  glVertex2i(0,H+y); glTexCoord2i(1,0);
  glVertex2i(W,H+y); glTexCoord2i(1,1);
  glVertex2i(W,y); glTexCoord2i(0,1);
  glEnd();
}

void* gl(void*ignore)
{
  fft_init();
  if (!glfwInit())
    exit(EXIT_FAILURE);
  GLFWwindow* window = glfwCreateWindow(W, 2*H, "gig-e-camera", NULL, NULL);
  glfwMakeContextCurrent(window);
  const int n_tex=2;
  GLuint texture[n_tex];
  glGenTextures( n_tex, texture );
  int i;
  for(i=0;i<n_tex;i++){
    glBindTexture( GL_TEXTURE_2D, texture[i] );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                     GL_NEAREST );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  
  //glMatrixMode(GL_COLOR);
  //glPushMatrix();
  //glLoadIdentity();
  //double s=10000*65536/2048.0;
  //double o=0;
  //glScaled(s,s,s);
  //glTranslated(-o,-o,-o);
    glTexImage2D(GL_TEXTURE_2D,0,GL_LUMINANCE,W,H,0,GL_LUMINANCE,GL_UNSIGNED_SHORT,0);
  //glPopMatrix();
  //glMatrixMode(GL_MODELVIEW);
  }  
  glEnable(GL_TEXTURE_2D);
  
  while (1 //read_dontquit()
	 ) {
    //pthread_mutex_lock( &mutex_texture );
    // Wait while reader function new_buffer_cb copies data
    // mutex unlocked if condition variable in new_buffer_cb  signaled.
    //pthread_cond_wait( &condition_new_image, &mutex_texture );
    glBindTexture( GL_TEXTURE_2D, texture[0] );
    int fifo_now=fifo;
    glTexSubImage2D(GL_TEXTURE_2D,0,0,0,W,H,GL_LUMINANCE,GL_UNSIGNED_SHORT,store+fifo_now*W*H);
    

    /*    FILE*f=fopen("/dev/shm/bla.pgm","w");
    fprintf(f,"P5\n658 494\n65535\n");
    fwrite(store,sizeof(store),1,f);
    fclose(f);
    */

    //pthread_mutex_unlock( &mutex_texture );
    
    fft_fill(fifo_now); fft_run();
    glBindTexture( GL_TEXTURE_2D, texture[1] );
    glTexSubImage2D(GL_TEXTURE_2D,0,0,0,W,H,GL_LUMINANCE,GL_UNSIGNED_SHORT,store2);


    int width,height;
    glfwGetFramebufferSize(window, &width, &height);
    //float ratio = width / (float) height;
    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    //glOrtho(-ratio, ratio, -1.f, 1.f, 1.f, -1.f);
    glOrtho(0, W, 0,2*H, 1.f, -1.f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    //glRotatef((float) glfwGetTime() * 50.f, 0.f, 0.f, 1.f);
    glColor3f(1.f,1.f,1.f);
    draw_quad(texture[0],0);
    draw_quad(texture[1],H);
     
    glfwSwapBuffers(window);
    usleep(164000);
  }

  glfwTerminate();
}

typedef struct {
	GMainLoop *main_loop;
	int buffer_count;
} ApplicationData;

static gboolean cancel = FALSE;

static void
set_cancel (int signal)
{
	cancel = TRUE;
}
int count=0;

#define min(a,b) (((a)<(b))?(a):(b))

static void
new_buffer_cb (ArvStream *stream, ApplicationData *data)
{
	ArvBuffer *buffer;

	buffer = arv_stream_try_pop_buffer (stream);
	if (buffer != NULL) {
		if (buffer->status == ARV_BUFFER_STATUS_SUCCESS)
			data->buffer_count++;
		/* Image processing here */
		/*
		char s[100];
		snprintf(s,sizeof(s),"/dev/shm/dat/o%04d.pgm",count);
		FILE*f=fopen(s,"w");
		fprintf(f,"P5\n658 494\n65535\n");
		count++;
		fwrite(buffer->data,buffer->size,1,f);
		fclose(f);
		*/
		//pthread_mutex_lock(&mutex_texture);
	      
		int i,byte;
		//printf("%d %d\n",W*H,buffer->size);
		unsigned char *buf=buffer->data;
		/* for(i=0,byte=0; byte<buffer->size;i+=2, byte+=3){ */
		/*   char  */
		/*     a=(buf[byte+0] & 0xf0) >> 4, */
		/*     b=(buf[byte+0] & 0x0f), */
		/*     c=(buf[byte+1] & 0xf0) >> 4, */
		/*     d=(buf[byte+1] & 0x0f), */
		/*     e=(buf[byte+2] & 0xf0) >> 4, */
		/*     f=(buf[byte+2] & 0x0f); */
		/*   store[i]=(c<<8+b<<4+a)*16; */
		/*   store[i+1]=(f<<8+e<<4+d)*16; */
		/* } */
		int fifo_now=count%NFIFO;
		for(i=0;i<W*H;i++)
		  store[i+fifo_now*W*H]=(buf[2*i]+256*buf[2*i+1])*16;
		fifo=fifo_now;
	       	//memcpy(store,buffer->data,min(sizeof(store),buffer->size));
		//pthread_cond_signal( &condition_new_image );
		//pthread_mutex_unlock(&mutex_texture);
		
		count++;
		if (count==1000){
		  //g_main_loop_quit (data->main_loop);
		}
		arv_stream_push_buffer (stream, buffer);
	}
}

static gboolean
periodic_task_cb (void *abstract_data)
{
	ApplicationData *data = abstract_data;

	printf ("Frame rate = %d Hz\n", data->buffer_count);
	data->buffer_count = 0;

	if (cancel) {
	  g_main_loop_quit (data->main_loop);
		return FALSE;
	}

	return TRUE;
}

static void
control_lost_cb (ArvGvDevice *gv_device)
{
	/* Control of the device is lost. Display a message and force application exit */
	printf ("Control lost\n");

	cancel = TRUE;
}



int
main (int argc, char **argv)
{
	ApplicationData data;
	ArvCamera *camera;
	ArvStream *stream;
	ArvBuffer *buffer;
	int i;

	pthread_t gl_thread;
      
	int iret1 = pthread_create( &gl_thread, NULL, gl, (void*) NULL);
	
	data.buffer_count = 0;

	/* Mandatory glib type system initialization */
	arv_g_type_init ();

	/* Instantiation of the first available camera */
	camera = arv_camera_new (NULL);

	if (camera != NULL) {
		void (*old_sigint_handler)(int);
		gint payload;
		guint software_trigger_source = 0;
		
		{
		  guint n_pixel_formats;
		  gint64 *formats= arv_camera_get_available_pixel_formats(camera,
									  &n_pixel_formats);
		  int i;
		  for(i=0;i<n_pixel_formats;i++)
		    printf("format %03d = 0x%llx\n",i,(guint64)formats[i]);
		  printf("current format = 0x%llx\n",(guint64)arv_camera_get_pixel_format(camera));
		  arv_camera_set_pixel_format(camera,ARV_PIXEL_FORMAT_MONO_12);
		}
/* format 000 = 0x1080001 MONO_8 */
/* format 001 = 0x1100005 MONO_12 i need Byte_Swapper.class to view the data in imagej */
/* format 002 = 0x10c0006 MONO_12_PACKED */
/* format 003 = 0x210001f YUV_422_PACKED (it was in this setting in the beginning) */
/* format 004 = 0x2100032 YUV_422_YUYV_PACKED */

		/* Set region of interrest to a ...x... pixel area */
		arv_camera_set_region (camera, 0, 0, W, H);
		/* Set frame rate to 10 Hz */
		arv_camera_set_frame_rate (camera, 100.0);
		arv_camera_set_gain (camera, 100);
		arv_camera_set_exposure_time (camera, 990.0 /*us*/);
		/* retrieve image payload (number of bytes per image) */
		payload = arv_camera_get_payload (camera);
		
		/* Create a new stream object */
		stream = arv_camera_create_stream (camera, NULL, NULL);
		if (stream != NULL) {
			/* Push 50 buffer in the stream input buffer queue */
			for (i = 0; i < 50; i++)
				arv_stream_push_buffer (stream, arv_buffer_new (payload, NULL));

			/* Start the video stream */
			arv_camera_start_acquisition (camera);

			/* Connect the new-buffer signal */
			g_signal_connect (stream, "new-buffer", G_CALLBACK (new_buffer_cb), &data);
			/* And enable emission of this signal (it's disabled by default for performance reason) */
			arv_stream_set_emit_signals (stream, TRUE);

			/* Connect the control-lost signal */
			g_signal_connect (arv_camera_get_device (camera), "control-lost",
					  G_CALLBACK (control_lost_cb), NULL);

			/* Install the callback for frame rate display */
			g_timeout_add_seconds (1, periodic_task_cb, &data);

			/* Create a new glib main loop */
			data.main_loop = g_main_loop_new (NULL, FALSE);

			old_sigint_handler = signal (SIGINT, set_cancel);

			/* Run the main loop */
			g_main_loop_run (data.main_loop);

			signal (SIGINT, old_sigint_handler);

			g_main_loop_unref (data.main_loop);

			/* Stop the video stream */
			arv_camera_stop_acquisition (camera);

			g_object_unref (stream);
		} else
			printf ("Can't create stream thread (check if the device is not already used)\n");

		g_object_unref (camera);
	} else
		printf ("No camera found\n");

	//write_dontquit(0);
	pthread_join( gl_thread, NULL);
	return 0;
}

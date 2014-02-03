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

enum { W = 658, H = 494};
unsigned short store[W*H],store2[W*H];
static gboolean cancel = FALSE;
GMainLoop *main_loop;

int camformat = ARV_PIXEL_FORMAT_MONO_12;
/* format 000 = 0x1080001 MONO_8 */
/* format 001 = 0x1100005 MONO_12 i need Byte_Swapper.class to view the data in imagej */
/* format 002 = 0x10c0006 MONO_12_PACKED */
/* format 003 = 0x210001f YUV_422_PACKED (it was in this setting in the beginning) */
/* format 004 = 0x2100032 YUV_422_YUYV_PACKED */

/* red camera (photon focus) */
/* format 000 = 0x1080001 MONO_8 */
/* format 001 = 0x1100003 MONO_10 */
/* format 002 = 0x10c0004 MONO_10_PACKED */
/* format 003 = 0x1100005 MONO_12 */
/* format 004 = 0x10c0006 MONO_12_PACKED */


fftw_complex *fft_in, *fft_out;
fftw_plan fft_plan;

void fft_init()
{
  fft_in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * W*H);
  fft_out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * W*H);
  fftw_init_threads();
  fftw_plan_with_nthreads(6);
  fft_plan=fftw_plan_dft_2d(H,W,fft_in, fft_out, FFTW_FORWARD, FFTW_ESTIMATE);
}

void fft_fill()
{
  int i;
  double s=1/65535.0;
  for(i=0;i<W*H;i++){
    fft_in[i]=store[i]*s;
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

int count=0;
int buffer_count=0;

void draw_quad(int obj, int y){
  glBindTexture( GL_TEXTURE_2D, obj );
  glBegin(GL_QUADS);
  glVertex2i(0,y); glTexCoord2i(0,0);
  glVertex2i(0,H+y); glTexCoord2i(1,0);
  glVertex2i(W,H+y); glTexCoord2i(1,1);
  glVertex2i(W,y); glTexCoord2i(0,1);
  glEnd();
}

void* gl(void*str)
{
  ArvStream*stream=(ArvStream*)str;
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

  ArvBuffer *buffer;
  
  while (!cancel) {
    buffer = arv_stream_pop_buffer (stream); // blocks until image is available
    if (buffer != NULL) {
      if (buffer->status != ARV_BUFFER_STATUS_SUCCESS){
	printf("error: %d\n",buffer->status);
      } else {
	buffer_count++;
      }
      
      {
	unsigned char *buf=buffer->data;
	int byte;
	switch(camformat){
	case ARV_PIXEL_FORMAT_MONO_12_PACKED:
	  for(i=0,byte=0; byte<buffer->size; i+=2, byte+=3){
	    // look at three bytes of the buffer, assign each of the 6 4-bit
	    // nibbles to the letters a-f:
	    // ab cd ef
	    // the stored 12-bit values are then (in little-endian):
	    // abd efc
	    unsigned char
	      a=(buf[byte+0] & 0xf0) >> 4,
	      b=(buf[byte+0] & 0x0f),
	      c=(buf[byte+1] & 0xf0) >> 4,
	      d=(buf[byte+1] & 0x0f),
	      e=(buf[byte+2] & 0xf0) >> 4,
	      f=(buf[byte+2] & 0x0f);
	    store[i+0]=(a<<8+b<<4+d)<<4;
	    store[i+1]=(e<<8+f<<4+c)<<4;
	  }
	  break;
	case ARV_PIXEL_FORMAT_MONO_12:
	  for(i=0;i<W*H;i++){
	    store[i]=(buf[2*i]+256*buf[2*i+1])*16;
	  }
	  break;
	}
      }
      
      
      glBindTexture( GL_TEXTURE_2D, texture[0] );
      glTexSubImage2D(GL_TEXTURE_2D,0,0,0,W,H,GL_LUMINANCE,GL_UNSIGNED_SHORT,store);
      
      /*    FILE*f=fopen("/dev/shm/bla.pgm","w");
	    fprintf(f,"P5\n658 494\n65535\n");
	    fwrite(store,sizeof(store),1,f);
	    fclose(f);
      */
      
      fft_fill(); fft_run();
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
      
      count++;
      if (count==1000){
	//g_main_loop_quit (data->main_loop);
      }
      arv_stream_push_buffer (stream, buffer);
      }
    }
    glfwTerminate();
  }


static void
set_cancel (int signal)
{
  cancel = TRUE;
}


#define min(a,b) (((a)<(b))?(a):(b))

static gboolean
periodic_task_cb (void *abstract_data)
{

	printf ("Frame rate = %d Hz\n", buffer_count);
	buffer_count=0;

	if (cancel) {
	  g_main_loop_quit (main_loop);
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
	ArvCamera *camera;
	ArvStream *stream;
	ArvBuffer *buffer;
	int i;

	pthread_t gl_thread;
      
	/* Mandatory glib type system initialization */
	arv_g_type_init ();

	/* Instantiation of the first available camera */
	camera = arv_camera_new ("Basler-21211553");

	if (camera != NULL) {
		void (*old_sigint_handler)(int);
		gint payload;
		guint software_trigger_source = 0;
		
		{
		  guint n_pixel_formats;
		  gint64 *formats= arv_camera_get_available_pixel_formats(camera,
									  &n_pixel_formats);
		  guint n_names;
		  const char**names = arv_camera_get_available_pixel_formats_as_display_names(camera,&n_names);
		  int i;
		  for(i=0;i<fmin(n_pixel_formats,n_names);i++)
		    printf("format %03d = 0x%llx = %s\n",i,(guint64)formats[i],names[i]);

		  arv_camera_set_pixel_format(camera,camformat);

		  printf("current format = 0x%llx = %s\n",
			 (guint64)arv_camera_get_pixel_format(camera),
			 arv_camera_get_pixel_format_as_string(camera));
		  
		}


		/* Set region of interrest to a ...x... pixel area */
		gint sensor_w,sensor_h;
		arv_camera_get_sensor_size(camera,&sensor_w,&sensor_h);
		// red camera is 2080x2080
		printf("sensor size %dx%d\n",sensor_w,sensor_h);
		gint center_x=sensor_w/2, center_y=sensor_h/2;
		gint cx=center_x-W/2, cy=center_y-H/2;
		  printf("setting size: %dx%d+%d+%d\n", W, H, cx, cy);
		arv_camera_set_region (camera, cx, cy, W, H);
		double fmin,fmax;
		arv_camera_get_frame_rate_bounds(camera,&fmin,&fmax);
		if(fmin && fmax){
		  printf("frame-rate min: %f max: %f \n",fmin,fmax);
		} else {
		  printf("frame-rate bounds not reported\n");
		}

		/* Set frame rate to 10 Hz */
		arv_camera_set_frame_rate (camera, 20.0);
		arv_camera_set_gain (camera, 100);
		arv_camera_set_exposure_time (camera, 190.0 /*us*/);
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

			int iret1 = pthread_create( &gl_thread, NULL, gl, (void*) stream);

			/* Connect the control-lost signal */
			g_signal_connect (arv_camera_get_device (camera), "control-lost",
					  G_CALLBACK (control_lost_cb), NULL);

			/* Install the callback for frame rate display */
			g_timeout_add_seconds (1, periodic_task_cb, NULL);
			
			/* Create a new glib main loop */
			main_loop = 
			  g_main_loop_new (NULL, FALSE);

			old_sigint_handler = signal (SIGINT, set_cancel);

			/* Run the main loop */
			g_main_loop_run (main_loop);

			signal (SIGINT, old_sigint_handler);

			g_main_loop_unref (main_loop);

			/* Stop the video stream */
			arv_camera_stop_acquisition (camera);

			g_object_unref (stream);
		} else
			printf ("Can't create stream thread (check if the device is not already used)\n");

		g_object_unref (camera);
	} else
		printf ("No camera found\n");

	pthread_join( gl_thread, NULL);
	return 0;
}

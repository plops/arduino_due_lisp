//  gcc -g -O2 -o arv-example arvexample.c -MD -MP -MF -pthread -I/usr/include/aravis-0.4 -I/usr/lib/glib-2.0/include  -I/usr/include/glib-2.0  -lm -L/usr/lib -lgio-2.0 -lgobject-2.0 -lxml2 -lgthread-2.0 -pthread -lrt -lglib-2.0 -lz  -laravis-0.4 -lglfw
#include <GLFW/glfw3.h>
#include <string.h>
#include <arv.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>

enum { W = 658, H = 494};

unsigned short store[W*H];

pthread_mutex_t mutex_texture = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  condition_new_image   = PTHREAD_COND_INITIALIZER;

pthread_mutex_t mutex_dontquit = PTHREAD_MUTEX_INITIALIZER;
int dontquit=1;

void write_dontquit(int b)
{
  pthread_mutex_lock( &mutex_texture );
  dontquit=b;
  pthread_mutex_unlock( &mutex_texture );
}


int read_dontquit()
{
  pthread_mutex_lock( &mutex_texture );
  int b=dontquit;
  pthread_mutex_unlock( &mutex_texture );
  return b;
}

void* gl(void*ignore)
{
  if (!glfwInit())
    exit(EXIT_FAILURE);
  GLFWwindow* window = glfwCreateWindow(W, H, "gig-e-camera", NULL, NULL);
  glfwMakeContextCurrent(window);
  GLuint texture;
  glGenTextures( 1, &texture );
  glBindTexture( GL_TEXTURE_2D, texture );
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
  glTexImage2D(GL_TEXTURE_2D,0,GL_LUMINANCE16,W,H,0,GL_LUMINANCE,GL_UNSIGNED_SHORT,0);
  //glPopMatrix();
  //glMatrixMode(GL_MODELVIEW);
  glEnable(GL_TEXTURE_2D);

  while (read_dontquit()) {
    //pthread_mutex_lock( &mutex_texture );
    // Wait while reader function new_buffer_cb copies data
    // mutex unlocked if condition variable in new_buffer_cb  signaled.
    //pthread_cond_wait( &condition_new_image, &mutex_texture );

    glTexSubImage2D(GL_TEXTURE_2D,0,0,0,W,H,GL_LUMINANCE,GL_UNSIGNED_SHORT,store);
 

    FILE*f=fopen("/dev/shm/bla.pgm","w");
    fprintf(f,"P5\n658 494\n65535\n");
    fwrite(store,sizeof(store),1,f);
    fclose(f);


    //pthread_mutex_unlock( &mutex_texture );
    int width,height;
    glfwGetFramebufferSize(window, &width, &height);
    float ratio = width / (float) height;
    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    //glOrtho(-ratio, ratio, -1.f, 1.f, 1.f, -1.f);
    glOrtho(0, W, 0, H, 1.f, -1.f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    //glRotatef((float) glfwGetTime() * 50.f, 0.f, 0.f, 1.f);
    glColor3f(1.f,1.f,1.f);
    glBegin(GL_QUADS);
    glVertex2i(0,0); glTexCoord2i(0,0);
    glVertex2i(0,H); glTexCoord2i(1,0);
    glVertex2i(W,H); glTexCoord2i(1,1);
    glVertex2i(W,0); glTexCoord2i(0,1);
    glEnd();
     
    glfwSwapBuffers(window);
    usleep(32000);
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
		char s[100];
		snprintf(s,sizeof(s),"/dev/shm/dat/o%04d.pgm",count);
		FILE*f=fopen(s,"w");
		fprintf(f,"P5\n658 494\n65535\n");
		count++;
		fwrite(buffer->data,buffer->size,1,f);
		fclose(f);

		//pthread_mutex_lock(&mutex_texture);
		int i;
		for(i=0;i<W*H;i++)
		store[i]=(1<<16)/(1<<12)*(buffer->data[2*i]+256*buffer->data[2*i+1]);
		//memcpy(store,buffer->data,min(sizeof(store),buffer->size));
		/*pthread_cond_signal( &condition_new_image );
		pthread_mutex_unlock(&mutex_texture);
		*/
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
		arv_camera_set_exposure_time (camera, 390.0 /*us*/);
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

	write_dontquit(0);
	pthread_join( gl_thread, NULL);
	return 0;
}

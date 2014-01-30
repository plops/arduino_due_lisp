#include <arv.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>

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
		fprintf(f,"P5\n659 494\n65535\n");
		count++;
		fwrite(buffer->data,buffer->size,1,f);
		fclose(f);
		if (count==1000){
		  g_main_loop_quit (data->main_loop);
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
/* format 001 = 0x1100005 MONO_12 */
/* format 002 = 0x10c0006 MONO_12_PACKED */
/* format 003 = 0x210001f YUV_422_PACKED (it was in this setting in the beginning) */
/* format 004 = 0x2100032 YUV_422_YUYV_PACKED */

		/* Set region of interrest to a 200x200 pixel area */
		arv_camera_set_region (camera, 0, 0, 659, 494);
		/* Set frame rate to 10 Hz */
		arv_camera_set_frame_rate (camera, 100.0);
		arv_camera_set_gain (camera, 100);
		arv_camera_set_exposure_time (camera, 10000.0 /*us*/);
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

	return 0;
}

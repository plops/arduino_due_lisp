#include <sys/inotify.h>
#include <stdio.h>
/* size of the event structure, not counting name */
#define EVENT_SIZE  (sizeof (struct inotify_event))

/* reasonable guess as to size of 1024 events */
#define BUF_LEN        (1024 * (EVENT_SIZE + 16))


int main()
{
  int fd=inotify_init();
  
  int in=inotify_add_watch(fd,"/dev/shm",IN_CLOSE_WRITE);
  //  int in1=inotify_add_watch(fd,"/dev/shm/1.pgm",IN_CLOSE_WRITE);

  //  int in2=inotify_add_watch(fd,"/dev/shm/2.pgm",IN_CLOSE_WRITE);


  char buf[BUF_LEN];
  while (1){
  
    int len, i = 0;
    len = read (fd, buf, BUF_LEN);
    
    if(len<=0)
      printf("error\n");
    
    /* if (len < 0) { */
    /*   if (errno == EINTR){ */
    /*     /\* need to reissue system call *\/ */
    /*   }else{ */
    /*     perror ("read"); */
    /*   } */
    /* } else if (!len){ */
    /*   /\* BUF_LEN too small? *\/ */
    /* } */
  
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
    //    inotify_rm_watch(fd,in1);
    //inotify_rm_watch(fd,in2);
  }
}




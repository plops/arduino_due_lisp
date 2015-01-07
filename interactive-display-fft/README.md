Hier zeige ich wie man interaktiv ein graphisches Programm mit
libvncserver http://libvnc.github.io/ programmieren kann.

Vor einer Weile habe ich durch den Post
http://nullprogram.com/blog/2014/12/23/ von einer einfachen Methode
gelernt wie man ein C-Programm interaktiv entwickeln kann. Dabei
compiliert man seinen Code als shared library und laesst diese nach
jeder Neucompilation erneut laden. 

Hier zeige ich wie man mit libvncserver auf diese Weise sehr einfach
Bilder ausgeben kann. 


Zunaechst binde ich den Header von libvncserver ein. Dann definiere
ich den Datentyp run_state, der mit dem Typ rfbScreenInfoPtr den VNC
Server repraesentiert. Ausserdem noch die Dimensionen des Bildes.

#include <rfb/rfb.h>

struct run_state{
  rfbScreenInfoPtr server;
};

const  int w=512,h=512;


Die Funktion r_init oeffnet einen VNC Server und alloziiert Speicher
fuer den Framebuffer.

struct run_state * r_init()
{
  struct run_state *state = malloc(sizeof(*state));
  printf("init\n");
  state->server=rfbGetScreen(0,NULL,w,h,8,3,4);
  if(!state->server)
    return 0;
  state->server->frameBuffer=(char*)malloc(w*h*4);
  state->server->alwaysShared=(1==1);
  rfbInitServer(state->server);

  return state;
}

Die folgende Funktion macht den VNC Server wieder zu. Wenn run.c zu
einer neuen librun.so compiliert wurde, wird durch main.c in kurzer
Folge r_finalize der alten Library aufgerufen und dann das r_init der
neuen.  Ehrlich gesagt hat es mich ueberrascht, dass mein vncclient
trotzdem offenbleibt und dann Bilder der neuen modifizierten librun.so
erhaelt.

void r_finalize(struct run_state *state)
{
  printf("finalize\n");
  rfbShutdownServer(state->server,TRUE);
  free(state->server->frameBuffer);
  rfbScreenCleanup(state->server);
  free(state);
}

Hier noch die letzte wichtige Funktion r_step. Diese erzeugt das
anzuzeigende Bild.

static int count = 0;
int r_step(struct run_state *state)
{
  //  printf("step\n");
  if(!rfbIsActive(state->server))
    return 0;
  int i,j;
  char *b=state->server->frameBuffer;
  for(j=0;j<h;j++)
    for(i=0;i<w;i++){
      int p=4*(i+w*j);
      b[p+0]=b[p+1]=b[p+2]=i%255;
    } 
  char s[100];
  snprintf(s,100,"count: %d\n",count++);
  rfbDrawString(state->server,&radonFont,20,100,s,0xffffff);
  rfbMarkRectAsModified(state->server,0,0,w,h);
  long usec = state->server->deferUpdateTime*1000;
  rfbProcessEvents(state->server,usec);

  return 1; 
}

Ich praesentierte hier nur ein paar Ausschnitte. Der vollstaendige
Code ist in diesem Verzeichnis auf github:
https://github.com/plops/arduino_due_lisp/tree/3c21d70a9f1b214e39fa3dfb07704c06ed1e2cb9/interactive-display-fft

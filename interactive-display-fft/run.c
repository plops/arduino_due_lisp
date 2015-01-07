#include "api.h"
#include <stdio.h>
#include <malloc.h>
#include <rfb/rfb.h>
#include "radon.h"
struct run_state{
  rfbScreenInfoPtr server;
};

const  int w=512,h=512;

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
void r_finalize(struct run_state *state)
{
  printf("finalize\n");
  rfbShutdownServer(state->server,TRUE);
  free(state->server->frameBuffer);
  rfbScreenCleanup(state->server);
  free(state);
}
void r_reload(struct run_state *state)
{
  //  printf("reload\n");
}
void r_unload(struct run_state *state)
{
  //  printf("unload\n");
}
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


const struct run_api RUN_API = {
  .init = r_init,
  .finalize = r_finalize,
  .reload = r_reload,
  .unload = r_unload,
  .step = r_step
};



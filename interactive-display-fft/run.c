#include "api.h"
#include <stdio.h>
#include <malloc.h>
#include <rfb/rfb.h>

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
  free(state->server->frameBuffer);
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
  
  return 1; 
}


const struct run_api RUN_API = {
  .init = r_init,
  .finalize = r_finalize,
  .reload = r_reload,
  .unload = r_unload,
  .step = r_step
};



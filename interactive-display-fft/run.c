#include "api.h"
//#include <stdio.h>
#include <malloc.h>
struct run_state{
  int a;
};



struct run_state state;

struct run_state * r_init()
{
  //  printf("init\n");
  //struct run_state *state = malloc(sizeof(*state));
  //state->a = 10;
  return &state;
}
void r_finalize(struct run_state *state)
{
  //  printf("finalize\n");
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
  return 1; //--state->a;
}


const struct run_api RUN_API = {
  .init = r_init,
  .finalize = r_finalize,
  .reload = r_reload,
  .unload = r_unload,
  .step = r_step
};



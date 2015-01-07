// http://nullprogram.com/blog/2014/12/23/


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <dlfcn.h>
#include "api.h"

const char *RUN_LIBRARY = "./librun.so";

#define d(e) do{if(1)(e);}while(0)

struct run {
  void *handle;
  ino_t id;
  struct run_api api;
  struct run_state *state;
};

static void run_unload(struct run*run)
{
  if(run->handle){
    run->api.finalize(run->state);
    run->state = NULL;
    dlclose(run->handle);
    run->handle = NULL;
    run->id=0;
  }
}

static void run_load_if_new_lib(struct run*run)
{
  struct stat attr;
  d(printf("trying to load library\n"));
  if(stat(RUN_LIBRARY,&attr)!=0){
    d(printf("RUN_LIBRARY doesn't exist.\n"));
    return;
  }
  d(printf("check if inode was modified\n"));
  if (run->id == attr.st_ino) { // note: id is initially 0 and the
				 // file should therefore be loaded at
				 // first call
    d(printf("file didn't change\n"));
    return;
  }
  if(run->handle){
    d(printf("library was already open, closing it.\n"));
    run->api.unload(run->state);
    dlclose(run->handle);
  }
  d(printf("dlopen library\n"));
  void *handle = dlopen(RUN_LIBRARY,RTLD_NOW);
  if(!handle){
    run->handle = NULL;
    run->id = 0;
    d(printf("error during dlopen.\n"));
    return;
  }
  run->handle = handle;
  run->id = attr.st_ino;
  d(printf("load the struct\n"));
  const struct run_api *api =
    dlsym(run->handle,"RUN_API");
  if(api == NULL){
    dlclose(run->handle);
    run->handle = NULL;
    run->id = 0;
    d(printf("error during dlsym of api struct.\n"));
    return ;
  }
  d(printf("copy the data\n"));
  run->api = *api;
  if(run->state == NULL){
    d(printf("set state by calling init\n"));
    if(run->api.init)
      run->state = run->api.init();
    else {
      d(printf("init isn't defined\n"));
      return;
    }
  }
  if(run->api.reload && run->state){
    d(printf("call reload\n"));
    run->api.reload(run->state);
  }
}

int main(void)
{
  struct run run = {0};
  for(;;){
    d(printf("try to reload\n"));
    run_load_if_new_lib(&run);
    d(printf("call step\n"));
    if(run.handle){
      //      d(printf("step is pointer to this address: %lx %lx %lx\n", run.api, run.api.step, run.state));
      if(run.api.step==NULL){
	d(printf("step is undefined\n"));
	break;
      }
      if(run.state==NULL){
	d(printf("state is undefined\n"));
	break;
      }
      
      {
	int ret = run.api.step(run.state);
	d(printf("step returned %d\n",ret));
	if(!ret)
	  break;
      }
    }
    d(printf("sleep\n"));
    usleep(100000);
  }
  d(printf("unload\n"));
  run_unload(&run);
  return 0;
}

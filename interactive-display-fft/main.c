// http://nullprogram.com/blog/2014/12/23/

#define _GNU_SOURCE 
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <dlfcn.h>
#include <signal.h>
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
  //d(printf("trying to load library\n"));

  if(stat(RUN_LIBRARY,&attr)!=0){
    d(printf("RUN_LIBRARY doesn't exist.\n"));
    return;
  }
  //d(printf("check if inode was modified\n"));
  if (0 && run->id == attr.st_ino) { // note: id is initially 0 and the
				 // file should therefore be loaded at
				 // first call
    //d(printf("file didn't change\n"));
    return;
  }
  //printf("trying to reload library\n");
  if(run->handle){
    d(printf("library was already open, closing it.\n"));
    run->api.unload(run->state);
    if(0!=dlclose(run->handle))
      printf("error with dlclose\n");
    if(0!=dlclose(run->handle))
      printf("error with dlclose\n");
    if(0!=dlclose(run->handle))
      printf("error with dlclose\n");
  }

  //d(printf("dlopen library\n"));
  void *handle = dlopen(RUN_LIBRARY,RTLD_NOW);
  if(!handle){
    run->handle = NULL;
    run->id = 0;
    d(printf("error during dlopen %s, check with strace, perhaps your library dependencies are not in LD_LIBRARY_PATH.\n",dlerror()));
    return;
  }
  run->handle = handle;
  run->id = attr.st_ino;
  //printf("load the struct\n");
  const struct run_api *api =
    dlsym(run->handle,"RUN_API");
  if(api == NULL){
    dlclose(run->handle);
    run->handle = NULL;
    run->id = 0;
    d(printf("error during dlsym of api struct.\n"));
    return ;
  }
  //d(printf("copy the data\n"));
  run->api = *api;
  // printf("reload %lx\n step %lx\n",(unsigned long)run->api.reload,(unsigned long)run->api.step);
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

struct run run = {0};

void signalHandler(int a)
{
  (void)a;
  // in case i send sigusr1 to the process, reload library
  run_load_if_new_lib(&run);
}


#define e(q) do{if(0!=(q)) printf("error in %s",__func__);}while(0)
int main(void)
{
  
  signal(SIGUSR1,signalHandler);
  run_load_if_new_lib(&run);
  for(;;){
    //run_load_if_new_lib(&run);

    if(run.handle){
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
	//int (*step)(struct run_state *) =  dlsym(run.handle,"r_step");
	//int ret = step(run.state);
        
	if(!ret)
	  break;

	
	Dl_info info;
	ret = dladdr(run.api.step,&info);
	if(ret==0){
	  printf("error in dladdr\n");
	} else {
	  printf("dladdr run.api.step fname=%s base=%lx sname=%s saddr=%lx \n",
		 info.dli_fname,info.dli_fbase,info.dli_sname,info.dli_saddr);
	}
	/* ret = dladdr(step,&info); */
	/* if(ret==0){ */
	/*   printf("error in dladdr\n"); */
	/* } else { */
	/*   printf("dladdr         step fname=%s base=%lx sname=%s saddr=%lx \n", */
	/* 	 info.dli_fname,info.dli_fbase,info.dli_sname,info.dli_saddr); */
	/* } */
      }
    }
    usleep(32000);
  }
  d(printf("unload\n"));
  run_unload(&run);
  return 0;
}

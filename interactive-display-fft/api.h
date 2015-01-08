#pragma once

#ifdef __cplusplus
extern "C" {
#endif

  struct run_state;
  
  struct run_api {
    struct run_state *(*init)();
    void (*finalize)(struct run_state *state);
    void (*reload)(struct run_state *state);
    void (*unload)(struct run_state *state);
    int (*step)(struct run_state *state);
  };
  
  extern const struct run_api RUN_API;

  
#ifdef __cplusplus
}
#endif

 %module pylon
 %{
 /* Includes the header in the wrapper code */
 #include "pylon_wrapper.h"
 %}
 
 /* Parse the header file to generate wrappers */
 %include "pylon_wrapper.h"
 

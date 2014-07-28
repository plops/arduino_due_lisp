# documentation of the first measurement capturing data from 3 cameras

The code to create these results is in [processing-2.jl](../processing-2.jl) 

Intensities of the mode fields at the fiber endface averaged over all illumination angles:


|       refl w/ polrot   | trans no polrot         | trans w/ polrot               |
| ------------- | ------------- | ------------- |
| ![reflection polarizationrotated](/processing/julia/step12_0724/fiber_endface_intens_refl_perp.jpg?raw=true "refl perp") | ![transmission polarization parallel](/processing/julia/step12_0724/fiber_endface_intens_tran_para.jpg?raw=true "tran para") |  ![transmission polarization  rotated](/processing/julia/step12_0724/fiber_endface_intens_tran_perp.jpg?raw=true "tran perp") |

The following diagrams show the intensity in the first orders of the
three holograms for different deflection angles of the steering
mirror. The mirror was scanned from 400 below 2900 by 12 in first axis
and from 1800 below 3700 by 12 in second axis. This corresponds to 167
points along the first axis and 127 points along the second axis.

These images were acquired simultaneously with 40 fps (overall 522s)

|       refl w/ polrot   | trans no polrot         | trans w/ polrot               |
| ------------- | ------------- | ------------- |
| ![refl_perp](/processing/julia/step12_0724/angular_throughput_refl_perp.jpg?raw=true "refl_perp") | ![tran_para](/processing/julia/step12_0724/angular_throughput_tran_para.jpg?raw=true "tran_para") | ![tran_perp](/processing/julia/step12_0724/angular_throughput_tran_perp.jpg?raw=true "tran_perp") |


The following displays some images of the fiber end of the 3 cameras with different illumination angles:

|                backreflection w/ polrot |             |               |
| -------------: | ------------- | ------------- |
|               | ![](/processing/julia/step12_0724/fiber_coherent_intens_1-2.jpg?raw=true "") | |
| ![](/processing/julia/step12_0724/fiber_coherent_intens_2-2.jpg?raw=true "") | ![](/processing/julia/step12_0724/fiber_coherent_intens_3-2.jpg?raw=true "") | ![](/processing/julia/step12_0724/fiber_coherent_intens_4-2.jpg?raw=true "") |
|               | ![](/processing/julia/step12_0724/fiber_coherent_intens_5-2.jpg?raw=true "") | |


|    transmission no polrot |  |  |
| -------------: | ------------- | ------------- |
| | ![](/processing/julia/step12_0724/fiber_coherent_intens_1-3.jpg?raw=true "") | |
| ![](/processing/julia/step12_0724/fiber_coherent_intens_2-3.jpg?raw=true "") | ![](/processing/julia/step12_0724/fiber_coherent_intens_3-3.jpg?raw=true "") | ![](/processing/julia/step12_0724/fiber_coherent_intens_4-3.jpg?raw=true "") |
| | ![](/processing/julia/step12_0724/fiber_coherent_intens_5-3.jpg?raw=true "") | |


|       transmission w/ polrot   |          |               |
| -------------: | ------------- | ------------- |
|               | ![](/processing/julia/step12_0724/fiber_coherent_intens_1-1.jpg?raw=true "") | |
| ![](/processing/julia/step12_0724/fiber_coherent_intens_2-1.jpg?raw=true "") | ![](/processing/julia/step12_0724/fiber_coherent_intens_3-1.jpg?raw=true "") | ![](/processing/julia/step12_0724/fiber_coherent_intens_4-1.jpg?raw=true "") |
|               | ![](/processing/julia/step12_0724/fiber_coherent_intens_5-1.jpg?raw=true "") | |



correlation between holograms with different illumination angles with respect to the central hologram 

|       refl w/ polrot   | trans no polrot         | trans w/ polrot               |
| ------------- | ------------- | ------------- |
| ![refl_perp](/processing/julia/step12_0724/pearson_center_refl_perp.jpg?raw=true "refl_perp") | ![tran_para](/processing/julia/step12_0724/pearson_center_tran_para.jpg?raw=true "tran_para") | ![tran_perp](/processing/julia/step12_0724/pearson_center_tran_perp.jpg?raw=true "tran_perp") |


correlation of transmission holograms perp and para 

|    U1 U2*     | Re(U1) Re(U2)         | Re(U1) Im(U2)               | Im(U1) Re(U2) | Im(U1) Im(U2) | Abs(U1) Abs(U2) |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| ![tran](/processing/julia/step12_0724/pearson_tran.jpg?raw=true "") | ![tran](/processing/julia/step12_0724/pearson_tran-rr.jpg?raw=true "") | ![tran](/processing/julia/step12_0724/pearson_tran-ri.jpg?raw=true "") | ![tran](/processing/julia/step12_0724/pearson_tran-ir.jpg?raw=true "") | ![tran](/processing/julia/step12_0724/pearson_tran-ii.jpg?raw=true "") | ![tran](/processing/julia/step12_0724/pearson_tran-aa.jpg?raw=true "") |


all images (refl w/ polrot, trans no polrot, trans w/ polrot, U1 U2*) at the same scale:

![](/processing/julia/step12_0724/pearson_center_all.jpg?raw=true "")

the raw data:

```
julia> begin
+    d = 5
+    int(100*abs(pearson_c[cx-d:cx+d,cy-d:cy+d]))
end
11x11 Array{Int64,2}:
 31  23  23  24  25  35  37  21  44  33  21
 23  36  22  34  38  39  36  26  39  18   3
 22  48  29  30  38  49  50  34  43  19   2
 18  56  22  27  45  42  56  37  42   6   4
 31  56  39  35  40  44  58  39  36  25  16
 36  62  40  34  42  32  54  33  47  32  20
 48  59  49  31  41  26  42  32  23  36  34
 46  53  40  30  32  23  30  22  32  39  35
 44  57  20  22  20   9   5  22  20  39  37
 41  36  22  14   6  17   6   9  30  49  44
 24  28  12   7  10  10   3  11  29  39  33

julia> begin
+    d = 5
+    int(100*abs(pearson[cx-d:cx+d,cy-d:cy+d,1]))
end
11x11 Array{Int64,2}:
 343  309  376  440  454  401  339  189  158   75   33
 355  328  421  521  562  525  427  269  227  145   67
 380  356  453  583  667  636  531  355  308  223  129
 371  355  486  644  770  764  652  446  406  314  205
 337  341  485  695  835  843  740  521  482  393  270
 294  313  448  671  867  943  812  570  560  454  340
 229  278  402  621  823  873  811  588  614  511  398
 174  218  341  512  722  769  738  556  607  512  438
 124  159  260  428  585  630  645  513  561  502  433
  78  113  191  320  454  533  531  445  501  471  419
  58   77  128  234  335  408  421  364  434  426  377

julia> begin
+    d = 5
+    int(100*abs(pearson[cx-d:cx+d,cy-d:cy+d,2]))
end
11x11 Array{Int64,2}:
 210  277  327  352  330  307  257  189  126   88   63
 227  310  384  424  419  401  349  268  195  136   99
 239  332  423  486  493  487  444  354  272  198  141
 245  346  443  519  560  569  525  436  350  268  195
 232  342  438  527  584  621  591  509  418  329  241
 205  303  414  509  597  662  620  545  461  377  287
 169  259  367  473  573  615  609  553  489  405  315
 137  204  303  408  498  550  558  533  483  411  326
 105  161  235  328  401  456  486  482  446  401  318
  82  117  171  241  313  363  403  412  407  371  312
  57   80  116  174  232  280  322  342  352  334  286

julia> begin
+    d = 5
+    int(100*abs(pearson[cx-d:cx+d,cy-d:cy+d,3]))
end
11x11 Array{Int64,2}:
 323  294  330  421  426  396  304  183  159  122   79
 345  332  382  502  554  506  399  253  212  170  100
 372  370  412  566  649  607  482  312  281  231  159
 373  373  433  614  709  700  585  386  367  304  225
 362  362  445  616  764  769  648  455  454  378  272
 310  324  418  593  768  884  723  506  498  463  342
 261  279  379  540  751  774  704  529  534  481  363
 200  216  317  469  650  721  662  499  549  477  386
 145  164  246  385  541  628  573  454  510  449  372
  93  108  182  286  417  496  493  382  459  406  347
  64   66  117  217  322  399  395  324  389  363  315

```
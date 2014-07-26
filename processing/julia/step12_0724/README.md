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

# Julia Processing Scripts for Holographic Microscopy Data

This directory contains Julia scripts for processing holographic microscopy data acquired from multiple cameras in the arduino_due_lisp project.

## Overview

This collection of Julia scripts processes complex-valued holographic image data stored in ICS (Image Cytometry Standard) format. The scripts perform various image processing tasks including FFT analysis, SVD decomposition, correlation analysis, and multi-camera data fusion for polarization-sensitive holographic microscopy.

## Files

### Core Processing Scripts

#### `processing.jl`  

A comprehensive Julia tutorial and processing script that demonstrates:
- Julia language basics (functions, types, loops, conditional expressions)
- Complex number operations
- ICS file format reading and parsing
- PGM (Portable Graymap) image file I/O
- FFT operations on image data
- SVD factorization for dimensionality reduction
- Data masking and thresholding using quantiles
- Pseudo-inverse matrix calculations for image reconstruction

The script includes functions for:
- `find_ics_files()` - Locates ICS files in a directory using regex pattern matching
- `find_ics_raw_start()` - Finds the position where raw data begins in ICS files
- `read_ics()` - Reads complex-valued data arrays from ICS files
- `write_pgm()` - Exports arrays as 8-bit PGM images with automatic intensity scaling
- `read_pgm()` - Reads 16-bit PGM files with byte-swapping

#### `processing-2.jl`  

Advanced processing script for three-camera holographic acquisition system with:
- Multi-dimensional complex data processing (90×90×3×47×37 arrays)
- Three camera configurations:
  - Camera 0: Transmission with polarization rotation (top)
  - Camera 1: Back-reflection with polarization rotation
  - Camera 2: Transmission same polarization
- Angular throughput analysis across scanning mirror positions
- Fiber endface intensity averaging
- Coherent image reconstruction at specific illumination angles
- Pearson correlation coefficient calculations between cameras and polarization states
- Mosaic image generation combining multiple angular positions

Key analysis functions:
- `avg_endface()` - Computes average fiber endface intensity
- `bin_avg_endface()` - Creates binary mask using quantile thresholding
- `avg_angle()` - Averages intensity over angular dimensions
- `pearson_coef()` - Calculates correlation between different acquisitions

#### `processing-3.jl`  

Similar to processing-2.jl with additional features:
- Variable-sized data array reading (configurable dimensions)
- Support for concatenated multi-polarization datasets
- Pattern matching and convolution analysis for peak finding
- Complex-valued cross-correlation between transmission and reflection images
- Detailed polarization correlation analysis (real-real, real-imaginary, imaginary-imaginary, absolute-absolute)
- View5D integration for interactive 5D data visualization

#### `extract.jl`  

Utility library providing intelligent array extraction and padding functions:
- `extract()` - Extracts a sub-region from an array with automatic padding
- `extract_martin()` - Alternative implementation with explicit padding control
- `asize()` - Returns array size as an array instead of tuple
- `pad_dimensions_from_array()` - Automatically pads dimension specifications

These functions handle edge cases intelligently, padding with specified values when extraction regions extend beyond array boundaries.

## Data Structure

The ICS files contain 5-dimensional complex-valued arrays with dimensions:
- Spatial X (typically 64-128 pixels)
- Spatial Y (typically 64-128 pixels)
- Camera index (1-3 cameras)
- Angular scan axis 1 (varies, e.g., 47 or 120 positions)
- Angular scan axis 2 (varies, e.g., 37 or 120 positions)  

## Example Results

### Step12_0724 Measurement Documentation

The `step12_0724/` subdirectory contains a detailed example of processed results from a three-camera acquisition:  

**Key Results Include:**

1. **Fiber Endface Intensities** - Average mode field intensities at the fiber endface for all three cameras:
   - Reflection with polarization rotation
   - Transmission without polarization rotation  
   - Transmission with polarization rotation

2. **Angular Throughput Maps** - Intensity variations across 167×127 scanning mirror positions, acquired simultaneously at 40 fps (total 522 seconds)

3. **Coherent Fiber Images** - Complex-valued reconstructions at five specific illumination angles arranged in a cross pattern

4. **Correlation Analysis**:
   - Pearson correlation between holograms at different illumination angles relative to center
   - Cross-polarization correlation maps (U₁U₂*, Re(U₁)Re(U₂), Re(U₁)Im(U₂), Im(U₁)Re(U₂), Im(U₁)Im(U₂), |U₁||U₂|)

**Image Links** (if available in the repository):
- `fiber_endface_intens_refl_perp.jpg`
- `fiber_endface_intens_tran_para.jpg`
- `fiber_endface_intens_tran_perp.jpg`
- `angular_throughput_refl_perp.jpg`
- `angular_throughput_tran_para.jpg`
- `angular_throughput_tran_perp.jpg`
- `fiber_coherent_intens_
- `pearson_center_refl_perp.jpg`
- `pearson_center_tran_para.jpg`
- `pearson_center_tran_perp.jpg`
- `pearson_tran.jpg` (and variants: -rr, -ri, -ir, -ii, -aa)
- `pearson_center_all.jpg`

## Dependencies

The scripts require the following Julia packages:
- Base FFT functions
- View5D (for 5D array visualization)
- Winston (for plotting)
- Images and ImageView (for display)
- Standard Julia libraries (Readline, REPL)  

## Performance Notes  

The scripts include timing benchmarks showing:
- FFT operations on large arrays: ~22-132 seconds
- SVD factorization: ~65 seconds for reduced datasets
- Pseudo-inverse computation: ~16-78 seconds depending on matrix size
- Correlation analysis: ~391 seconds for full angular scan  

## Usage Example

```julia
# Include the extract utility
include("extract.jl")

# Read ICS file
ics_file = "/path/to/data.ics"
a = read_ics(ics_file)

# Process and visualize
using View5D
view5d(squeeze(abs2(a
```

## Notes

- The code is significantly faster than equivalent MATLAB implementations
- Julia's column-major array ordering (Fortran-style) is used throughout
- Integer arithmetic follows wrapping behavior rather than saturating arithmetic
- Some camera orientations may require mirroring (camera 3 uses reversed indexing in some cases)  

---

**Notes:**

This README documents the Julia processing scripts for holographic microscopy data in the arduino_due_lisp project. The scripts handle complex-valued multidimensional data from a three-camera system designed for polarization-sensitive measurements. The step12_0724 subdirectory contains a complete example with processed images, though the actual image files may not be present in the repository depending on file size constraints. The code demonstrates Julia's capabilities for numerical computing and image processing, with particular emphasis on FFT operations, linear algebra (SVD, pseudo-inverse), and correlation analysis.



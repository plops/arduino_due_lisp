function asize(a::Array)
    # return size of array as an array (instead of tuple)
    [size(a)...]
end
function pad_dimensions_from_array(dim::Array, arr::Array, fun=identity)
    # if dim is a vector with a smaller size than the array rank, copy
    # the dimensions from the array (and optionally apply fun)
    if length(dim) < ndims(arr)
        append!(dim, map(fun, asize(arr)[length(dim)+1:end]))
    end
    dim
end

function extract{T}(a::Array{T}, newsize::Array, center::Array, value::T)
  # create a new array with dimensions NEWSIZE and copy data from
  # array A.
  #
  # if dimensions of newsize and/or center are insufficient,
  # pad trailing dimensions from array
  newsize = pad_dimensions_from_array(newsize, a)
  center = pad_dimensions_from_array(center, a, (x)->div(x+1, 2))
  shift = div(newsize, 2) - center + 1
  # coordinates of the first pixel of the source.
  srcstart = center - div(newsize, 2)
  # coordinates of the last pixel of the source.
  srcend = srcstart + newsize - 1
  # limit borders
  srcstart = max(srcstart, 1)
  srcend = min(srcend, asize(a))
  dststart = srcstart + shift
  dstend = srcend + shift
  # create an array of ranges
  srcrange = map(colon, srcstart, srcend)
  dstrange = map(colon, dststart, dstend)
  out = fill(value, newsize...)
  out[dstrange...] = a[srcrange...]
  out
end


function extract_martin(a, newsize, center, value)
    # create a new array with dimensions NEWSIZE and copy data from
    # array A. the code tries to be intelligent in acting according to
    # the arguments and will hopefully do the right thing even if you
    # hand over incomplete arguments for NEWSIZE or CENTER (or a
    # floating point CENTER) if only a single number is given as
    # newsize, turn it into array if dimension of newsize is
    # insufficient, copy missing part from array. This code was
    # created by 2014 Martin Kielhorn based on Matlab code by Rainer
    # Heintzmann (with help from Hauke and Simon)
    newsize = pad_dimensions_from_array(newsize, a)
    # use similar code to fill up center if necessary, the center is
    # by default set to the middle of the array
    center = pad_dimensions_from_array(center,a,(x)->div(x,2)+1)
    # convert to int, in case center contains floating point
    srccenter = int(round(center))
    # originating from the center find the coordinates of the first
    # pixel of the source. 
    srcstart = srccenter-div(newsize,2)
    # the coordinates of the last pixel of the source. the colon
    # function takes inclusive range as an argument. that explains the
    # -1.    
    srcend   = srcstart+newsize-1
    # the result can be <1. in this case the destination needs to be
    # shifted in the positive direction to create an appropriate
    # padding band:
    dststart = [(ss<1)?2-ss:1 for ss in srcstart]
    # limit the left border of srcstart
    srcstart[srcstart.<1]=1
    # the result can be too big and outside the range of valid
    # coordinates of array a. the largest legal value for srcend is
    # asize(a).  if dstend is within the array bounds srcend<=size(a),
    # the coordinates are calculated as in the line before the
    # previous for srcend.
    dstend   = [newsize[i]+
                ((size(a)[i]<srcend[i])?-srcend[i]+size(a)[i]:0)
                for i=1:length(srcstart)]
    # limit the right border of srcend
    outranged = srcend.>asize(a)
    srcend[outranged] = asize(a)[outranged]
    # create an array of ranges 
    srcrange = map(colon,srcstart,srcend)
    dstrange = map(colon,dststart,dstend)
    out = fill(value,newsize...)
    out[dstrange...] = a[srcrange...]
    out
end

function extract(a::Array, newsize::Array, center::Array=div(asize(a)+1, 2), value=0)
    extract(a, newsize, int(round(center)), value)
end
function extract(a::Array, newsize::Number, center::Array=div(asize(a)+1, 2), value=0)
    extract(a, [newsize], int(round(center)), value)
end
function extract(a::Array, newsize::Array, center::Number, value=0)
    extract(a, newsize, [center], value)
end
function extract(a::Array, newsize::Number, center::Number, value=0)
    extract(a, [newsize], [center], value)
end


#extract([x*10+y for x=1:3,y=1:3],[3,3])

#extract_hauke([x*10+y for x=1:3,y=1:3],[3,3],[2,2],0)   

# example use:
# [x*10+y for x=1:9,y=1:9]
# extract([x*10+y for x=1:9,y=1:9],[11,11])

## using Base.Test

## @test(extract([x*10+y for x=1:3,y=1:3],[3,3],[2,1])==
## [0 11  12; 
##  0 21  22;
##  0 31  32])


## @test(extract([x*10+y for x=1:3,y=1:3],[3,3],[1,1])==
## [0  0   0;
##  0 11  12; 
##  0 21  22])

## @test(extract([x*10+y for x=1:3,y=1:3],[3,3])==
## [11  12  13;
##  21  22  23;
##  31  32  33;])


## @test(extract([x*10+y for x=1:3,y=1:3],[4,4])==
## [ 0   0   0   0;
##  0  11  12  13;
##  0  21  22  23;
##  0  31  32  33;])

## @test(extract([x*10+y for x=1:3,y=1:3],[5,5])==
## [ 0   0   0   0  0;
##  0  11  12  13  0;
##  0  21  22  23  0;
##  0  31  32  33  0;
##  0   0   0   0  0])

## @test(extract([x*10+y for x=1:3,y=1:3],[2,2])==
## [ 11  12;
##  21  22;])


## @test(extract([x*10+y for x=1:3,y=1:3],[2,2],[1,1])==
## [0 0
##  0 11])
 
## @test(extract([x*10+y for x=1:3,y=1:3],[5,3],[1,1])==
## [0 0 0
##  0 0 0
##  0 11 12
##  0 21 22
##  0 31 32])

## @test(extract([x*10+y for x=1:3,y=1:3],[5,3])==
## [  0   0   0;
##  11  12  13;
##  21  22  23;
##  31  32  33;
##   0   0   0;])


## @test(extract([x*10+y for x=1:3,y=1:3],[3,5])==
## [ 0  11  12  13  0;
##  0  21  22  23  0;
##  0  31  32  33  0;])
 
## @test(extract([x*10+y for x=1:3,y=1:3],[3,5],[2,1])==
## [0 0 11 12 13
##  0 0 21 22 23
##  0 0 31 32 33]);
 
## @test(extract([x*10+y for x=1:3,y=1:4],[3,7],[2,4])==
## [11  12  13  14  0  0  0;
##  21  22  23  24  0  0  0;
##  31  32  33  34  0  0  0])

## @test(extract([x*10+y for x=1:3,y=1:3],[3,3],[2,3])==
## [12  13  0;
##  22  23  0;
##  32  33  0;])

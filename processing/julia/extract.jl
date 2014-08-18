function extract(a,newsize,center=div(asize(a),2),value=0)
    
end

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
    asz = asize(a)
    # create a new array with dimensions NEWSIZE and copy data from
    # array A. the code tries to be intelligent in acting according to
    # the arguments and will hopefully do the right thing even if you
    # hand over incomplete arguments for NEWSIZE or CENTER (or a
    # floating point CENTER) if only a single number is given as
    # newsize, turn it into array if dimension of newsize is
    # insufficient, copy missing part from array. This code was
    # created by 2014 Martin Kielhorn based on Matlab code by Rainer
    # Heintzmann
    newsize = pad_dimensions_from_array(newsize, a)
    # use similar code to fill up center if necessary, the center is
    # by default set to the middle of the array
    center = fill_from_array(ensure_array(center),a,(x)->div(x,2))
    # convert to int, in case center contains floating point
    srccenter = int(round(center))
    # originating from the center find the coordinates of the first
    # pixel of the source. 
    srcstart = srccenter-div(newsize,2)
    # the result can be <1. in this case the destination needs to be
    # shifted in the positive direction to create an appropriate
    # padding band:
    dststart = [(ss<1)?1-ss:1 for ss in srcstart]
    # limit the left border of srcstart
    srcstart[srcstart.<1]=1
    # the coordinates of the last pixel of the source. the colon
    # function takes inclusive range as an argument. that explains the
    # -1.    
    srcend   = srcstart+newsize-1
    # the result can be too big and outside the range of valid
    # coordinates of array a. the largest legal value for srcend is
    # asize(a).  if dstend is within the array bounds srcend<=size(a),
    # the coordinates are calculated as in the previous line for
    # srcend.
    dstend   = [dststart[i]+newsize[i]-1+
                ((size(a)[i]<srcend[i])?-srcend[i]+size(a)[i]:0)
                for i=1:length(srcstart)]
    # limit the right border of srcend
    outranged = srcend.>asize(a)
    srcend[outranged] = asize(a)[outranged]
    # create an array of ranges 
    srcrange = map(colon,srcstart,srcend)
    dstrange = map(colon,dststart,dstend)
    println([srcrange dstrange])
    out = zeros(eltype(a),newsize...)+value
    out[dstrange...] = a[srcrange...]
    out
end
function extract(a::Array, newsize::Array, center::Array=div(asize(a), 2), value=0)
    extract(a, newsize, int(round(center)), value)
end
function extract(a::Array, newsize::Number, center::Array=div(asize(a), 2), value=0)
    extract(a, [newsize], int(round(center)), value)
end
function extract(a::Array, newsize::Array, center::Number, value=0)
    extract(a, newsize, [center], value)
end
function extract(a::Array, newsize::Number, center::Number, value=0)
    extract(a, [newsize], [center], value)
end


# example use:
# [x*10+y for x=1:9,y=1:9]
# extract([x*10+y for x=1:9,y=1:9],[11,11])

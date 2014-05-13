x = 1.0


typeof(x)

eps(Float64)


# complex numbers
z=1+2im

typeof(z)

z=1.0+2im

typeof(z)

real(z)
imag(z)
conj(z)
abs(z)
abs2(z)
angle(z)
complex(1,3) # explicit construction 

# plain function:
function f(x,y)
    x + y
end

f(1,2)

# anonymous functions:
x->x^2

(x->x^2)(2)

# apply function to elements in an array:
map(x->x^2+2x-1,[1,3,-1])

map(x->x^2+2x-1,[1 3 -1])

# anonymous function with two arguments
(x,y)->(x+y)

((x,y)->(x+y))(3,4)


# function returning multiple values:
function f2(x,y)
    x + y, x*y
end

f2(2,3)

# tuple
x= (3,4)


# optional function arguments:
function blop(num, base=10)
    base + num
end
blop(10)
blop(10,1)


# keyword arguments:
function psf(x,y,z; na=1.4, ri=1.518)
    [x y z na ri]
end
psf(12, 12, 12, na=1.)


# another way to define an anonymous function
map([1, 2, 4]) do x
    if x<0 && iseven(x)
        0
    elseif x==0
        1
    else
        x
    end
end

# this "do x" syntax is useful when handling files: this opens a the
# file "/dev/shm/outfile" and writes the string "bla" into it
cd("/dev/shm/") do
    open("outfile","w") do f
        write(f,"bla")
    end
end


# two ways of defining code blocks:
z = begin
    x=1
    y=3
    x/y
end

z = (x=1;y=4;x+y)

# conditional expressions must not be anything but true or false

# several ways to write loop constructs

i=0;
while i<4
    println(i)
    i+=1
end

for i=0:4
    println(i)
end

for s in [1 2 3 4]
    println(s)
end


# this will iterate over both variables (outer product)
for i=0:2, j=3:4
    println([i j])
end

whos()


# different ways to copy an array:
a=[1 2 3]
b=a

b[1]=12

a


# shallow copy will use new memory:
a=[1 2 3]
b=copy(a)

b[1]=12

a

# there is also this: 
deepcopy(a)

#(x = rand(8);Float64[ .25x[i-1]+.5x[i]+.25x[i+1] for i=2:length(x)-1])

#SVD(x,true) # for thinning

#diag

# julia is 1 based
# arrays are column major ordered (fortran)

extrema([1 2 3 4 5])

findmin([1 2 3 4 5])

minimum([1 2 3 4 5])


# string concatenation    
"Hallo" * " Welt"


ccall((:sin,"libm"),Float64,(Float64,),12.0)

# note: make sure julias garbage collector doesn't move the addresses
# around while you are in a c-call
# pointer_to_array()


# matlab uses saturated integer arithmetic:
# adding to a big value, value stays the same
# not efficient
# not associative
# makes it hard to write many integer algorithms
# prevents aggressive optimization

# julia makes some better assumptions, the authors argue and give this
# neat example:


f(k) = 5k-1;


help(code_native)

code_native(f,(Int,)
            
code_llvm(f,(Int,))

code_typed(f,(Int,))            

function g(k)
    for i=1:10
        k=f(k)
    end
    return k
end

code_native(g,(Int,))
 
code_llvm(g,(Int,))           

# compiler (llvm) optimizes the loop to a multiplication





# compare string with a pattern:
match(r"^j....\.ics$","j1234.ics").match

match(r"^j....\.ics$","bla.mat").match


# array functions similar to matlab:
vcat([1,2,3],[4,5,6])


# find all ics files in a folder
dir = "/media/sda4/b/20140309/0_/"
function find_ics_files(dir) 
    fns = readdir(dir)
    res = [];
    for f in fns
        m = match(r"^j....\.ics$",f) 
        if m != nothing
            res = vcat(res,m.match);
        end
    end
    res
end
fns=find_ics_files(dir)


# i use this to search for the end of the ics header in the files
ismatch(r"^end$","end\n")


# this function opens a file searches from the beginning for the
# string "end" and returns the position
function find_ics_raw_start(fn)
    # locate the word "end" in the ics file
    f=open(fn)
    while !ismatch(r"end",readline(f))
    end
    res = position(f)
    close(f)
    res
end

find_ics_raw_start(dir * first(fns))

# this function 
# jumps over the header
# and reads the raw data from the file into an array
# (i keep the dimensions fixed for this example)
function read_ics(fn)
    pos = find_ics_raw_start(fn)
    f=open(fn)
    seek(f,pos)
    a=read(f,Complex64,80,84,151)
    close(f)
    a
end

a = read_ics(dir * first(fns));


# check that the dimensions make sense
(filesize(dir * first(fns))-519)/(80*84*16)

size(a[:,:,1])



# there is a nice interface for the browser in julia, but for now i
# just store images as pgm files.


# i make a 1d array of the input array then i find the maximum and
# minimum values and create an array with 8bit version of the data,
# scaled between 0 and 255
function write_pgm(a,fn="/dev/shm/o.pgm")
    ar=reshape(a,prod(size(a)))
    mi = minimum(ar)
    ma = maximum(ar)
    s = 0;
    if ma!=mi
        s=255/(ma-mi);
    else
        s=1;
    end
    a8=uint8(min(255,max(0,floor((ar.-mi)*s))))
    f=open(fn,"w")
    @printf(f,"P5\n%d %d\n255\n",size(a,1),size(a,2));
    write(f,a8);
    close(f);
end

write_pgm(abs(a[:,:,1]))

# try reading all the data (considerably faster than matlab)
a=Array(Complex64,80,84,151,161);
fns=find_ics_files(dir)
i = 1;
@time for file in fns
    println(file)
    a[:,:,:,i] = read_ics(dir * fns[i])
    i = i+1;
end


# this is data from a second experiment:
a2=Array(Complex64,80,84,151,161);
dir2 = "/media/sda4/b/20140309/1_/"
fns=find_ics_files(dir2)
i = 1;
@time for f in fns
    println(f)
    a2[:,:,:,i] = read_ics(dir * fns[i])
    i = i+1;
end

    
@time extrema(abs(a)) # 3.7s

a = reshape(a,80*84,151*161)

@time acam = mean(abs(a),[3 4]);

write_pgm(acam,"/dev/shm/acam.pgm")

acamb = (acam .> quantile(reshape(acam,80*84),.5));

write_pgm(acamb*1.0,"/dev/shm/acamb.pgm")

@time aang = squeeze(mean(abs(a),[1 2]),[1 2]);

aangb = (aang .> quantile(reshape(aang,151*161),.6));


write_pgm(aang,"/dev/shm/aang.pgm")
write_pgm(aangb,"/dev/shm/aangb.pgm")
asmall=reshape(a,80*84,151*161)[reshape(acamb,80*84),reshape(aangb,151*161)]


asmall2=reshape(a2,80*84,151*161)[reshape(acamb,80*84),reshape(aangb,151*161)]

    ## julia> size(asmall)
## (3360,9724)

@time svdobj  = svdfact(asmall); 
## elapsed time: 65.275851006 seconds (1060689224 bytes allocated)


# julia> size(a)
# (6720,24311)
# elapsed time: 632.120279614 seconds (6320399940 bytes allocated)

# diagm(s)
# svdfact is  more efficient than sv

cd("/dev/shm/") do
    open("outfile","w") do f
        s = svdobj[:S];
        for i=1:length(s)
            @printf(f,"%d %f\n",i,s[i])
        end
    end
end


cd("/dev/shm/") do
    open("outfile.gp","w") do f
        println(f,"""set term posts; set grid;set outpu "/dev/shm/plot.ps"; set log y; plot "/dev/shm/outfile" u 1:2 w l""")
    end
end

run(`gnuplot /dev/shm/outfile.gp`)


run(`evince /dev/shm/plot.ps`)



@time for i=1:2000
    urec = Array(Complex64,80*84);
    urec[reshape(acamb,80*84)]= svdobj[:U][:,i];
    try
        write_pgm(reshape(abs(urec),80,84),@sprintf("/dev/shm/%04d.pgm",i))
    catch
    end
end

@time for i=1:2000
    vrec = Array(Complex64,151*161);
    vrec[reshape(aangb,151*161)]= svdobj[:V][:,i];
    try
        write_pgm(reshape(abs(vrec),151,161),@sprintf("/dev/shm/v%04d.pgm",i))
    catch
    end
end

@time asmalli=pinv(asmall);
#  elapsed time: 78.32984076 seconds (1626177148 bytes allocated)


@time recon = asmalli * asmall2;
# 31s

## julia> size(asmall2)
## (3360,9724)

## julia> size(asmall)
## (3360,9724)

## julia> size(recon)
## (9724,9724)


@time for i=1:9724
    rec = Array(Complex64,151*161);
    rec[reshape(aangb,151*161)]= recon[:,i];
    try
        write_pgm(reshape(abs(rec),151,161),@sprintf("/dev/shm/r%04d.pgm",i))
    catch
    end
end

# i only have openh264 right now and this encoder needs images sizes to be multiples of 16:
# for i in r*.pgm ;do pnmcut -width 144 -height 160 $i > cut.pgm ; convert cut.pgm o.yuv ; cat o.yuv >> ../o.yuv;done






# this gives an overview of some datatypes and their ranges
for T = {Int8,Int16,Int32,Int64,Int128,Uint8,Uint16,Uint32,Uint64,Uint128}
    println("$(lpad(T,7)): [$(typemin(T)),$(typemax(T))]")
end



# read a pgm file

function read_pgm(fn)
    l=open(fn,"r")
    l1=readline(l)
    l1=="P5\n"
    l2=readline(l)
    split(l2)
    (w,h)=map(int,split(l2))
    l3=readline(l)
    65535 == int(split(l3)[1])
    buf=read(l,Uint16,w,h)
    buf2=copy(buf)
    for i=1:size(buf,1), j=1:size(buf,2)
        buf2[i,j]=bswap(buf[i,j])
    end
    buf2
end

buf2=read_pgm("/media/sdc1/dat/3/i1387_j2527_1_012845.00.pgm")


function find_pgm_files(dir) 
    fns = readdir(dir)
    res = [];
    for f in fns
        m = match(r"^i.*\.pgm$",f) 
        if m != nothing
            res = vcat(res,m.match);
        end
    end
    res
end

dir4="/media/sdc1/dat/3/";

find_pgm_files(dir4)


write_pgm(buf2, "/dev/shm/o.pgm")

fft(buf2)

write_pgm(abs(fft(buf2)), "/dev/shm/of.pgm")


using Readline
using REPL
using ImageView
using Images
a=imread("/home/martin/Downloads/franck/a59120v.bmp")

help(display)

display(a)

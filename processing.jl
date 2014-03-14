x = 1.0

for T = {Int8,Int16,Int32,Int64,Int128,Uint8,Uint16,Uint32,Uint64,Uint128}
    println("$(lpad(T,7)): [$(typemin(T)),$(typemax(T))]")
end

eps(Float64)

z=1+2im
real(z)
imag(z)
conj(z)
abs(z)
abs2(z)
angle(z)
complex(1,3)


function f(x,y)
    x + y
end


map(x->x^2+2x-1,[1,3,-1])
map(x->x^2+2x-1,[1 3 -1])


## function f(x,y)
##     x + y, x*y
## end

x= (3,4)
f(x...)

function blop(num, base=10)
    base + num
end
blop(10)
blop(10,1)


function psf(x,y,z; na=1.4, ri=1.518)
    [x y z na ri]
end
psf(12, 12, 12, na=1.)


map([1, 2, 4]) do x
    if x<0 && iseven(x)
        0
    elseif x==0
        1
    else
        x
    end
end

cd("/dev/shm/") do
    open("outfile","w") do f
        write(f,"bla")
    end
end


z = begin
    x=1
    y=3
    x/y
end

z = (x=1;y=4;x+y)

# conditional expressions must not be anything but true or false

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

for i=0:2, j=3:4
    println([i j])
end

whos()

copy(a)
deepcopy(a)

(x = rand(8);Float64[ .25x[i-1]+.5x[i]+.25x[i+1] for i=2:length(x)-1])

SVD(x,true) # for thinning

diag

# julia is 1 based
# arrays are column major ordered (fortran)

extrema([1 2 3 4 5])
findmin([1 2 3 4 5])

"Hallo" * " Welt"




match(r"^j....\.ics$","j1234.ics").match
match(r"^j....\.ics$","bla.mat").match


vcat([1,2,3],[4,5,6])

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


ismatch(r"^end$","end\n")



function find_ics_raw_start(fn)
    # locate the word "end" in the ics file
    f=open(fn)
    while !ismatch(r"end",readline(f))
    end
    res = position(f)
    close(f)
    res
end
#find_ics_raw_start(dir * first(fns))

function read_ics(fn)
    pos = find_ics_raw_start(fn)
    f=open(fn)
    seek(f,pos)
    a=read(f,Complex64,80,84,151)
    close(f)
    a
end

a = read_ics(dir * first(fns));
          
(filesize(dir * first(fns))-519)/(80*84*16)

size(a[:,:,1])

function write_pgm(a,fn="/dev/shm/o.pgm")
    ar=reshape(a,prod(size(a)))
    mi,ma = extrema(ar)
    s = 0;
    if ma!=mi
        s=255/(ma-mi);
    else
        s=1;
    a8=uint8(min(255,max(0,floor((ar.-mi)*s))))
    f=open(fn,"w")
    @printf(f,"P5\n%d %d\n255\n",size(a,1),size(a,2));
    write(f,a8);
    close(f);
end

write_pgm(abs(a[:,:,1]))

a=Array(Complex64,80,84,151,161);
fns=find_ics_files(dir)
i = 1;
@time for f in fns
    println(f)
    a[:,:,:,i] = read_ics(dir * fns[i])
    i = i+1;
end

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

    
size(svdobj[:V])
   
extrema(abs(urec))

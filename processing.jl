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


function f(x,y)
    x + y, x*y
end

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
find_ics_raw_start(dir * first(fns))



read(

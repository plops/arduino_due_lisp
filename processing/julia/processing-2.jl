
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



find_ics_raw_start(ics_file)

# this function 
# jumps over the header
# and reads the raw data from the file into an array
# (i keep the dimensions fixed for this example)
function read_ics(fn)
    pos = find_ics_raw_start(fn)
    f=open(fn)
    seek(f,pos)
    a=read(f,Complex64,66,66,3,167,127)
    close(f)
    a
end

ics_file = "/media/sdc1/dat/0723/o4.ics"
a = read_ics(ics_file)

# check that the dimensions make sense
(filesize(ics_file)-602)/(66*66*125*95*3)

size(a[:,:,1,:,:])



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


## CAM (ID  BINX BINY W H   X   Y  KX  KY  D  G    E     NAME)
## 0 (21433565 2 2 512 512 249  17 167 478 66  0  2800 "transmission with polrot (top)")
## 1 (21433566 1 1 580 580 520 215 220  11 66 28 21040 "backreflection with polrot")
## 2 (21433540 2 2 512 512 365   0 101 138 66  0  2800 "transmission same pol")
      
# the data in a is the raw acquisition data



camname=["tran_perp" "refl_perp" "tran_para"]
@time for i = 1:3 
    ds = (squeeze(mean(abs2(ifft(a,[1 2])[:,:,i,:,:]),[4 5]),[4 5]));
    name = camname[i];
    fn = "/dev/shm/fiber_endface_intens_$name";
    write_pgm(ds,fn * ".pgm");
    run(`convert $fn.pgm $fn.jpg`);
end

size(ds)

@time extrema(abs(a)) # 4.9s

for i = 1:3
    pt = (squeeze(mean(abs2(a[:,:,i,:,:]),[1 2]),[1 2 3]));
    name = camname[i];
    fn = "/dev/shm/angular_throughput_$name";
    write_pgm(pt,fn * ".pgm");
    run(`convert $fn.pgm $fn.jpg`);
 end

run(`scp /dev/shm/a1.jpg /dev/shm/a2.jpg /dev/shm/a3.jpg martin@dr-kielhorn.eu:/var/www/2014`)

for cam = 1:3
    mosaic = reshape(a[:,:,:,:,cam],66*25,66*19)
    for i=1:25
        for j=1:19
            for u=1:65
                for v=1:65
                    mosaic[(i-1)*66+u,(j-1)*66+v]=a[u,v,i,j,cam]
                end
            end
        end
    end
    write_pgm(abs(mosaic),"/dev/shm/m$cam.pgm")
    run(`convert /dev/shm/m$cam.pgm /dev/shm/m$cam.jpg`)
end

run(`scp /dev/shm/m1.jpg /dev/shm/m2.jpg /dev/shm/m3.jpg martin@dr-kielhorn.eu:/var/www/2014`)



norm(a[:,:,1,4,cama])

pearsonrr = zeros(25,19);
pearsonri = zeros(25,19);
pearsonir = zeros(25,19);
pearsonii = zeros(25,19);     
begin
    cama = 1
    camb = 3
    for i=1:25, j=1:19
        car = real(a[:,:,i,j,cama])
        cbr = real(a[end-1:1,:,i,j,camb])
        cai = imag(a[:,:,i,j,cama])
        cbi = imag(a[end-1:1,:,i,j,camb])
        pearsonrr[i,j] = sum(car .* cbr)/(norm(car) * norm(cbr));
        pearsonri[i,j] = sum(car .* cbi)/(norm(car) * norm(cbi));
        pearsonir[i,j] = sum(cai .* cbr)/(norm(cai) * norm(cbr));
        pearsonii[i,j] = sum(cai .* cbi)/(norm(cai) * norm(cbi)); 
    end
    #write_pgm(abs(pearson),"/dev/shm/p$cama$camb.pgm")
end


# note:
# think about reflection on one camera
# inverse fourier transform
# use mask
# global phase between images, maybe svd of [rr, ir; ri, ii], find rotation


show(floor(pearsonrr,2))

show(floor(pearsonrr[7:13,7:13],2))
show(floor(pearsonri[7:13,7:13],2))
show(floor(pearsonii[7:13,7:13],2))

abs(pearson)


    
@time begin
    kw=66
    kh=66
    w=25
    h=19
    n=1
    acam = zeros(kw,kh,n,3)
    acamb = zeros(kw,kh,n,3)
    aang = zeros(w,h,n,3)
    aangb = zeros(w,h,n,3)
    for (i, file) in [(2,"2")] # (3, "3")]
        ics_file = "/home/martin/scan0714_$file.ics"
        a = read_ics(ics_file);
        for cam=1:3
            acam[:,:,i,cam] = log(squeeze(mean(abs(a[:,:,:,:,cam]),[3 4]),[3 4]));
            #acamb[:,:,i,cam] = (acam[:,:,i,cam] .> quantile(reshape(acam[:,:,i,cam],kw*kh),.5f0));
            write_pgm(acam[:,:,i,cam],"/dev/shm/acam$cam.pgm")
            #write_pgm(acamb[:,:,i,cam]*1.0,"/dev/shm/acamb_$i-$cam.pgm")
            aang[:,:,i,cam] = log(squeeze(mean(abs(a[:,:,:,:,cam]),[1 2]),[1 2]));
            #aangb[:,:,i,cam] = (aang[:,:,i,cam] .> quantile(reshape(aang[:,:,i,cam],w*h),.6f0));
            ##  write_pgm(aang[:,:,i,cam],"/dev/shm/aang_$file-$cam.pgm")
            #write_pgm(aangb[:,:,i,cam],"/dev/shm/aangb_$i-$cam.pgm")
        end
    end
end


#            asmall=reshape(a,66*66,125*95)[reshape(acamb,66*66),reshape(aangb,125*95)]






write_pgm(aangb,"/dev/shm/aangb.pgm")

    ## julia> size(asmall)
## (3360,9724)

@time svdobj  = svdfact(asmall); 
## elapsed time: 65.275851006 seconds (1060689224 bytes allocated)


# julia> size(a)
# (6720,24311)
1# elapsed time: 632.120279614 seconds (6320399940 bytes allocated)

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

find_pgm_files(dir4);

begin
    res = [];
    imgs = 
    for f in find_pgm_files(dir4)
        m = match(r"^i(.*)_j(.*)_1_(.*)\.pgm$",f) 
        if m != nothing
            i = div(int(m.captures[1])-547,40)+1;
            j = div(int(m.captures[2])-447,40)+1;
            expos = float(m.captures[3]);
            res = vcat(res,(m.match,i,j,expos));
        end
    end
    res
end
            

   

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

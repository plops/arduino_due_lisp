#push!(LOAD_PATH,".")
include("extract.jl")
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
# this function 
# jumps over the header
# and reads the raw data from the file into an array
# (i keep the dimensions fixed for this example)
function read_ics(fn)
    pos = find_ics_raw_start(fn)
    f=open(fn)
    seek(f,pos)
    a=read(f,Complex64,64,64,3,120,120)
    close(f)
    a
end

#ics_file = "/home/martin/arduino_due_lisp/processing/julia/20150224/o-pol0.ics"
#ics_file2 = "/home/martin/arduino_due_lisp/processing/julia/20150224/o-pol1.ics"
ics_file = "/dev/shm/o-pol0.ics"
ics_file2 = "/dev/shm/o-pol1.ics"
@time a = read_ics(ics_file);
@time b = read_ics(ics_file2);

size(a)

c=cat(6,a,b);

size(c)

#ics_file = "/media/sdc1/dat/0805/orot9.ics"
#ar = read_ics(ics_file)
#ics_file = "/media/sdc1/dat/0805/o8.ics"
#a8 = read_ics(ics_file)

# check that the dimensions make sense
(filesize(ics_file)-602)/(66*66*125*95*3)

q= convert(Array{Int32,5},map((x)->div(abs2(x),1),a));

d = Array(Complex{Float32},64*64*4,120*120)

@time tinv=pinv(reshape(c[:,:,2,:,:,:],64*64,120*120*2))

using View5D

size(squeeze(abs2(a[16,16,1,:,:]),(1,2,3)))

view5d(abs2(a[:,:,1,20,16]))

squeeze(abs2(a[16,:,2:,:]),(1,3))


view5d(squeeze(abs2(a[:,:,1,:,:]),(3)))

view5d(squeeze(abs2(a[:,:,1,:,:]),[3]))

view5d(abs2(fft(squeeze(a[:,:,1,20,16],[3,4,5]))))

b=fft(squeeze(a[:,:,1,20,16],[3,4,5]))

view5d(abs2(ifft(extract_martin(b,[512,512],[45,45],complex(0.0)))))


extract_martin(squeeze(a[:,:,1,:,:],[3]),[128 128 47 37],[45 45 23 18],0.0im)

ac=mean(map((x)->abs2(x),squeeze(a[:,:,3,:,:],3)),(3,4,5))

b=0

delete(a)

view5d(ac)

size(squeeze(a[:,:,2,:,:],3))

view5d(squeeze(mean(map((x)->abs2(x),squeeze(a[:,:,2,:,:],3)),(1,2)),(1,2)))

view5d(squeeze(mean(map((x)->abs2(x),squeeze(a[:,:,2,:,:],3)),(3,4)),(3,4)))

view5d(squeeze(mean(map((x)->abs2(x),squeeze(c[:,:,1,:,:,:],3)),(3,4,5))))

view5d((mean(map((x)->abs2(x),squeeze(c[:,:,1,:,:,:],3)),(3,4,5))))

avg_1=squeeze(mean(map((x)->abs2(x),squeeze(c[:,:,1,:,:,:],3)),(3,4,5)),(3,4,5));
avg_3=squeeze(mean(map((x)->abs2(x),squeeze(c[:,:,3,:,:,:],3)),(3,4,5)),(3,4,5));

avgm_1=squeeze(mean(map((x)->abs2(x),squeeze(c[:,:,1,:,:,:],3)),(1,2,5)),(1,2,5));
avgm_3=squeeze(mean(map((x)->abs2(x),squeeze(c[:,:,3,:,:,:],3)),(1,2,5)),(1,2,5));

view5d(hcat(avg_1,avg_3))

bin_1 = (avg_1 .> quantile(reshape(avg_1,64*64),.5))
bin_3 = (avg_3 .> quantile(reshape(avg_3,64*64),.5))

binm_1 = (avgm_1 .> quantile(reshape(avgm_1,120*120),.2))
binm_3 = (avgm_3 .> quantile(reshape(avgm_3,120*120),.2))


bin13 = bin_1 .* bin_3

binm13 = binm_1 .* binm_3

reshape(c[:,:,1,:,:,1],64*64,120*120)[reshape(bin13,64*64),reshape(binm13,120,120)]

c0=reshape(c[:,:,1,:,:,1],64*64,120*120)[reshape(bin13,64*64),reshape(binm13,120*120)]

view5d((binm13)*1.0)

sum((bin13)*1.0) # 1921

sum((binm13)*1.0) # 11498

view5d(cat(3,bin_1,bin_3)*1.0)

view5d((avg_3 .> quantile(reshape(avg_3,64*64),.5))*1.0)

begin
#    ap = squeeze(a[:,:,1,20,16],[3,4,5]);
    ap=squeeze(mean(map((x)->abs2(x),squeeze(a[:,:,3,:,:],3)),[3 4]),[3 4])
    kap= fft(ap);
    cor= ifft(kap .* kap)
    view5d(abs(fftshift(cor)))
end
# find the center of the first order:
# results: 1 [44 45], 2 [41 43], 3 [36 42]

cor=zeros(90,90);
for i=1:size(a,5)
    for j=1:size(a,6)
        ap=squeeze(a[:,:,1,i,j],[3 4 5])
        kap= fft(ap);
        cor[:,:] += abs(ifft(kap .* kap))
    end
end
view5d(fftshift(cor))

view5d((squeeze(a[:,:,1,:,:],[3])))

ka=squeeze(mean(abs2(ifft(a[:,:,1,:,:],[1 2])),[4 5]),[3,4,5])

view5d(convert(Array{Float64,2},ka))

view5d(ka)

typeof(convert(Array{Float64,2},ka))

a1 = reshape(a[:,:,1,:,:],90*90,47*37);


function avg_endface(a,cam)
    mean(abs2(ifft(a[:,:,cam,:,:],[1 2])),[4 5])
end
write_pgm(avg_endface(a,1),"/dev/shm/os1.pgm");
write_pgm(avg_endface(a,3),"/dev/shm/os3.pgm");
write_pgm(avg_endface(ar,1),"/dev/shm/or1.pgm");
write_pgm(avg_endface(ar,3),"/dev/shm/or3.pgm");

function bin_avg_endface(a,cam)
    q=avg_endface(a,cam);
    q .> quantile(reshape(q,90*90),.5)
end

write_pgm(bin_avg_endface(a,1),"/dev/shm/bos1.pgm");
write_pgm(bin_avg_endface(a,3),"/dev/shm/bos3.pgm");
write_pgm(bin_avg_endface(ar,1),"/dev/shm/bor1.pgm");
write_pgm(bin_avg_endface(ar,3),"/dev/shm/bor3.pgm");

bin_all= bin_avg_endface(a,1) & bin_avg_endface(a,3) & bin_avg_endface(ar,1) & bin_avg_endface(ar,3);
write_pgm(bin_all,"/dev/shm/bin_all.pgm");


function avg_angle(a,cam)
    squeeze(mean(abs2(a[:,:,cam,:,:]),[1 2]),[1,2,3])
end

function bin_avg_angle(a,cam)
    q=avg_angle(a,cam);
    q .> quantile(reshape(q,47*37),.3)
end

abin_all= bin_avg_angle(a,1) & bin_avg_angle(a,3) & bin_avg_angle(ar,1) & bin_avg_angle(ar,3);
write_pgm(abin_all,"/dev/shm/abin_all.pgm");

asmall=vcat(reshape(a[:,:,1,:,:],90*90,47*37)[reshape(bin_all,90*90),reshape(abin_all,47*37)],
            reshape(ar[:,:,1,:,:],90*90,47*37)[reshape(bin_all,90*90),reshape(abin_all,47*37)],
            reshape(a[:,:,3,:,:],90*90,47*37)[reshape(bin_all,90*90),reshape(abin_all,47*37)],
            reshape(ar[:,:,3,:,:],90*90,47*37)[reshape(bin_all,90*90),reshape(abin_all,47*37)])

@time inv_asmall = pinv(asmall) # takes 16s

write_pgm(abs(asmall),"/dev/shm/asmall.pgm")

size(a1)

begin
    img = zeros(4,3)
    center = map((x)->div(x,2),size(img))
    asize = [5 5]
    if size(asize,1) > 1
        asize = asize'
    end
    if size(center,1) > 1
        center = center'
    end
    isize = size(img)
    if length(asize) < length(isize)
        for d = length(asize)+1:length(isize)
            asize(d)=size(img,d);
        end
    end
    if length(center) < length(isize)
        for d = length(center)+1:length(isize)
            center(d)=div(size(img,d),2);
        end
    end
    srccenter=center'
    if length(asize) > length(srccenter)
        srccenter[end+1:length(asize)]=0
        isize[end+1:length(asize)]=1
    end
    srcstart = map((x,y)->x-div(y,2),srccenter,asize)
    srcend=srcstart+asize
    dststart=zeros(length(asize))
end




a=rand(5,5)
b=zeros(2,2)

b[:,:]=a[[(1,2),(1,2)]]

b[:,:]=a[range(1,2),range(1,2)]

begin
    qq=1
    bb=2
    b[:,:]=a[range(qq,bb),range(qq,bb)]
end


begin
    qq=1
    bb=2
    q=map(range,[qq qq],[bb bb])
    #b[:,:]=a[q]
    b[:,:]=getindex(a,q...)
end


begin
    qq=1
    bb=2
    q=range(qq,bb)
    b[:,:]=a[q,q]
end

tuple([1,2,3]...)

q=[1,2,3]
tuple(q...)

apply((x,y)->slice(a,x,y),[range(1,2),range(1,2)])

tuple([range(1,2), range(1,2)]...)

map(range,[1,1],[2,2])

b[:,:]=a[expand_ranges([1,1],[2,2])]

expand_ranges([1,1],[2,2])

begin
    a = zeros(10,10)
    sizea = 10
#    if length(sizea) < length(size(a))
#        append!(sizea,map(identity,size(a)[length(sizea)+1:end]))
#    end
    sizea
    map(identity,size(a))
end






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

using Winston

x = linspace(0, 3pi, 100)
c = cos(x)
s = sin(x)
p = FramedPlot(title="title!",xlabel="\\Sigma x^2_i",ylabel="\\Theta_i")
add(p, FillBetween(x, c, x, s))
add(p, Curve(x, c, color="red"))
add(p, Curve(x, s, color="blue"))
p

savefig(p,"/dev/shm/o.png")



camname=["tran_perp" "refl_perp" "tran_para"]
ds = zeros(Float32,90,90,3);
@time for i = 1:3 
    ds[:,:,i] = (squeeze(mean(abs2(ifft(a,[1 2])[:,:,i,:,:]),[4 5]),[4 5]));
    name = camname[i];
    fn = "fiber_endface_intens_r_$name";
    #if(i==3)
    #    ds[:,:,i] = ds[90:-1:1,:,i]
    #end
    write_pgm(ds[:,:,i],"/dev/shm/" * fn * ".pgm");
    #savefig(imagesc(ds[:,:,i]),"/dev/shm" * fn * ".png")
    #run(`convert /dev/shm/$fn.png /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn.jpg`);
end # elapsed time: 22.026625532 seconds (9983991416 bytes allocated, 2.92% gc time)








@time extrema(abs(a)) # 4.9s

for i = 1:3
    pt = (squeeze(mean(abs2(a[:,:,i,:,:]),[1 2]),[1 2 3]));
    name = camname[i];
    fn = "/dev/shm/angular_throughput_r_$name";
    write_pgm(pt,fn * ".pgm");
    run(`convert $fn.pgm $fn.jpg`);
end

# with imagej i select a circle in the angular scan: 63x63+36+20
# sort them like this:
#     1
#
#  2  3  4
#
#     5

cx = 36+floor(63/2)
cy = 20+floor(63/2)
top = [cx 20]
left = [36 cy]
middle = [cx cy]
right = [36+63 cy]
bottom = [cx 20+63]
pos = [top; left; middle; right; bottom]
for k=1:3
    for i=1:size( pos,1 )
        im = abs(ifft(squeeze(a[:,:,k,pos[i,1],pos[i,2]],[3,4,5]),[1 2]));
        if(k==3)
            im = im[66:-1:1,:]
        end
        fn = "/dev/shm/fiber_coherent_intens_$i-$k";
        write_pgm(im,fn*".pgm")
        run(`convert $fn.pgm $fn.jpg`);
    end
end


# calculate correlation to the central image of the other polarization
@time begin
    cx = 36+floor(63/2)
    cy = 20+floor(63/2)
    w = size(a,4)
    h = size(a,5)
    ka=ifft(a,[1 2]);
    pearson = zeros(Complex{Float32},w,h,3);
    for cam = 1:3
        la = squeeze((cam == 3)?ka[end:-1:1,:,cam,cx,cy]:ka[:,:,cam,cx,cy],[3,4,5]);
        nla = norm(la);
        for i=1:w, j=1:h
            lb = squeeze((cam == 3)?ka[end:-1:1,:,cam,i,j]:ka[:,:,cam,i,j],[3,4,5]);
            pearson[i,j,cam] = sum(la .* conj(lb))/(nla * norm(lb));
        end
        name = camname[cam];
        fn = "pearson_center_$name";
        write_pgm(abs(pearson[:,:,cam]),"/dev/shm/$fn.pgm")
        run(`convert /dev/shm/$fn.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn.jpg`)
    end
end # elapsed time: 132.095573614 seconds (14144661320 bytes allocated, 6.02% gc time)

savefig(imagesc(abs(hcat(pearson[:,:,2], pearson[:,:,1], pearson[:,:,3],pearson_c))),"/home/martin/arduino_due_lisp/processing/julia/step12_0724/pearson_center_all.png");
run(`convert /home/martin/arduino_due_lisp/processing/julia/step12_0724/pearson_center_all.png /home/martin/arduino_due_lisp/processing/julia/step12_0724/pearson_center_all.jpg`);




# cross correlation between average images of cameras 1 and 3
begin
    write_pgm(abs(ifft(fft(ds[:,:,1]).*conj(fft(ds[:,:,3])))),"/dev/shm/correlate_trans_avg.pgm")
end
    
# compare images of camera 1 and 3 
@time begin
    w = size(a,4)
    h = size(a,5)
    pearson_c = zeros(Complex{Float32},w,h);
    pearsonaa_c = zeros(w,h);
    pearsonrr_c = zeros(w,h);
    pearsonri_c = zeros(w,h);
    pearsonir_c = zeros(w,h);
    pearsonii_c = zeros(w,h);     
    for i=1:w, j=1:h
        cam = 1;
        la = squeeze((cam == 3)?ka[end:-1:1,:,cam,i,j]:ka[:,:,cam,i,j],[3,4,5]);
        cam = 3;
        lb = squeeze((cam == 3)?ka[end:-1:1,:,cam,i,j]:ka[:,:,cam,i,j],[3,4,5]);
        pearson_c[i,j] = sum(la .* conj(lb))/(norm(la) * norm(lb));
        car = real(la)
        cai = imag(la)
        caa = abs(la)
        cbr = real(lb)
        cbi = imag(lb)
        cba = abs(lb)
        pearsonrr_c[i,j] = sum(car .* cbr)/(norm(car) * norm(cbr));
        pearsonri_c[i,j] = sum(car .* cbi)/(norm(car) * norm(cbi));
        pearsonir_c[i,j] = sum(cai .* cbr)/(norm(cai) * norm(cbr));
        pearsonii_c[i,j] = sum(cai .* cbi)/(norm(cai) * norm(cbi));
        pearsonaa_c[i,j] = sum(caa .* cba)/(norm(caa) * norm(cba)); 
    end
    fn = "pearson_tran";
    write_pgm(abs(pearson_c[:,:]),"/dev/shm/$fn.pgm")
    write_pgm(abs(pearsonrr_c[:,:]),"/dev/shm/$fn-rr.pgm")
    write_pgm(abs(pearsonri_c[:,:]),"/dev/shm/$fn-ri.pgm")
    write_pgm(abs(pearsonir_c[:,:]),"/dev/shm/$fn-ir.pgm")
    write_pgm(abs(pearsonii_c[:,:]),"/dev/shm/$fn-ii.pgm")
    write_pgm(abs(pearsonaa_c[:,:]),"/dev/shm/$fn-aa.pgm")
    run(`convert /dev/shm/$fn.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn.jpg`)
    run(`convert /dev/shm/$fn-rr.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn-rr.jpg`)
    run(`convert /dev/shm/$fn-ri.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn-ri.jpg`)
    run(`convert /dev/shm/$fn-ir.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn-ir.jpg`)
    run(`convert /dev/shm/$fn-ii.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn-ii.jpg`)
    run(`convert /dev/shm/$fn-aa.pgm /home/martin/arduino_due_lisp/processing/julia/step12_0724/$fn-aa.jpg`)
end # elapsed time: 391.645044235 seconds (20057144992 bytes allocated, 3.58% gc time)

begin
    d = 5
    int(100*abs(pearson_c[cx-d:cx+d,cy-d:cy+d]))
end


begin
    d = 5
    int(100*abs(pearson[cx-d:cx+d,cy-d:cy+d,1]))
end


begin
    d = 5
    int(100*abs(pearson[cx-d:cx+d,cy-d:cy+d,2]))
end


begin
    d = 5
    int(100*abs(pearson[cx-d:cx+d,cy-d:cy+d,3]))
end


function pearson_coef(a,b,cama,camb)
    pearson = zeros(Complex{Float32},47,37);
    for i=1:47
        for j=1:37
            xa = squeeze(a[:,:,cama,i,j],[3 4 5]);
            xb = squeeze(b[:,:,camb,i,j],[3 4 5]);
            ma = mean(xa);
            mb = mean(xb);
            ad = xa-ma;
            bd = xb-mb;
            pearson[i,j] = sum(ad.*conj(bd))/sqrt(sum(abs2(ad))*sum(abs2(bd)));
        end
    end
    pearson
end
write_pgm(abs(pearson_coef(a,ar,1,3)),"/dev/shm/pear_o9-1_or9-3.pgm")
write_pgm(abs(pearson_coef(a,ar,3,1)),"/dev/shm/pear_o9-3_or9-1.pgm")
write_pgm(abs(pearson_coef(a,ar,2,2)),"/dev/shm/pear_o9-2_or9-2.pgm")
write_pgm(abs(pearson_coef(a,a8,1,1)),"/dev/shm/pear_o9-1_o8-1.pgm")
write_pgm(abs(pearson_coef(a,a8,2,2)),"/dev/shm/pear_o9-2_o8-2.pgm")
write_pgm(abs(pearson_coef(a,a8,3,3)),"/dev/shm/pear_o9-3_o8-3.pgm")
write_pgm(abs(pearson_coef(a,a8,1,3)),"/dev/shm/pear_o9-1_o8-3.pgm")
write_pgm(abs(pearson_coef(a,a8,3,1)),"/dev/shm/pear_o9-3_o8-1.pgm")
write_pgm(abs(pearson_coef(a,a,1,3)),"/dev/shm/pear_o9-1_o9-3.pgm")
write_pgm(abs(pearson_coef(a,a,3,1)),"/dev/shm/pear_o9-3_o9-1.pgm")

        

for cam = 1:3
    mosaic = reshape(a[:,:,cam,:,:],90*47,90*37)
    for i=1:47
        for j=1:37
            for u=1:90
                for v=1:90
                    mosaic[(i-1)*90+u,(j-1)*90+v]=a[u,v,cam,i,j]
                end
            end
        end
    end
    write_pgm(abs(mosaic),"/dev/shm/m$cam.pgm")
    #write_pgm(angle(mosaic),"/dev/shm/ma_r$cam.pgm")
    run(`convert /dev/shm/m$cam.pgm /dev/shm/m$cam.jpg`)
    #run(`convert /dev/shm/ma_r$cam.pgm /dev/shm/ma_r$cam.jpg`)
end
for cam = 1:3
    mosaic = reshape(a[:,:,cam,:,:],90*47,90*37)
    for i=1:47
        for j=1:37
            for u=1:90
                for v=1:90
                    mosaic[(i-1)*90+u,(j-1)*90+v]=ar[u,v,cam,i,j]
                end
            end
        end
    end
    write_pgm(abs(mosaic),"/dev/shm/m_r$cam.pgm")
    #write_pgm(angle(mosaic),"/dev/shm/ma_r$cam.pgm")
    run(`convert /dev/shm/m_r$cam.pgm /dev/shm/m_r$cam.jpg`)
    #run(`convert /dev/shm/ma_r$cam.pgm /dev/shm/ma_r$cam.jpg`)
end

# image from cam 2 didn't cut out fourier order correctly

run(`scp /dev/shm/m1.jpg /dev/shm/m2.jpg /dev/shm/m3.jpg martin@dr-kielhorn.eu:/var/www/2014`)




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

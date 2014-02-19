%% -*- mode: Octave;-*-

% evaluate data captured on 20140217
% we repaired the optical floating table and i added code to ensure
% making use of the cameras full dynamic range

%% filenames are like this:
% i2747_j2147_2_000253.00.pgm
% i3147_j3147_1_001382.00.pgm
% i, j      .. x and y deflection of the steering mirror
% 1 or 2    .. camera (1 is cmos, 2 is ccd)
% xxxxxx.xx .. exposure time for this image (genicam value, probably us)

% it complains about extra data (probably the additional newline after
% color number), but loading my pgm images seems to work anyway

folder = '/dev/shm/20140217/'
fns=dir([folder '*_2_*.pgm']); % get all filenames of images of camera 2
temp=readim([folder fns(1).name]); % just open one image to get the
dimensions
w = size(temp,1);
h = size(temp,2);
z = size(fns,1);
im = newim([w h z], 'single');
clear temp;
for k=1:size(fns)
  A=sscanf(fns(k).name,'i%d_j%d_%d_%g.pgm');
  exposure=A(4);
% the 100 is an artificial offset that i add during acquisition when
% subtracting the background
  im(:,:,k-1)=(readim([folder fns(k).name])-100)./exposure;
end

%im=reshape(im,[w h 16 16]);
%im=reshape(im,[w h 256]);

a2 = newim([96 96 z],'complex');
for k=0:z-1
  a2(:,:,k) = ift(extract(ft(squeeze(im(:,:,k))),[96 96],[156 69]));
end

% writeim(abs(a2),'/dev/shm/a2.tif')


fns=dir([folder '*_1_*.pgm']); % get all filenames of images of camera 1
temp=readim([folder fns(1).name]); 
w = size(temp,1);
h = size(temp,2);
z = size(fns,1);
im = newim([w h z], 'single');
clear temp;
for k=1:size(fns)
  A=sscanf(fns(k).name,'i%d_j%d_%d_%g.pgm');
  exposure=A(4);
  im(:,:,k-1)=(readim([folder fns(k).name])-100)./exposure;
end


  % due to the large pixel size of the cmos i ended up with an aliased
  % hologram

%ka=newim([w h],'single');
%for k=0:z-1
%  ka(:,:)=ka(:,:)+abs(ft(im(:,:,k)));
%end
%log(abs(cat(1,ka,ka))))
%DampEdge(extract(log(abs(cat(1,ka,ka))),[118 118],[514 312]),.08,2,1,2)


a1 = newim([118 118 z],'complex');
for k=0:z-1
  ka = ft(DampEdge(squeeze(im(:,:,k)),.08,2));
  ka = extract(cat(1,ka,ka),[118 118],[514 312]);
  a1(:,:,k) = ift(DampEdge(ka,.08,2,0,1));
end


%ft(a1(:,:,70))
% phase(a1)

% writeim(abs(a1),'/dev/shm/a1.tif')

% make a bigger image
%a1b = newim([256 256 z],'complex');
%for k=0:z-1
%  a1b(:,:,k)=ift(extract(ft(a1(:,:,k)),[256 256]));
%end

addpath('/home/martin/Arduino/arduino_due_lisp/')

unwph = newim([256 256 z],'single');
for k=0:z-1
  unwph(:,:,k)=volkov_unwrap(squeeze(a1b(:,:,k)));
end

mask = gaussf(mean(abs(a1b),[],3))>.1;

writeim(unwph.*mask,'/dev/shm/unwph.fits')


%% the cmos (cam1) has 8x8 um pixels
%% the ccd (cam2) has 5.6x5.6 um pixels
% find a good image integral image size for both
% factor(56) => 2 2 2 7
% mean(abs(a1),[],3)
% field diameter on cam1: 63 px = 7*9
% field diameter on cam2: 73 px
% 8*63/5.6 => 90

% extract(mean(abs(a1),[],3),[63 63],[57 53])
% extract(mean(abs(a2),[],3),[90 90],[47 52])

				% find centroid
a1c=center_centroid(a1);
ka1c=center_centroid(dip_fouriertransform(a1c,'forward',[1 1 0]));
a2c=center_centroid(a2);
ka2c=center_centroid(dip_fouriertransform(a2c,'forward',[1 1 0]));

big=[256 256];
blobs=cat(3,extract(mean(abs(a1c),[],3)./sum(abs(a1c)),big),extract(mean(abs(a2c),[],3)./sum(abs(a2c)),big),extract(mean(abs(ka1c),[],3)./sum(abs(ka1c)),big),extract(mean(abs(ka2c),[],3)./sum(abs(ka2c)),big));

rad1=24;rad2=26
dogs=cat(3,gaussf(blobs(:,:,2),rad1)-gaussf(blobs(:,:,2),rad2),gaussf(blobs(:,:,3),rad1)-gaussf(blobs(:,:,3),rad2))
radoncircle(minima(dogs(:,:,0)),[132/2:156/2])
radoncircle(minima(dogs(:,:,1)),[120/2:130/2])
[rt p o]=radoncircle(dx(real(squeeze(blobs(:,:,2)))).^2+dy(real(squeeze(blobs(:,:,2)))).^2,[43:2:64],1)
[rt p o]=radoncircle(dx(real(squeeze(blobs(:,:,3)))).^2+dy(real(squeeze(blobs(:,:,3)))).^2,[30:2:64],1)

aa=mean(abs(dip_fouriertransform(DampEdge(im,.08,2),'forward',[1 1 0])),[],3)
aa=squeeze(aa);
aa_z=aa.*gaussf(rr(aa,'freq')>.2 & shift(rr(aa,'freq'),[0 floor(size(aa,2)/2)])>.1,10); % get rid of dc peak and peak at nyquist
aa2=cat(1,aa_z,aa_z)

[rt p o]=radoncircle(dx(aa2).^2+dy(aa2).^2,[43:2:64],1)

cx=p(5,1);
cy=p(5,2);
rad=p(5,3);
diam=ceil(rad*2*1.1);
extract(aa2,[diam diam],[round(cx) round(cy)])
ka2=dip_fouriertransform(DampEdge(im,.08,2),'forward',[1 1 0]);
aas=dip_fouriertransform(extract(cat(1,ka2,ka2),[diam diam],[round(cx) round(cy)]),'inverse',[1 1 0])
clear(ka2);

aa3=squeeze(mean(abs(aas),[],3));
%% increase size so that radon hough circle finding works better
%% problems arise because there is some dust on the cmos camera, i use
%% a median filter against this
aa3big=medif(real(ift(extract(ft(aa3),[2*size(aa3,1) 2*size(aa3,2)]))),13);
[rt p o]=radoncircle(dx(aa3big).^2+dy(aa3big).^2,[48:2:68],1)

rcx=p(1,1)/2;
rcy=p(1,2)/2;
rrad=p(1,3);
rdiam=ceil(rrad*1.1);

min_a1=extract(aas,[rdiam rdiam],[round(rcx) round(rcy)])

% store minimal representation of the electric field on the cmos camera
save '/media/sda2/stabil-p/20140217_min_a1' min_a1
load '/media/sda2/stabil-p/20140217_min_a1' min_a1



aa=mean(abs(dip_fouriertransform(DampEdge(im,.08,2),'forward',[1 1 0])),[],3)
aa=squeeze(aa);
aa_z=aa.*gaussf(rr(aa,'freq')>.2 & shift(rr(aa,'freq'),[0 floor(size(aa,2)/2)])>.1,10); % get rid of dc peak and peak at nyquist
aa2=aa_z

[rt p o]=radoncircle(dx(aa2).^2+dy(aa2).^2,[32:1:40],1)

cx=p(1,1);
cy=p(1,2);
rad=p(1,3);
diam=ceil(rad*2*1.1);
extract(aa2,[diam diam],[round(cx) round(cy)])
ka2=dip_fouriertransform(DampEdge(im,.08,2),'forward',[1 1 0]);
aas=dip_fouriertransform(extract(ka2,[diam diam],[round(cx) round(cy)]),'inverse',[1 1 0])
clear ka2

aa3=squeeze(mean(abs(aas),[],3));
%% increase size so that radon hough circle finding works better
%% problems arise because there is some dust on the cmos camera, i use
%% a median filter against this
aa3big=medif(real(ift(extract(ft(aa3),[2*size(aa3,1) 2*size(aa3,2)]))),13);
[rt p o]=radoncircle(dx(aa3big).^2+dy(aa3big).^2,[52:1:62],1)

rcx=p(2,1)/2;
rcy=p(2,2)/2;
rrad=p(2,3);
rdiam=size(min_a1,1); %ceil(rrad*1.1);

min_a2=extract(aas,[rdiam rdiam],[round(rcx) round(rcy)])
dip_fouriertransform(min_a2,'forward',[1 1 0])

% store minimal representation of the electric field on the ccd camera
save '/media/sda2/stabil-p/20140217_min_a2' min_a2
load '/media/sda2/stabil-p/20140217_min_a2' min_a2


reshape(min_a2,[w h 16 16])
phase(ift(extract(ft(min_a1(:,:,70)),[256 256])))
phase(ift(extract(ft(min_a2(:,:,70)),[256 256])))

q=reshape(min_a1,[62*62 256]);
mc=corr(dip_array(q))
mc=corr(dip_array(q)')

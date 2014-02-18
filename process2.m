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
fns=dir([folder '*_2_*.pgm']); % get all filenames of images of camera
2
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

writeim(abs(a2),'/dev/shm/a2.tif')


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

ka=newim([w h],'single');
for k=0:z-1
  ka(:,:)=ka(:,:)+abs(ft(im(:,:,k)));
end
log(abs(cat(1,ka,ka))))
DampEdge(extract(log(abs(cat(1,ka,ka))),[118 118],[514 312]),.08,2,1,2)


a1 = newim([118 118 z],'complex');
for k=0:z-1
  ka = ft(DampEdge(squeeze(im(:,:,k)),.08,2));
  ka = extract(cat(1,ka,ka),[118 118],[514 312]);
  a1(:,:,k) = ift(DampEdge(ka,.08,2,0,1));
end
%ft(a1(:,:,70))
% phase(a1)

writeim(abs(a1),'/dev/shm/a1.tif')

% make a bigger image
a1b = newim([256 256 z],'complex');
for k=0:z-1
  a1b(:,:,k)=ift(extract(ft(a1(:,:,k)),[256 256]));
end

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
int1=mean(abs(a1),[],3);
cent1=[sum(xx(int1).*int1)/sum(int1) sum(yy(int1).*int1)/sum(int1)]
int1c=real(ift(ft(int1)*exp(2*pi*i*(xx(int1,'freq')*cent1(1)+yy(int1,'freq')*cent1(2)))))

int2=mean(abs(a2),[],3);
cent2=[sum(xx(int2).*int2)/sum(int2) sum(yy(int2).*int2)/sum(int2)]

ka1s=dip_fouriertransform(extract(a1,[63 63],[57 53]),'forward',[1 1 0]);
ka2s=extract(dip_fouriertransform(extract(a2,[90 90],[47 52]),'forward',[1 1 0]),[63 63],[45 45]);

cat(3,mean(abs(ka1s),[],3),mean(abs(ka2s),[],3));

a1s= extract(a1,[63 63],[57 53]);
a2s=dip_fouriertransform(ka2s,'inverse',[1 1 0]);
cat(3,mean(abs(a1s),[],3),mean(abs(a2s),[],3));

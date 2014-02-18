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
% hologram, in order to easy extracting the first order i use fftn
% without fftshift
%ka=newim([w h],'single');
%for k=0:z-1
%  ka(:,:)=ka(:,:)+abs(dip_image(fftn(dip_array(im(:,:,k)))));
%end
%extract(ka,[112 112],[259 57])


a1 = newim([112 112 z],'complex');
for k=0:z-1
  a1(:,:,k) = ift(dip_image(fftshift(dip_array(extract(dip_image(fftn(dip_array(squeeze(im(:,:,k))))),[112 112],[259 57])))));
end

writeim(abs(a2),'/dev/shm/a2.tif')


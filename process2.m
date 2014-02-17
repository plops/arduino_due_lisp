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
temp=readim([folder fns(1).name]); % just open one image to get the dimensions
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

im=reshape(im,[w h 16 16]);

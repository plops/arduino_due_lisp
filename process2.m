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

a=readim('/dev/shm/20140217/i2747_j2147_2_000253.00.pgm');
fns=dir('/dev/shm/20140217/*_2_*.pgm');
A=sscanf(fns(254).name,'i%d_j%d_%d_%g.pgm');
exposure=A(4);

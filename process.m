%% convert pgm 16-bit data to tif with imagej
% the following was acquired with the sony icx ccd sensor
a = readtimeseries('/dev/shm/20140215/20140215_2.tif');
ka = newim([82 82 529],'complex');
for k=0:528
	ka(:,:,k) = extract(ft(a(:,:,k).*gaussf(rr([512 494],'freq')<.45,8)),[82 82],[220 63]);
end
	

% inverse fourier transform
a2 = dip_fouriertransform(ka,'inverse',[1 1 0]);
a2 = reshape(a2,[82 82 23 23]);
save '/home/martin/dat/20140215_2' a2

%%
% now open the cmos data
a = readtimeseries('/dev/shm/20140215/1/20140215_1.tif');

ka = newim([108 108 530],'complex');
for k=0:529
	ka(:,:,k) = extract(ft(a(:,:,k).*gaussf(rr([512 512],'freq')<.45,8)),[108 108],[382 149]);
end

% the lower left contains a bit of the zero order
a1 = dip_fouriertransform(ka.*gaussf(rr(ka(:,:,1),'freq')<.5,4),'inverse',[1 1 0]);
% a1 = reshape(a2,[108 108 23 23]); %why ist there one image too much?

save '/home/martin/dat/20140215_1' a1

load '/home/martin/dat/20140215_2' a2
load '/home/martin/dat/20140215_1' a1

writeim(abs(a1),'/dev/shm/1.tif');
writeim(abs(reshape(a2,[82 82 23*23])),'/dev/shm/2.tif');


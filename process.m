%% convert pgm 16-bit data to tif with imagej
% the following was acquired with the sony icx ccd sensor
a = readtimeseries('/dev/shm/20140215/20140215_2.tif');
a=a.*gaussf(rr([512 494],'freq')<.45,8);
ka = newim(a,'complex');
for k=0:size(a,3)-1
	ka(:,:,k) = ft(a(:,:,k))
end
	
ka = dip_fouriertransform(a,'forward',[1 1 0]);
clear a

% cut out the first order
ka = extract(ka,[82 82],[220 63]);

% inverse fourier transform
a2 = dip_fouriertransform(ka,'inverse',[1 1 0]);
a2 = reshape(a1,[82 82 23 23]);
save '/home/martin/dat/20140215_2' a2

%%
% now open the cmos data
a = readtimeseries('/dev/shm/20140215/1/20140215_1.tif');
% DampEdge doesnt run on my data, and is not necessary
ka = dip_fouriertransform(a,'forward',[1 1 0]);
ka=extract(ka,[108 108],[382 149]);
% the lower left contains a bit of the zero order
a1 = dip_fouriertransform(ka.*gaussf(rr(ka(:,:,1),'freq')<.5,4),'inverse',[1 1 0]);
a1 = reshape(a2,[108 108 23 23]);

save '/home/martin/dat/20140215_1' a1

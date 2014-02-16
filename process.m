%% convert pgm 16-bit data to tif with imagej
tic
a = DampEdge(readtimeseries('/dev/shm/20140215/20140215_2.tif'),.08,2);
ka = dip_fouriertransform(a,'forward',[1 1 0]);
toc % 145 s on one core

% cut out the first order
ka = extract(ka,[82 82],[220 63]);

% inverse fourier transform
a1 =   dip_fouriertransform(ka,'forward',[1 1 0]);

reshape(a1,[82 82 23 23])

%%
tic
a = DampEdge(readtimeseries('/dev/shm/20140215/20140215_1.tif'),.08,2);
ka = dip_fouriertransform(a,'forward',[1 1 0]);
toc % 145 s on one core

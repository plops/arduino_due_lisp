% -*- mode: Octave;-*-

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


%% check how holograms fluctuates, when nothing happens
% mkdir /dev/shm/tif;for i in /media/sda2/stabil-p/20140216/1/1_*;do convert $i /dev/shm/tif/`basename $i .pgm`.tif;done
a=readtimeseries('/dev/shm/tif/1_000.tif');
ka=extract(dip_fouriertransform(a,'forward',[1 1 0]),[128 128],[220 60]);
% demodulating the first order gives us the complex valued field
ac=dip_fouriertransform(ka,'inverse',[1 1 0]);


mask = gaussf(mean(abs(ac),[],3))>600;
mask(33,63,0)=0; % something weird going on with this pixel

phase(gaussf(real(ac))+i*gaussf(imag(ac))).*mask

% calculate phase change relative to first image
dac = newim(ac,'complex');
for k=1:size(ac,3)-1;
  dac(:,:,k)=-imag((ac(:,:,0)-ac(:,:,k-1))/ac(:,:,0));
end % perhaps this isn't the best approach because of intensity fluctuations
gaussf(real(dac).*mask)

% average the phase difference to the first image in the central area
% of each image
avgphase=newim(size(ac,3));
for k=0:size(avgphase)-1
  avgphase(k)=mean(real(dac(:,:,k)).*mask);
end
avgphase

% subtract the estimated phase fluctuation from measurement
ac_corrected = newim(ac,'complex');
for k=0:size(ac,3)-1;
  ac_corrected(:,:,k)=ac(:,:,k).*exp(-2*pi*i*avgphase(k));
end
phase(gaussf(real(ac_corrected))+i*gaussf(imag(ac_corrected))).*mask

% average the phase corrected holograms
ac_avg = mean(real(ac_corrected),[],3) + i*mean(imag(ac_corrected),[],3);



% mkdir /dev/shm/tif;for i in /media/sda2/stabil-p/20140216/2/2_*;do convert $i /dev/shm/tif/`basename $i .pgm`.tif;done
%mkdir /dev/shm/tif;for i in /media/sda2/stabil-p/20140216/3/3_*;do convert $i /dev/shm/tif/`basename $i .pgm`.tif;done
% ls 3*|while read -r i; do mv $i `echo $i | cut -d "_" -f 2`.tif;done
a=readtimeseries('/dev/shm/tif/3/000.tif');
ka=extract(dip_fouriertransform(a,'forward',[1 1 0]),[128 128],[220 60]);
% demodulating the first order gives us the complex valued field
ac=dip_fouriertransform(ka,'inverse',[1 1 0]);


mask = gaussf(mean(abs(ac),[],3))>1200;
mask(33,63,0)=0; % something weird going on with this pixel

phase(gaussf(real(ac),2)+i*gaussf(imag(ac),2)).*mask

ac_f=((gaussf(real(ac),2))+i*(gaussf(imag(ac),2)));
dac_f=(dz(gaussf(real(ac),2))+i*dz(gaussf(imag(ac),2)));
dac_f=gaussf(imag(dac_f./ac_f(:,:,0)))


dac_n=(dz((real(ac)))+i*dz(imag(ac)));
dac_n=-imag(dac_n./ac(:,:,0));
dac_n=dip_cumulativesum(dac_n,[],[0 0 1])
log(abs(dac_n))

ddac_n=-imag((ac(:,:,:)-ac(:,:,0))/ac(:,:,0))
gaussf(ddac_n).*mask
log(abs(ddac_n)).*mask

dip_image(unwrap(double(phase(squeeze(ac(:,:,0)))),[],2))

dip_cumulativesum(-imag((dx(real(ac))+i*dx(imag(ac)))./ac),[],[1 0 0])

phase(ac(:,:,:))-phase(ac(:,:,0))

% calculate phase change relative to first image
dac = newim(ac,'complex');
for k=1:size(ac,3)-1;
  dac(:,:,k)=-imag((ac(:,:,0)-ac(:,:,k-1))/ac(:,:,0));
end % perhaps this isn't the best approach because of intensity fluctuations
gaussf(real(dac).*mask)

% average the phase difference to the first image in the central area
% of each image
avgphase=newim(size(ac,3));
for k=0:size(avgphase)-1
  avgphase(k)=mean(real(dac_n(:,:,k)).*mask);
end
avgphase

% subtract the estimated phase fluctuation from measurement
ac_corrected = newim(ac,'complex');
for k=0:size(ac,3)-1;
  ac_corrected(:,:,k)=ac(:,:,k).*exp(2*pi*i*avgphase(k));
end
phase(gaussf(real(ac_corrected))+i*gaussf(imag(ac_corrected))).*mask

% average the phase corrected holograms
ac_avg = mean(real(ac_corrected),[],3) + i*mean(imag(ac_corrected),[],3);

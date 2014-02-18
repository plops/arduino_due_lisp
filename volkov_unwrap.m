%% -*- mode: Octave;-*-

function ph2 = volkov_unwrap(z)
  %% volkov 2003 phase unwrapping in the presence of noise
  clear i;
  zx =real((dx(real(z))+i*dx(imag(z)))./(i*z));
  zy =real((dy(real(z))+i*dy(imag(z)))./(i*z));
  
  %% this is equation 3 from the paper
  ka=(ft(zx).*xx(zx,'freq') + ft(zy).*yy(zy,'freq'))./rr(zx,'freq').^2;
  ka(floor(size(ka,1)/2),floor(size(ka,2)/2))=0;
% first estimation of unwrapped phase (according to paper around 2% error)
  ph1=real(ift(ka)/(2*pi*i));
  
  kx=(zx-dx(ph1))/(2*pi);
  ky=(zy-dy(ph1))/(2*pi);
  ka=(ft(kx).*xx(kx,'freq') + ft(ky).*yy(ky,'freq'))./rr(kx,'freq').^2;
  ka(floor(size(ka,1)/2),floor(size(ka,2)/2))=0;
  %% reconstructed integer field
  k_recon=round(real(ift(ka)/(2*pi*i)));
  
  %% using the integer field, produce a better correction of the unwrapped phase
  ph2=ph1-2*pi*k_recon;
end

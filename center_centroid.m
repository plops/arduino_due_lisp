% -*- mode: Octave;-*-

function [a1c cent1] = center_centroid(a1)
  int1=mean(abs(a1),[],3);
  cent1=[sum(xx(int1).*int1)/sum(int1) sum(yy(int1).*int1)/sum(int1)];
  clear i;
  a1c=dip_fouriertransform(dip_fouriertransform(a1,'forward',[1 1 0]).*exp(2*pi*i*(xx(int1,'freq')*cent1(1)+yy(int1,'freq')*cent1(2))),'inverse',[1 1 0]);
end

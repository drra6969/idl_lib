  pro scross, center,dir1,dir2,s1,s2,no,eps, xi,yi,zi

;  subroutine to determine no equidistant start locations on a 
;  cross centered at center for 2 no field lines. 
;  A second set of start locations can be selected with 
;  a seperation of eps from the first set if eps <> 0.
  print, no,eps
  xi1=findgen(no)+1.0 & yi1=xi1 & zi1=xi1
  xi2=findgen(no)+1.0 & yi2=xi2 & zi2=xi2
  xstart=center(0) & ystart=center(1) & zstart=center(2) 
  abs1=sqrt(dir1(0)*dir1(0)+dir1(1)*dir1(1)+dir1(2)*dir1(2))
  abs2=sqrt(dir2(0)*dir2(0)+dir2(1)*dir2(1)+dir2(2)*dir2(2))
  dxi1=s1*dir1(0)/abs1/float(no-1)  
  dyi1=s1*dir1(1)/abs1/float(no-1)  
  dzi1=s1*dir1(2)/abs1/float(no-1)  
  dxi2=s2*dir2(0)/abs2/float(no-1)
  dyi2=s2*dir2(1)/abs2/float(no-1)
  dzi2=s2*dir2(2)/abs2/float(no-1)
  xi10=-xi1*dxi1+xstart 
  yi10=-yi1*dyi1+ystart
  zi10=-zi1*dzi1+zstart 
  xi11=xi1*dxi1+xstart 
  yi11=yi1*dyi1+ystart
  zi11=zi1*dzi1+zstart 
  xi20=-xi2*dxi2+xstart 
  yi20=-yi2*dyi2+ystart
  zi20=-zi2*dzi2+zstart 
  xi21=xi2*dxi2+xstart 
  yi21=yi2*dyi2+ystart
  zi21=zi2*dzi2+zstart 
  xi=[xstart,xi10,xi11,xi20,xi21]
  xi=[xstart,xi10,xi11,xi20,xi21]
  xi=[xstart,xi10,xi11,xi20,xi21]
  
  dxin=(s2(0)-s1(0))/float(no-1) 
  dyin=(s2(1)-s1(1))/float(no-1) 
  dzin=(s2(2)-s1(2))/float(no-1) 
  xin1=xin1*dxin+xstart  
  yin1=yin1*dyin+ystart
  zin1=zin1*dzin+zstart 
  print,xin1
  xi=xin1
  yi=yin1
  zi=zin1
  if eps ne 0.0 then begin
   xin2=xin1+eps*dxin 
   yin2=yin1+eps*dyin
   zin2=zin1+eps*dzin
   xi=[xin1,xin2]
   yi=[yin1,yin2]
   zi=[zin1,zin2]
   s=size(xi)
   print,s
  endif
  s=size(xi)
  print,s
  print,xi

end

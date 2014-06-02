  pro sline, s1,s2,no,eps, xi,yi,zi

;  subroutine to determine no equidistant start locations between
;  s1 and s2. A second set of start location can be selected with 
;  a seperation of eps from the first set if eps <> 0.
  print, no,eps
  xin1=findgen(no) & yin1=xin1 & zin1=xin1
  xstart=s1(0) & ystart=s1(1) & zstart=s1(2) 
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

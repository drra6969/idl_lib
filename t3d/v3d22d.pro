PRO V3D22D, whatplane, coor, xv,yv,zv, vxout,vyout,xout,yout

  s1=size(xv) & s2=size(yv) & s3=size(zv)
  pi=3.14159265 & kfl=pi/10.0
  dimx=s1(1) & dimy=s2(1) & dimz=s3(1) 
  if whatplane eq 'x' then begin
    vycut=fltarr(dimy,dimz) & vzcut=vycut
    for iy=0,dimy-1 do  for iz=0,dimz-1 do begin
        rxy=coor**2+yv(iy)**2+1.44
        rxz=coor**2+zv(iz)**2         
         vycut(iy,iz) =  coor/rxy
         vzcut(iy,iz) =  0.1*coor/cosh(rxz/16.)
;         vycut(iy,iz) = -cos(kfl*coor)*sin(kfl*yv(iy))
;         vzcut(iy,iz) =  0   
    endfor
    vxout=vycut & vyout=vzcut 
    xout=yv & yout=zv
  endif
  if whatplane eq 'y' then begin
    vxcut=fltarr(dimx,dimz) & vzcut=vxcut
    for ix=0,dimx-1 do  for iz=0,dimz-1 do begin
        rxy=xv(ix)**2+coor**2+1.44
        rxz=xv(ix)**2+zv(iz)**2         
         vxcut(ix,iz) = -coor/rxy-0.1*zv(iz)/cosh(rxz/16.)
         vzcut(ix,iz) =  0.1*xv(ix)/cosh(rxz/16.)
;         vxcut(ix,iz) = sin(kfl*xv(ix))*cos(kfl*coor) 
;         vzcut(ix,iz) = 0   
    endfor
    vxout=rotate(vzcut,4) & vyout=rotate(vxcut,4) 
    xout=zv & yout=xv
  endif
  if whatplane eq 'z' then begin
    vxcut=fltarr(dimx,dimy) & vycut=vxcut
    for ix=0,dimx-1 do  for iy=0,dimy-1 do begin
        rxy=xv(ix)**2+yv(iy)**2+1.44
        rxz=xv(ix)**2+coor**2         
         vxcut(ix,iy) = -yv(iy)/rxy-0.1*coor/cosh(rxz/16.)
         vycut(ix,iy) =  xv(ix)/rxy
;         vxcut(ix,iy) =  sin(kfl*xv(ix))*cos(kfl*yv(iy)) 
;         vycut(ix,iy) = -cos(kfl*xv(ix))*sin(kfl*yv(iy))   
    endfor
    vxout=rotate(vycut,4) & vyout=rotate(vxcut,4) 
    xout=yv & yout=xv
  endif

end



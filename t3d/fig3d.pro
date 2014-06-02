;----------------------------------------------------------
; START OF MAIN PROGRAM
  COMMON field, f, xmin,xmax,ymin,ymax,zmin,zmax
         
  nx=long(101) & ny=long(101) & nz=long(101) 
  xmin = -10.0 & ymin = -10.0 & zmin = -10.0
  xmax =  10.0 & ymax =  10.0 & zmax =  10.0
  x=findgen(nx) & y=findgen(nx) & z=findgen(nx) 
  dx=(xmax-xmin)/float(nx-1) &  x=x*dx+xmin
  dy=(ymax-ymin)/float(nx-1) &  y=y*dy+ymin
  dz=(zmax-zmin)/float(nx-1) &  z=z*dz+zmin 
  print, 'xmin=', x(0), '  xmax=', x(nx-1)
  print, 'ymin=', y(0), '  ymax=', y(ny-1)
  print, 'zmin=', z(0), '  zmax=', z(nz-1)
  f=fltarr(nx,ny,nz,/NOZERO) 
  rxz=fltarr(ny,nz,/NOZERO) 
  ac=fltarr(nx,/NOZERO) 
  ac(*)=0.0
;  ac(*)=-0.5
;  ind=where(x lt -5.0)
;  ac(ind)=1.0
  
  for iy=0,ny-1 do rxz(iy,*)=y(iy)^2.0+z(*)^2.0
  for ix=0,nx-1 do $
    f(ix,*,*)  = 0.5*( 1.0 $
                       + tanh(-x(ix)+ac(ix)-0.005*rxz(*,*)^1.7) ) $
                -0.7/cosh(((x(ix)+5.0))/1.2)^2.0 

      
  PLOT3DDD
    
end



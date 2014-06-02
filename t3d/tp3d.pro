;----------------------------------------------------------
; START OF MAIN PROGRAM
  COMMON field, a, xmin,xmax,ymin,ymax,zmin,zmax
         
  nx=long(131) & ny=long(126) & nz=long(126) 
  time=0.0 & fnumber=1 & next='y'
  nxf=151 & nyf=151 & nzf=151     ;grid for uniform interpol fields
  ioxf=fltarr(nxf) & ioyf=fltarr(nxf) & iozf=fltarr(nxf)
  
  xmin = -10.0 & ymin = -10.0 & zmin = -10.0
  xmax =  10.0 & ymax =  10.0 & zmax =  10.0

  print, 'Input filenumber'
  read, fnumber
  name='magtap'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNforMATTED
  readu, 8, nx, ny, nz, time
   print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
   print, 'time=',time
  
  rho=fltarr(nx,ny,nz,/NOZERO) 
  x=fltarr(nx,/NOZERO) & dx=0.1 & dxh=0.1
  y=fltarr(ny,/NOZERO) & dy=0.1 & dyh=0.1
  z=fltarr(nz,/NOZERO) & dz=0.1 & dzh=0.1 
  
  readu, 8,  x,dx,dxh,dxh,y,dy,dyh,dyh,z,dz,dzh,dzh
; print, x,y,z
  readu, 8, rho
  print, 'xmin=', x(1), '  xmax=', x(nx-2)
  print, 'ymin=', y(1), '  ymax=', y(ny-2)
  print, 'zmin=', z(1), '  zmax=', z(nz-2)
  close, 8

; generation of grid for uniform interpol fields
  xf=findgen(nxf) & yf=findgen(nxf) & zf=findgen(nxf) 
  dxf=(xmax-xmin)/float(nxf-1) &  xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nxf-1) &  yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nxf-1) &  zf=zf*dzf+zmin 
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k)) 
  endrep until in eq nxf-1
  
;  a=interpolate(rho,ioxf,ioyf,iozf,/grid)
  a=rho
    
  PLOT3D
    
end



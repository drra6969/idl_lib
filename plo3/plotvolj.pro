

  COMMON field, fslice, xmin,xmax,ymin,ymax,zmin,zmax
  nx=long(83) & ny=long(83) & nz=long(123) & time=0.0
  nxf=101 & nyf=101 & nzf=101     ;grid for uniform interpol fields
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  fslice=fltarr(nxf,nyf,nzf,/NOZERO)

;-----------PARAMETER----------------
  xmin =-10. & ymin = -45.   & zmin =  0.
  xmax = 10. & ymax = 45.   & zmax = 100.

  fnumber=1 & isclice=1
  fgroup='1' & group='1'
  name='' & jnew='y' & enew='y' & again='y'

  print, 'Input filenumber'
  read, fnumber
  name='mjout'+string(fnumber,'(i3.3)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time 
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    print, 'time=',time
    x=fltarr(nx,/NOZERO) 
    y=fltarr(ny,/NOZERO) 
    z=fltarr(nz,/NOZERO) 
    fj=fltarr(nx,ny,nz,/NOZERO) 
  readu, 8,  x,y,z
    print, 'xmin=', x(0), '  xmax=', x(nx-1)
    print, 'ymin=', y(0), '  ymax=', y(ny-1)
    print, 'zmin=', z(0), '  zmax=', z(nz-1)
    print,x
    print,y
    print,z
    if xmin lt x(0) then xmin = x(0)
    if xmax gt x(nx-1) then xmax = x(nx-1)
    if ymin lt y(0) then ymin = y(0)
    if ymax gt y(ny-1) then ymax = y(ny-1)
    if zmin lt z(0) then zmin = z(0)
    if zmax gt z(nz-1) then zmax = z(nz-1)
  readu, 8,  fj
  close, 8

  amin=min(fj) & amax=max(fj) 
; generation of grid for uniform interpol fields
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf) 
  dxf=(xmax-xmin)/float(nxf-1) &  xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nyf-1) &  yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nzf-1) &  zf=zf*dzf+zmin 
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xf(in) gt x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yf(in) gt y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zf(in) gt z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzf-1


  fslice=interpolate(fj,ioxf,ioyf,iozf,/grid)

  PLOT3D
  
; Alternative Subroutines are 
;       SLICER (idl demo) or 
;       SLOWN (includes postscript) or 
;       PLOT3D (includes postscript and animation) which require 
;           requires to include the boundaries in 
;    COMMON VOLUME_DATA, fslice, xmin,xmax,ymin,ymax,zmin,zmax
  

end



; START OF MAIN PROGRAM

  COMMON field, fslice, xmin,xmax,ymin,ymax,zmin,zmax
  nx=long(83) & ny=long(83) & nz=long(123) & time=0.0
  nxf=101 & nyf=101 & nzf=101     ;grid for uniform interpol fields
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  fslice=fltarr(nxf,nyf,nzf,/NOZERO)

;-----------PARAMETER----------------
  xmin =-8 & ymin = -50   & zmin =  0
  xmax = 8 & ymax = 50   & zmax = 100

  fnumber=1 & isclice=1
  fgroup='1' & group='1'
  name='' & jnew='y' & enew='y' & again='y'

  print, 'Input filenumber'
  read, fnumber
;  name='magtap0'+string(fnumber,'(i1)')
  name='magbin0'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time 
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    print, 'time=',time
    x=fltarr(nx,/NOZERO) & dx=x & dxh=x
    y=fltarr(ny,/NOZERO) & dy=y & dyh=y
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx 
    ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
  readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
    if xmin lt x(1) then xmin = x(1)
    if xmax gt x(nx-2) then xmax = x(nx-2)
    if ymin lt y(1) then ymin = y(1)
    if ymax gt y(ny-2) then ymax = y(ny-2)
    if zmin lt z(1) then zmin = z(1)
    if zmax gt z(nz-2) then zmax = z(nz-2)
  readu, 8,  bx,by,bz
  readu, 8,  sx,sy,sz
  readu, 8,  rho,u,res
  close, 8

  amin=min(rho) & amax=max(rho) &  thresh=amin+0.5*(amax-amin)
  print,'rho:',amin,amax,thresh
  amin=min(bx) & amax=max(bx) &  thresh=amin+0.5*(amax-amin)
  print,'bx:',amin,amax,thresh
  amin=min(by) & amax=max(by) &  thresh=amin+0.5*(amax-amin)
  print,'by:',amin,amax,thresh
  amin=min(bz) & amax=max(bz) &  thresh=amin+0.5*(amax-amin)
  print,'bz:',amin,amax,thresh
  amin=min(sx) & amax=max(sx) &  thresh=amin+0.5*(amax-amin)
  print,'sx:',amin,amax,thresh
  amin=min(sy) & amax=max(sy) &  thresh=amin+0.5*(amax-amin)
  print,'sy:',amin,amax,thresh
  amin=min(sz) & amax=max(sz) &  thresh=amin+0.5*(amax-amin)
  print,'sz:',amin,amax,thresh
  amin=min(res) & amax=max(res) &  thresh=amin+0.5*(amax-amin)
  print,'res:',amin,amax,thresh

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

  f1=rho & p=2*u^(5.0/3.0) & f2=p & f4=p & f3=p+bx^2+by^2+bz^2
  head1='Density' & head2='Pressure' & head3='Resistivity'
field:
  print, 'Input Fieldgroup:'
  print, 'Options: 1 - rho, p, res'
  print, '         2 - velocity, v'
  print, '         3 - magnetic field b'
  print, '         4 - current density'
  print, '         5 - electric field'
  print, '         6 - Jpar, ptot, Epar'
  print, '         7 - abs bx, abs v, b**2'
  print, '         q - terminate'
  print, '    return - no changes applied'
  print, 'Present Choice: ', fgroup
  read, group & if group ne '' then fgroup=group
  if group eq '' then print, 'present choice=',group,' not altered'
  if fgroup eq 'q' then stop
  if fgroup eq '1' then begin
    head1='Density' & head2='Pressure' & head3='Total Pressure'
    f1=rho & f2=2*u^(5.0/3.0) & f3=res & endif
  if fgroup eq '2' then begin
    head1='Vel. Vx' & head2='Vel. Vy' & head3='Vel. Vz'
    f1=sx/rho & f2=sy/rho & f3=sz/rho & endif 
  if fgroup eq '3' then begin
    head1='Magn. Field Bx' & head2='Magn. Field By'
    head3='Magn. Field Bz'
    f1=bx & f2=by & f3=bz & f4=bz & endif 
  if (fgroup eq '4') or (fgroup eq '5') or (fgroup eq '6') then begin $
    head1='Curr. Dens. Jx' & head2='Curr. Dens. Jy'
    head3='Curr. Dens. Jz'
    if jnew eq 'y' then begin
      f1 = shift(bz,0,-1,0)-shift(bz,0,1,0)
      for j=1,ny-2 do f3(*,j,*)=dy(j)*f1(*,j,*)
      f2 = shift(by,0,0,-1)-shift(by,0,0,1)
      for k=1,nz-2 do f4(*,*,k)=dz(k)*f2(*,*,k)
      jx = f3-f4
      f1 = shift(bx,0,0,-1)-shift(bx,0,0,1)
      for k=1,nz-2 do f3(*,*,k)=dz(k)*f1(*,*,k)
      f2 = shift(bz,-1,0,0)-shift(bz,1,0,0)
      for i=1,nx-2 do f4(i,*,*)=dx(i)*f2(i,*,*)
      jy = f3-f4
      f1 = shift(by,-1,0,0)-shift(by,1,0,0)
      for i=1,nx-2 do f3(i,*,*)=dx(i)*f1(i,*,*)
      f2 = shift(bx,0,-1,0)-shift(bx,0,1,0)
      for j=1,ny-2 do f4(*,j,*)=dy(j)*f2(*,j,*)
      jz = f3-f4
      jx([0,nx-1],*,*)=0.0 & jy([0,nx-1],*,*)=0.0 & jz([0,nx-1],*,*)=0.0
      jx(*,[0,ny-1],*)=0.0 & jy(*,[0,ny-1],*)=0.0 & jz(*,[0,ny-1],*)=0.0 
      jx(*,*,[0,nz-1])=0.0 & jy(*,*,[0,nz-1])=0.0 & jz(*,*,[0,nz-1])=0.0 
      jnew='n'
    endif
    f1=jx & f2=jy & f3=jz 
  endif
  if fgroup eq '5' then begin
    head1='Electr. Field Ex' & head2='Electr. Field Ey'
    head3='Electr. Field Ez'
    if enew eq 'y' then begin
      ex=(sy*bz-sz*by)/rho+res*jx
      ey=(sz*bx-sx*bz)/rho+res*jy
      ez=(sx*by-sy*bx)/rho+res*jz 
      enew='n'
    endif
    f1=ex & f2=ey & f3=ez 
  endif
  if fgroup eq '6' then begin
    head1='J parallel' & head2='Ptot' 
    head3='E parallel' 
        f2=sqrt(bx*bx+by*by+bz*bz)
        f1=(bx*jx+by*jy+bz*jz)/f2
        f3=res*f1
        f2=2*u^(5.0/3.0)+bx*bx+by*by+bz*bz
  endif
  if fgroup eq '7' then begin
    head1='ABS BX' & head2='ABS V' 
    head3='B**2' 
        f1=abs(bx)
        f2=sqrt(sx*sx+sy*sy+sz*sz)/rho
        f3=bx*bx+by*by+bz*bz
  endif

  islice=1
  print, 'which field for slicer (1, 2, or 3, or (4))?'
  read, islice
  if islice eq 1 then fslice=interpolate(f1,ioxf,ioyf,iozf,/grid)
  if islice eq 2 then fslice=interpolate(f2,ioxf,ioyf,iozf,/grid)
  if islice eq 3 then fslice=interpolate(f3,ioxf,ioyf,iozf,/grid)
  if (islice eq 4) and (fgroup eq 6)  then $
      fslice=interpolate(f4,ioxf,ioyf,iozf,/grid)

  openw, 8, 'ttt',/F77_UNFORMATTED
  writeu, 8, nxf, nyf, nzf, time 
    print, 'dimension nx=',nxf,'   ny=',nyf,'   nz=',nzf
    print, 'time=',time
  writeu, 8,  xf,yf,zf
  writeu, 8,  fslice
  close, 8

  PLOT3D
  
; Alternative Subroutines are 
;       SLICER (idl demo) or 
;       SLOWN (includes postscript) or 
;       PLOT3D (includes postscript and animation) which require 
;           requires to include the boundaries in 
;    COMMON VOLUME_DATA, fslice, xmin,xmax,ymin,ymax,zmin,zmax
  
  print, 'back to field choice?'
  read, again
  if again eq 'y' then goto, field


end



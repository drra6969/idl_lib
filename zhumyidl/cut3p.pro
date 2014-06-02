; START OF MAIN PROGRAM
; program needs update for vect, and laser 600 resolution
  nxn=21 & nyn=25 & nzn=21     ;grid for arrow presentation
  nxf=41 & nyf=41 & nzf=21     ;grid for uniform interpol fields
  nx=131l & ny=126l & nz=126l & nzz=126l 
;  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
;  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  time=0.0 & fnumber=1
  fgroup='1' & group='1'
  name='' & contin='' & again='y' & withps='n' & run=''  
  closeps='' & jnew='y' & enew='y' 
  plane='y' & whatcut='x' & newgrid='f' & igrid=2
  withunits='s' & wun='s'

  print, 'Input filenumber'
  read, fnumber
  name='pagtap'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time, nzz
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz,'   nzz=',nzz
    print, 'time=',time
    nv=nx*ny  & xvec=fltarr(nv,/NOZERO) & dxvec=xvec & yvec=xvec & dyvec=xvec
    x=fltarr(nx,/NOZERO) & dx=x & dxh=xvec
    y=fltarr(ny,/NOZERO) & dy=y & dyh=yvec
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z & u1=z & u2=z & u3=z
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx & prof=bx 
    ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
  readu, 8,  xvec,dxvec,dxh,dxh,dxh,dxh,dxh,yvec,dyvec,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
  readu, 8,  bx,by,bz
  readu, 8,  sx,sy,sz
  readu, 8,  rho,u,res,prof
    ischritt=1l & iaus=1l & isafe=1l & iend=1l 
    dt=0.1 & zeit=0.1 & gamma=0.1 & eta=0.1 & rho0=0.1
  readu, 8, ischritt,iaus,isafe,iend,dt,gamma,eta,rho0    
  close, 8
;-----------PARAMETER----------------
  xmin =-2.0 & ymin = 0.    & zmin = 0.
  xmax = 2. & ymax = 15.   & zmax = 15.0

  zsmin=z(0) & zsmax=z(nz-1)
  testbd3, nx,ny,nz,x,y,z,xmin,xmax,ymin,ymax,zmin,zmax

  amin=min(rho) & amax=max(rho) &  thresh=amin+0.5*(amax-amin)
  print,'rho:',amin,amax,thresh
  amin=min(u) & amax=max(u) &  thresh=amin+0.5*(amax-amin)
  print,'u:',amin,amax,thresh
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

;--- physical normalization---
  norm, nz,z,zsmin,zsmax,rhop,temp,pp1,vash1,db1,ec1,epar1,j1,resfac1,dcs1
  print,'pp1:'
  print, pp1
           
;--- generation of new grid for velocity vectors---
  newgrid, x,y,z,xmin,xmax,ymin,ymax,zmin,zmax, $
           nxn,nyn,nzn,xn,yn,zn,iox,ioy,ioz,dxn,dyn,dzn

;--- generation of grid for uniform interpol fields---
  newgrid, x,y,z,xmin,xmax,ymin,ymax,zmin,zmax, $
           nxf,nyf,nzf,xf,yf,zf,ioxf,ioyf,iozf,dxf,dyf,dzf

  bsq=bx*bx+by*by+bz*bz
  f1=rho & p=2*u^(5.0/3.0) & f2=p & f4=p & f3=p+bsq
  head1='Density' & head2='Pressure' & head3='Temp'
    u1=rhop & u2=pp1 & u3=temp 
    had1='  (cm^(-3))' & had2='  (nPa)' & had3='  (K)' 
field:
  print, 'Input Fieldgroup:'
  print, 'Options: 1 - rho, p, temp'
  print, '         2 - velocity, v'
  print, '         3 - magnetic field b'
  print, '         4 - current density'
  print, '         5 - electric field'
  print, '         6 - jpar, jperp, Epar'
  print, '         7 - vpar,vperp,res'
  print, '         8 - p,bsqr,p_tot'
  print, '         9 - v_perp components'
  print, '        10 - v_par components'
  print, '        11 - ExB components'
  print, '        12 - J X B'
  print, '        13 - grad p'
  print, '        14 - sum of  12 and 13'
  print, '         q - terminate'
  print, '    return - no changes applied'
  print, 'Present Choice: ', fgroup
  read, group & if group ne '' then fgroup=group
  if (strno(fgroup) ne -1) then fgrno=strno(fgroup)
  if group eq '' then print, 'present choice=',group,' not altered'
  if fgroup eq 'q' then stop
  if fgrno eq 1 then begin
    head1='Log (Rho)' & head2='log (P)' & head3='Temp' & head4=' '
    f1=rho & f2=p & f3=f2/f1 
    u1=rhop & u2=pp1 & u3=temp 
    had1='  (cm^(-3))' & had2='  (nPa)' & had3='  (K)' & endif
  if fgrno eq 2 then begin
    head1='Vel. Vx' & head2='Vel. Vy' & head3='Vel. Vz'& head4='Velocity V'
    f1=sx/rho & f2=sy/rho & f3=sz/rho 
    u1=vash1 & u2=vash1 & u3=vash1 
    had1='  (km/s)' & had2='  (km/s)' & had3='  (km/s)' & endif
  if fgrno eq 3 then begin
    head1='Magn. Field Bx' & head2='Magn. Field By'
    head3='Magn. Field Bz' & head4='Magn. Field B'
    f1=bx & f2=by & f3=bz 
    u1=db1 & u2=db1 & u3=db1 
    had1='  (nT)' & had2='  (nT)' & had3='  (nT)' & endif
  if ( (fgrno eq 4) or ((fgrno gt 4) and (jnew eq 'y')) ) then begin $
    head1='Curr. Dens. Jx' & head2='Curr. Dens. Jy'
    head3='Curr. Dens. Jz' & head4='Curr. Dens. J'
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
    f1=jx & f2=jy & f3=jz & u1=j1 & u2=j1 & u3=j1 
    had1='  (micro A/m^2)' & had2='  (micro A/m^2)' & had3='  (micro A/m^2)'
  endif
  if ( (fgrno eq 5)  or ((fgrno gt 5) and (enew eq 'y')) ) then begin
    head1='Electr. Field Ex' & head2='Electr. Field Ey'
    head3='Electr. Field Ez' & head4='Electr. Field E'
    if enew eq 'y' then begin
      ex=-(sy*bz-sz*by)/rho+res*jx
      ey=-(sz*bx-sx*bz)/rho+res*jy
      ez=-(sx*by-sy*bx)/rho+res*jz
      enew='n'
    endif
    f1=ex & f2=ey & f3=ez & u1=ec1/4. & u2=ec1/4. & u3=ec1/4. 
    had1='  (V/m)' & had2='  (V/m)' & had3='  (V/m)'
  endif
  if fgrno eq 6 then begin
    head1='J parallel' & head2='J perp' 
    head3='E parallel' 
       f1 = (jx*bx+jy*by+jz*bz)/sqrt(bsq)
       f2 = sqrt( (jx*jx+jy*jy+jz*jz)-f1*f1 )
       f3 = res*f1
       u1=j1 & u2=j1 & u3=epar1 
    had1='  (micro A/m^2)' & had2='  (micro A/m^2)' & had3='  (mV/m)'
  endif
  if fgrno eq 7 then begin
    head1='V parallel' & head2='V perp' 
    head3='Resistivity' 
       f1 = (sx*bx+sy*by+sz*bz)/sqrt(bsq)
       f2 = sqrt( (sx*sx+sy*sy+sz*sz)-f1*f1 )/rho
       f1 = f1/rho
       f3 = res
       u1=vash1 & u2=vash1 & u3=resfac1 
    had1='  (km/s)' & had2='  (km)' & had3='  (omh*m)'
  endif
  if fgrno eq 8 then begin
    head1='p' & head2='B**2' 
    head3='Ptot' 
       f1=p & f2=bsq & f3=bsq+p
       u1=pp1 & u2=pp1 & u3=pp1 
    had1='  (nPa)' & had2='  (nPa)' & had3='  (nPa)'
  endif
  if fgrno eq 9 then begin
    head1='Vperp_x' & head2='Vperp_y' 
    head3='Vperp_z'  & head4='Vperp' 
       f3=(sx*bx+sy*by+sz*bz)/bsq
       f1=(sx-f3*bx)/rho
       f2=(sy-f3*by)/rho
       f3=(sz-f3*bz)/rho
       u1=vash1 & u2=vash1 & u3=vash1 
    had1='  (km/s)' & had1='  (km/s)' & had1='  (km/s)'
  endif
  if fgrno eq 10 then begin
    head1='Vpar_x' & head2='Vpar_y' 
    head3='Vpar_z' & head4='Vpar'
       f3=(sx*bx+sy*by+sz*bz)/bsq
       f1=f3*bx/rho
       f2=f3*by/rho
       f3=f3*bz/rho
       u1=vash1 & u2=vash1 & u3=vash1 
    had1='  (km/s)' & had1='  (km/s)' & had1='  (km/s)'
  endif
  if fgrno eq 11 then begin
    head1='(ExB)x' & head2='(ExB)y'
    head3='(ExB)z' & head4='(ExB)'
      f1=ey*bz-ez*by
      f2=ez*bx-ex*bz
      f3=ex*by-ey*bx
      u1(*)=1. & u2(*)=1. & u3(*)=1. 
    had1='' & had1='' & had1=''
  endif
  if fgrno eq 12 then begin
    head1='x comp JxB' & head2='y comp JxB'
    head3='z comp JxB' & head4='(JxB)'
      f1=jy*bz-jz*by
      f2=jz*bx-jx*bz
      f3=jx*by-jy*bx
      u1(*)=1. & u2(*)=1. & u3(*)=1. 
    had1='' & had1='' & had1=''
  endif
  if fgrno eq 13 then begin
    head1='-gradx P' & head2='-grady P'
    head3='-gradz P' & head4='-(grad P)'
      f1=shift(p,-1,0,0)-shift(p,1,0,0)
      f2=shift(p,0,-1,0)-shift(p,0,1,0)
      f3=shift(p,0,0,-1)-shift(p,0,0,1)
      for i=1,nx-2 do f1(i,*,*)=-dx(i)*f1(i,*,*)
      for j=1,ny-2 do f2(*,j,*)=-dy(j)*f2(*,j,*)
      for k=1,nz-2 do f3(*,*,k)=-dz(k)*f3(*,*,k)
      u1(*)=1. & u2(*)=1. & u3(*)=1. 
    had1='' & had1='' & had1=''
  endif
  if fgrno eq 14 then begin
    head1='force_x' & head2='force_y'
    head3='force_z' & head4='Force'
      f1=shift(p,-1,0,0)-shift(p,1,0,0)
      f2=shift(p,0,-1,0)-shift(p,0,1,0)
      f3=shift(p,0,0,-1)-shift(p,0,0,1)
      for i=1,nx-2 do f1(i,*,*)=-dx(i)*f1(i,*,*)
      for j=1,ny-2 do f2(*,j,*)=-dy(j)*f2(*,j,*)
      for k=1,nz-2 do f3(*,*,k)=-dz(k)*f3(*,*,k)
      f1=f1+jy*bz-jz*by
      f2=f2+jz*bx-jx*bz
      f3=f3+jx*by-jy*bx
      u1(*)=1. & u2(*)=1. & u3(*)=1. 
    had1='' & had1='' & had1=''
  endif

cut:
  print, 'Input - What Cut Through The 3-D System:'
  print, 'Options: x -> y,z-plane, x=const'
  print, '         y -> z,x-plane, y=const'
  print, '         z -> x,y-plane, z=const'
  print, '    return -> no changes applied'
  print, '         f -> back to fieldgroup'
  print, '         q -> terminate'
  print, 'Present Choice: ', plane
  read, whatcut
  if whatcut eq 'q' then stop
  if whatcut eq 'f' then goto, field
  if whatcut eq 'x' then plane='x'
  if whatcut eq 'y' then plane='y'
  if whatcut eq 'z' then plane='z'
  if whatcut eq '' then print,'choice=',plane,' not altered'
  if plane eq 'x' then begin 
    nplane=nx & coord=x & head5=head1 & head6=head4+'y,z Comp' & endif
  if plane eq 'y' then begin 
    nplane=ny & coord=y & head5=head2 & head6=head4+'z,x Comp' & endif
  if plane eq 'z' then begin 
    nplane=nz & coord=z & head5=head3 & head6=head4+'x,y Comp' & endif

gridindex:
  print, plane, 'Coordinates:'
  for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Options:   integer -> grid index'
  print, '            return -> no changes applied'
  print, '                 f -> back to fieldgroup'
  print, '                 c -> back to cut'
  print, '                 q -> terminate'
  print, 'Present Choice: ', igrid
  read, newgrid 
  if newgrid eq 'q' then stop
  if newgrid eq 'f' then goto, field
  if newgrid eq 'c' then goto, cut
  if newgrid ne '' then igrid=fix(newgrid)
  if newgrid eq '' then print,'choice=',igrid,' not altered'
  if (igrid lt 0) or (igrid gt nplane-1) then igrid=(nplane+1)/2  

  if plane eq 'x' then begin
    cutata='x=' & cutatb=string(x(igrid),'(f7.2)')
    xtit='y'    & ytit='z'    & xchoice=y   & ychoice=z
    xpmin=ymin  & xpmax=ymax  & ypmin=zmin  & ypmax=zmax 
    fc1=f1(igrid,*,*) & fc1=reform(fc1) 
    fc2=f2(igrid,*,*) & fc2=reform(fc2) 
    fc3=f3(igrid,*,*) & fc3=reform(fc3) 
    xplot=y  &  yplot=z
      fa=interpolate(fc1,ioyf,iozf,/grid)
      fb=interpolate(fc2,ioyf,iozf,/grid)
      fc=interpolate(fc3,ioyf,iozf,/grid)
      xsu=yf & ysu=zf 
  endif
  if plane eq 'y' then begin
    cutata='y=' & cutatb=string(y(igrid),'(f7.2)')
    xtit='x'    & ytit='z'    & xchoice=x   & ychoice=z
    xpmin=xmax  & xpmax=xmin  & ypmin=zmin  & ypmax=zmax 
    fc1=f1(*,igrid,*) & fc1=reform(fc1) 
    fc2=f2(*,igrid,*) & fc2=reform(fc2) 
    fc3=f3(*,igrid,*) & fc3=reform(fc3) 
    xplot=z  &  yplot=x
      fa=interpolate(fc1,ioxf,iozf,/grid)
      fb=interpolate(fc2,ioxf,iozf,/grid)
      fc=interpolate(fc3,ioxf,iozf,/grid)
      xsu=xf & ysu=zf 
  endif
  if plane eq 'z' then begin
    cutata='z=' & cutatb=string(z(igrid),'(f7.2)')
    xtit='x'    & ytit='y'    & xchoice=x   & ychoice=y
    xpmin=xmin  & xpmax=xmax  & ypmin=ymin  & ypmax=ymax 
    fc1=f1(*,*,igrid) & fc2=f2(*,*,igrid) & fc3=f3(*,*,igrid)  
    fc1=reform(fc1) & fc2=reform(fc2) & fc3=reform(fc3) 
    xplot=x  &  yplot=y
      fa=interpolate(fc1,ioxf,ioyf,/grid)
      fb=interpolate(fc2,ioxf,ioyf,/grid)
      fc=interpolate(fc3,ioxf,ioyf,/grid)
      xsu=xf & ysu=yf 
  endif

units:
  if withunits eq 'p' then print, 'Plots are in physical (MKSA) units.'
  if withunits ne 'p' then print, 'Plots are in simulation units.'
  print, 'Plots in physical or simulation units?'
  print, 'Input           return -> no change'
  print, '                     s -> in simulation units'
  print, '                     p -> in physical (mksa) units)'
  read, wun
  if wun eq 's' then withunits='s'
  if wun eq 'p' then withunits='p'
  if withunits ne 'p' then begin
    titl1=head1 & titl2=head2 & titl3=head3 & titl5=head5 & titl6=head6 & endif
  if withunits eq 'p' then begin
    fu1=interpolate(u1,iozf,/grid)
    fu2=interpolate(u2,iozf,/grid)
    fu3=interpolate(u3,iozf,/grid)
    titl1=head1+had1 & titl2=head2+had2 & titl3=head3+had3
    titl5=head5+had1 & titl6=head6+had1 
    if plane eq 'x' then begin
      for i=0,nyf-1 do begin
        fa(i,*)= fa(i,*)*fu1(*)
        fb(i,*)= fb(i,*)*fu2(*)
        fc(i,*)= fc(i,*)*fu3(*)
      endfor
    endif
    if plane eq 'y' then begin
      for i=0,nxf-1 do begin
        fa(i,*)= fa(i,*)*fu1(*)
        fb(i,*)= fb(i,*)*fu2(*)
        fc(i,*)= fc(i,*)*fu3(*)
      endfor
    endif
    if plane eq 'z' then begin
        fa= fa*u1(igrid) & print,min(fa),max(fa)
        fb= fb*u2(igrid) & print,min(fb),max(fb)
        fc= fc*u3(igrid) & print,min(fc),max(fc)
    endif
    if (fgrno eq 1) and (plane ne 'z') then begin
      fa=alog10(fa) & fb=alog10(fb)
      titl1='Log (N (cm^-3))' & titl2='Log (P (nPa))'
    endif     
  endif
  


; first plot...
grausig:
  !P.THICK=1.
  print, 'With postscript (output in p3.ps)? '
  read, withps
  if withps eq 'y' then begin 
        set_plot,'ps'
        psname='p'+string(fgroup,'(i2.2)')+'t' $
                           +string(time,'(i3.3)')+plane $
                           +string(igrid,'(i3.3)')+'.ps'
        device,filename=psname
        !P.THICK=2.
        if abs(xpmax-xpmin) ge abs(ypmax-ypmin) then begin 
          device,/portrait
          device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5 
          device,/inches,ysize=10.,scale_factor=1.0,yoffset=0.5
        endif
        if abs(xpmax-xpmin) lt abs(ypmax-ypmin) then begin 
          device,/landscape
;          device,/inches,ysize=8.,scale_factor=1.0,xoffset=0.5
;          device,/inches,xsize=10.0,scale_factor=1.0,yoffset=0.5
        endif
;        device,/times,/bold,font_index=3
  endif

  xpos =xpmax+0.02*(xpmax-xpmin)
  yposa=ypmin+0.3*(ypmax-ypmin)
  yposb=ypmin+0.22*(ypmax-ypmin)
  ypos1=ypmin+0.4*(ypmax-ypmin)
  ypos2=ypmin+0.6*(ypmax-ypmin)
  ypos3=ypmin+0.75*(ypmax-ypmin)
  ypos4=ypmin+0.67*(ypmax-ypmin)
  ypos5=ypmin+0.95*(ypmax-ypmin)
  ypos6=ypmin+0.87*(ypmax-ypmin)
  xpost=xpmin+0.2*(xpmax-xpmin)
  ypost=ypmax+0.01*(ypmax-ypmin)
  if abs(xpmax-xpmin) ge abs(ypmax-ypmin) then begin 
   if abs(xpmax-xpmin)/abs(ypmax-ypmin) gt 2.0 then $
                              hoch=0.2 else hoch=0.25
   if abs(xpmax-xpmin)/abs(ypmax-ypmin) lt 3.0 then $
     breit=1.25*abs(xpmax-xpmin)/abs(ypmax-ypmin)*hoch else breit=.75
      !P.MULTI=[0,0,3]
      pos1=[0.1,0.05,0.1+breit,0.05+hoch]
      pos2=[0.1,0.38,0.1+breit,0.38+hoch]
      pos3=[0.1,0.71,0.1+breit,0.71+hoch]
  endif
  if abs(xpmax-xpmin) lt abs(ypmax-ypmin) then begin 
;   if abs(ypmax-ypmin)/abs(xpmax-xpmin) gt 2.0 then $
;                              breit=0.2 else breit=0.25
   breit=0.2 
   if abs(ypmax-ypmin)/abs(xpmax-xpmin) lt 3.0 then $
     hoch=1.25*abs(ypmax-ypmin)/abs(xpmax-xpmin)*breit else hoch=.75
      !P.MULTI=[0,3,0]
      pos1=[0.05,0.1,0.05+breit,0.1+hoch]
      pos2=[0.38,0.1,0.38+breit,0.1+hoch]
      pos3=[0.71,0.1,0.71+breit,0.1+hoch]
  endif
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=3
       !Y.TICKS=3
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ypmin,ypmax]
  print, 'Plot First Page (Contour Lines)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 f -> back to fieldgroup'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'f' then goto, field
  if contin eq 'i' then goto, gridindex
  if contin eq 'c' then goto, cut
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin

  if (fgrno eq 6) and (plane ne 'z') then begin
     equi='n' & delc=0.002 & nlev=22 & endif
     
    pcont,fa,xsu,ysu,contin,pos1,titl1,xtit,ytit,cutata,cutatb,time,run,$
          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6
    pcont,fb,xsu,ysu,contin,pos2,titl2,xtit,ytit,cutata,cutatb,time,run,$
          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6
    pcont,fc,xsu,ysu,contin,pos3,titl3,xtit,ytit,cutata,cutatb,time,run,$
          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6
;    pcont1,fc,xsu,ysu,contin,pos3,equi,delc,nlev,$
;          titl3,xtit,ytit,cutata,cutatb,time,run,$
;          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        !P.THICK=1.
        print,'postscipt file '+psname+' closed'
        set_plot,'x' & withps='' & endif
    endif
  endif

  print, 'Plot 2. Page (Surface Plots)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 f -> back to fieldgroup'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'f' then goto, field
  if contin eq 'c' then goto, cut
  if contin eq 'i' then goto, gridindex
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y' or contin eq 's') then begin
    erase
    fp1=fa & fp2=fb & fp3=fc
    if contin eq 's' then begin
      fp1=smooth(fa,3) & fp2=smooth(fb,3) & fp3=smooth(fc,3) & endif
    surface,fp1,xsu,ysu,position=[0.1,0.1,0.45,0.4],ax=35,$
      ztitle=titl1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    surface,fp2,xsu,ysu,position=[0.6,0.35,0.95,0.65],ax=35,$
      ztitle=titl2,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    surface,fp3,xsu,ysu,position=[0.1,0.65,0.45,0.95],ax=35,$
      ztitle=titl3,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        !P.THICK=1.
        print,'postscipt file '+psname+' closed'
        set_plot,'x' & withps='' & endif
    endif
  endif

  print, 'Plot 3. Page (Contour Lines and Arrows)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 f -> back to fieldgroup'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'f' then goto, field
  if contin eq 'c' then goto, cut
  if contin eq 'i' then goto, gridindex
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y' or contin eq 's') $ 
                                  and (fgroup ne '1') then begin
    if plane eq 'x' then begin 
      xar=yn & yar=zn  
      f0=fc1  
      far1=interpolate(fc2,ioy,ioz,/grid)
      far2=interpolate(fc3,ioy,ioz,/grid)
      fmax=sqrt(max(far1^2+far2^2))
      if withunits eq 'p' then begin
        mul=interpolate(u1,ioz,/grid)
        for i=0,ny-1  do   f0(i,*)=f0(i,*)*u1(*)
        for i=0,nyn-1 do   far1(i,*)=far1(i,*)*mul(*)
        for i=0,nyn-1 do   far2(i,*)=far2(i,*)*mul(*)
      endif
    endif
    if plane eq 'y' then begin 
      xar=xn & yar=zn  
      f0=fc2
      far1=interpolate(fc1,iox,ioz,/grid)
      far2=interpolate(fc3,iox,ioz,/grid)
      fmax=sqrt(max(far1^2+far2^2))
      if withunits eq 'p' then begin
        mul=interpolate(u1,ioz,/grid)
        for i=0,nx-1  do   f0(i,*)=f0(i,*)*u1(*)
        for i=0,nxn-1 do   far1(i,*)=far1(i,*)*mul(*)
        for i=0,nxn-1 do   far2(i,*)=far2(i,*)*mul(*)
      endif
    endif
    if plane eq 'z' then begin 
      xar=xn & yar=yn 
      f0=fc3 
      far1=interpolate(fc1,iox,ioy,/grid)
      far2=interpolate(fc2,iox,ioy,/grid)
      fmax=sqrt(max(far1^2+far2^2))
      if withunits eq 'p' then begin
        f0=f0*u1(igrid)
        far1=far1*u1(igrid)
        far2=far2*u1(igrid)
      endif
    endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.CHARSIZE=2.0

    pcont,f0,xchoice,ychoice,contin,pos3,titl5,xtit,ytit,cutata,cutatb,$
          time,run,xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    length=1.2
    vplot,far1,far2,xar,yar,length,pos2,titl6,cutata,cutatb,time,run, $
         xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4

;    fpmax=sqrt(max(far1^2+far2^2))
;    !P.POSITION=pos2
;    vect, far1, far2, xar, yar, length=1.2,$
;    title=head5
;    xyouts,xpos,yposa,cutata 
;    xyouts,xpos,yposb,cutatb 
;    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
;    xyouts,xpos,ypos2,' '+run 
;    xyouts,xpos,ypos3,'Max=' 
;    xyouts,xpos,ypos4,'  '+string(fpmax,'(f7.3)') 
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        !P.THICK=1.
        print,'postscipt file '+psname+' closed'
        set_plot,'x' & withps='' & endif
    endif
  endif

  print, 'Plot again (Contour Lines and Arrows)?'
  print, 'Options:         y or return'
  print, 'Or:              s -> plot with smoothing'
  print, '                 f -> back to fieldgroup'
  print, '                 c -> back to cut'
  print, '                 i -> back to grid index'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if contin eq 'q' then stop
  if contin eq 'f' then goto, field
  if contin eq 'c' then goto, cut
  if contin eq 'i' then goto, gridindex
  if contin eq 'p' then goto, grausig
  if (contin eq '' or contin eq 'y' or contin eq 's') $ 
                                  and (fgroup ne '1') then goto, grausig
end


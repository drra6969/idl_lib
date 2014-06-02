; START OF MAIN PROGRAM
;
; program needs update for vect, and laser 600 resolution
  nxn=21 & nyn=15 & nzn=21     ;grid for arrow presentation
  nxf=41 & nyf=15 & nzf=41     ;grid for uniform interpol fields
  nx=long(131) & ny=long(126) & nz=long(126) & nzz=long(126) 
;  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
;  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  time=0.0 & fnumber=1
  fgroup='1' & group='1'
  name='' & contin='' & again='y' & withps='n' & run=''  
  closeps='' & jnew='y' & enew='y' 
  plane='y' & whatcut='x' & newgrid='f' & igrid=2
  I = 1

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
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx & prof=bx 
    ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
;-----------PARAMETER----------------
  xmin =-0.5 & ymin = 0.  & zmin = 4.
  xmax = 4. & ymax = 15.   & zmax = 100.0
  readu, 8,  xvec,dxvec,dxh,dxh,dxh,dxh,dxh,yvec,dyvec,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
             
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
  readu, 8,  bx,by,bz
  readu, 8,  sx,sy,sz
  readu, 8,  rho,u,res,prof
   ischritt=1l & iaus=1l & isafe=1l & iend=1l 
   dt=0.1 & zeit=0.1 & gamma=0.1 & eta=0.1 & rho0=0.1
;   itart, ivisrho, nglatt, naus, ifehler, igrid, idiag, isat
  readu, 8, ischritt,iaus,isafe,iend,dt,gamma,eta,rho0    
  close, 8

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

;--- generation of new grid for velocity vectors---
;  newgrid, x,y,z,xmin,xmax,ymin
;  ,ymax,zmin,zmax, $
;           nxn,nyn,nzn,xn,yn,zn,iox,ioy,ioz,dxn,dyn,dzn

;--- generation of grid for uniform interpol fields---
;  newgrid, x,y,z,xmin,xmax,ymin
;  ,ymax,zmin,zmax, $
;           nxf,nyf,nzf,xf,yf,zf,ioxf,ioyf,iozf,dxf,dyf,dzf

  f1=rho & p=2*u^(5.0/3.0) & f2=p & f4=p & f3=p+bx^2+by^2+bz^2
  head1='Density' & head2='Pressure' & head3='Temp'
field:
  print, 'Input Fieldgroup:'
  print, 'Options: 1 - rho, p, temp'
  print, '         2 - velocity, v'
  print, '         3 - magnetic field b'
  print, '         4 - current density'
  print, '         5 - electric field'
  print, '         6 - res, jpar, Epar'
  print, '         7 - J X B'
  print, '         8 - grad p'
  print, '         9 - summe 7 und 8'
  print, '         q - terminate'
  print, '    return - no changes applied'
  print, 'Present Choice: ', fgroup
  read, group & if group ne '' then fgroup=group
  if (strno(fgroup) ne -1) then fgrno=strno(fgroup)
  if group eq '' then print, 'present choice=',group,' not altered'
  if fgroup eq 'q' then stop
  if fgrno eq 1 then begin
    head1='Density' & head2='Pressure' & head3='Temp' & head4=' '
    f1=rho & f2=2*u^(5.0/3.0) & f3=f2/f1 & endif
  if fgrno eq 2 then begin
    head1='Vel. Vx' & head2='Vel. Vy' & head3='Vel. Vz'& head4='Velocity V'
    f1=sx/rho & f2=sy/rho & f3=sz/rho & endif 
  if fgrno eq 3 then begin
    head1='Magn. Field Bx' & head2='Magn. Field By'
    head3='Magn. Field Bz' & head4='Magn. Field B'
    f1=bx & f2=by & f3=bz & endif 
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
    f1=jx & f2=jy & f3=jz 
  endif
  if ( (fgrno eq 5)  or ((fgrno gt 5) and (enew eq 'y')) ) then begin
    head1='Electr. Field Ex' & head2='Electr. Field Ey'
    head3='Electr. Field Ez' & head4='Electr. Field E'
    if enew eq 'y' then begin
      ex=(sy*bz-sz*by)/rho+res*jx
      ey=(sz*bx-sx*bz)/rho+res*jy
      ez=(sx*by-sy*bx)/rho+res*jz
      enew='n'
    endif
    f1=ex & f2=ey & f3=ez 
  endif
  if fgrno eq 6 then begin
    head1='RES' & head2='J parallel' 
    head3='E parallel' 
       f1 = sqrt(bx*bx+by*by+bz*bz)
       f2 = (jx*bx+jy*by+jz*bz)/f1
       f1 = res
       f3 = res*f2
  endif
  if fgrno eq 7 then begin
    head1='x comp JXB' & head2='y comp JXB'
    head3='z comp JXB'
      f1=jy*bz-jz*by
      f2=jz*bx-jx*bz
      f3=jx*by-jy*bx
  endif
  if fgrno eq 8 then begin
    head1='-gradx P' & head2='-grady P'
    head3='-gradz P'
      f1=shift(p,-1,0,0)-shift(p,1,0,0)
      f2=shift(p,0,-1,0)-shift(p,0,1,0)
      f3=shift(p,0,0,-1)-shift(p,0,0,1)
      for i=1,nx-2 do f1(i,*,*)=-dx(i)*f1(i,*,*)
      for j=1,ny-2 do f2(*,j,*)=-dy(j)*f2(*,j,*)
      for k=1,nz-2 do f3(*,*,k)=-dz(k)*f3(*,*,k)
  endif
  if fgrno eq 9 then begin
    head1='(ExB)x' & head2='(ExB)y'
    head3='(ExB)z'
      f1=ey*bz-ez*by
      f2=ez*bx-ex*bz
      f3=ex*by-ey*bx
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
  head5=head4+plane
  if plane eq 'x' then begin 
    nplane=nx & coord=x & head6=head4+'y,z Comp' & endif
  if plane eq 'y' then begin 
    nplane=ny & coord=y & head6=head4+'z,x Comp' & endif
  if plane eq 'z' then begin 
    nplane=nz & coord=z & head6=head4+'x,y Comp' & endif

gridindex:
  print, plane, 'Coordinates:
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

; first plot...
grausig:
  !P.THICK=1.
  print, 'With postscript (output in p3.ps)? '
  read, withps
  if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='p3.ps'
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
   if abs(xpmax-xpmin)/abs(ypmax-ypmin) gt 1.6 then $
                              hoch=0.2 else hoch=0.25
   if abs(xpmax-xpmin)/abs(ypmax-ypmin) lt 2.4 then $
     breit=1.25*abs(xpmax-xpmin)/abs(ypmax-ypmin)*hoch else breit=.6
      !P.MULTI=[0,0,3]
      pos1=[0.1,0.05,0.1+breit,0.05+hoch]
      pos2=[0.1,0.38,0.1+breit,0.38+hoch]
      pos3=[0.1,0.71,0.1+breit,0.71+hoch]
  endif
  if abs(xpmax-xpmin) lt abs(ypmax-ypmin) then begin 
;   if abs(ypmax-ypmin)/abs(xpmax-xpmin) gt 1.6 then $
;                              breit=0.2 else breit=0.25
   breit=0.2 
   if abs(ypmax-ypmin)/abs(xpmax-xpmin) lt 2.4 then $
     hoch=1.25*abs(ypmax-ypmin)/abs(xpmax-xpmin)*breit else hoch=.6
      !P.MULTI=[0,3,0]
      pos1=[0.05,0.1,0.05+breit,0.1+hoch]
      pos2=[0.38,0.1,0.38+breit,0.1+hoch]
      pos3=[0.71,0.1,0.71+breit,0.1+hoch]
  endif
       
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
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=4
       !Y.TICKS=4
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ypmin,ypmax]


    pcont,fa,xsu,ysu,contin,pos1,head1,xtit,ytit,cutata,cutatb,time,run,$
          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    pcont,fb,xsu,ysu,contin,pos2,head2,xtit,ytit,cutata,cutatb,time,run,$
          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    pcont,fc,xsu,ysu,contin,pos3,head3,xtit,ytit,cutata,cutatb,time,run,$
          xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        !P.THICK=1.
        print,'postscipt file p3.ps closed'
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
      ztitle=head1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    surface,fp2,xsu,ysu,position=[0.6,0.35,0.95,0.65],ax=35,$
      ztitle=head2,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    surface,fp3,xsu,ysu,position=[0.1,0.65,0.45,0.95],ax=35,$
      ztitle=head3,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        !P.THICK=1.
        print,'postscipt file p3.ps closed'
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
    endif
    if plane eq 'y' then begin 
      xar=xn & yar=zn  
      f0=fc2
      far1=interpolate(fc1,iox,ioz,/grid)
      far2=interpolate(fc3,iox,ioz,/grid)
      fmax=sqrt(max(far1^2+far2^2))
    endif
    if plane eq 'z' then begin 
      xar=xn & yar=yn 
      f0=fc3 
      far1=interpolate(fc1,iox,ioy,/grid)
      far2=interpolate(fc2,iox,ioy,/grid)
      fmax=sqrt(max(far1^2+far2^2))
    endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.CHARSIZE=2.0

    pcont,f0,xchoice,ychoice,contin,pos3,head5,xtit,ytit,cutata,cutatb,$
          time,run,xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    length=1.2
    vplot,far1,far2,xar,yar,length,pos2,head6,cutata,cutatb,time,run, $
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
        print,'postscipt file p3.ps closed'
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

  @ÔÄ  qÈindex'
  print, '                 p -> back to postscript'
  print, '                 q -> terminate'
  read, contin
  if
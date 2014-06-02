; START OF MAIN PROGRAM
;  attention! y and z interchanged

  nxn=21 & nyn=21 & nzn=21     ;grid for arrow presentation
  nxf=41 & nyf=41 & nzf=41     ;grid for uniform interpol fields
  nx=long(131) & ny=long(126) & nz=long(126) 
  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  time=0.0 & fnumber=1
  fgroup='1' & group='1' & jnew='y' 
  name='' & contin='' & again='y' & withps='n' & run=''  
  closeps=''
  
;-----------PARAMETER----------------
;  xmin =-2.5 & ymin =  0   & zmin = -150
;  xmax = 2.5 & ymax =  7   & zmax =   40
;  xmin =-0.5 & ymin = 0.0   & zmin = -100.0
;  xmax = 1.0 & ymax = 25.0   & zmax =   50.0
  xmin =-4.0 & ymin = 0.0   & zmin = -100.0
  xmax = 4.0 & ymax = 50.0   & zmax =   50.0

  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  name='magt'+string(fnumber,'(i1)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    print, 'time=',time
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx 
    h1=fltarr(nx,nz,ny,/NOZERO) & h2=h1 & h3=h1 & f3=h1 & f4=h1
    x=fltarr(nx,/NOZERO) & dx=x & dxh=x
    y=fltarr(ny,/NOZERO) & dy=y & dyh=y
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
  readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
;  print, 'x:', x
;  print, 'y:', y
;  print, 'z:', z
  readu, 8, bx,by,bz
  readu, 8, sx,sy,sz
  readu, 8, rho,u,res
    print, nx,ny,nz,time
    print, 'Boundaries in simulation coordinates:' 
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
  close, 8
  sx=-sx  & bx=-bx  
  for iy=0,ny-1 do for iz=0,nz-1 do begin
    h1(*,iz,iy)=rho(*,iy,iz)
    h2(*,iz,iy)=u(*,iy,iz)
    h3(*,iz,iy)=res(*,iy,iz)
  endfor
  rho=h1 & u=h2 & res=h3
  for iy=0,ny-1 do for iz=0,nz-1 do begin
    h1(*,iz,iy)=bx(*,iy,iz)
    h2(*,iz,iy)=by(*,iy,iz)
    h3(*,iz,iy)=bz(*,iy,iz)
  endfor
  bx=h1 & bz=h2 & by=h3
  for iy=0,ny-1 do for iz=0,nz-1 do begin
    h1(*,iz,iy)=sx(*,iy,iz)
    h2(*,iz,iy)=sy(*,iy,iz)
    h3(*,iz,iy)=sz(*,iy,iz)
  endfor
  sx=h1 & sz=h2 & sy=h3
  h1=y &  y=z &  z=h1 & h1=dy &  dy=dz &  dz=h1
  nh=ny & ny=nz & nz=nh

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


;--- generation of new grid for velocity vectors---
  xn=findgen(nxn) & yn=findgen(nyn) & zn=findgen(nzn)
  dxn=(xmax-xmin)/float(nxn-1) &  xn=xn*dxn+xmin
  dyn=(ymax-ymin)/float(nyn-1) &  yn=yn*dyn+ymin
  dzn=(zmax-zmin)/float(nzn-1) &  zn=zn*dzn+zmin
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xn(in) gt x(k+1) do k=k+1
    iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yn(in) gt y(k+1) do k=k+1
    ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zn(in) gt z(k+1) do k=k+1
    ioz(in) = float(k) + (zn(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzn-1
  xn=-xn

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
  x=-x &  dx=-dx & hmax=-xmin & xmin=-xmax & xmax=hmax
  xf=-xf
  
; current density
    if jnew eq 'y' then begin
      f1 = shift(bz,0,-1,0)-shift(bz,0,1,0)
      for j=1,ny-2 do f3(*,j,*)=dy(j)*f1(*,j,*)
      f2 = shift(by,0,0,-1)-shift(by,0,0,1)
      for k=1,nz-2 do f4(*,*,k)=dz(k)*f2(*,*,k)
      c1 = f3-f4
      f1 = shift(bx,0,0,-1)-shift(bx,0,0,1)
      for k=1,nz-2 do f3(*,*,k)=dz(k)*f1(*,*,k)
      f2 = shift(bz,-1,0,0)-shift(bz,1,0,0)
      for i=1,nx-2 do f4(i,*,*)=dx(i)*f2(i,*,*)
      c2 = f3-f4
      f1 = shift(by,-1,0,0)-shift(by,1,0,0)
      for i=1,nx-2 do f3(i,*,*)=dx(i)*f1(i,*,*)
      f2 = shift(bx,0,-1,0)-shift(bx,0,1,0)
      for j=1,ny-2 do f4(*,j,*)=dy(j)*f2(*,j,*)
      c3 = f3-f4
      c1([0,nx-1],*,*)=0.0 & c2([0,nx-1],*,*)=0.0 & c3([0,nx-1],*,*)=0.0
      c1(*,[0,ny-1],*)=0.0 & c2(*,[0,ny-1],*)=0.0 & c3(*,[0,ny-1],*)=0.0 
      c1(*,*,[0,nz-1])=0.0 & c2(*,*,[0,nz-1])=0.0 & c3(*,*,[0,nz-1])=0.0 
      jnew='n'
      h1=res*(c1*bx+c2*by+c3*bz)/sqrt(bx*bx+by*by+bz*bz)
    endif    

  f1=rho & f2=2*u^(5.0/3.0) & f3=h1
  head1='Density' & head2='Pressure' & head3='E parallel'
  plane='x' & newgrid='10' & igrid=10
  group='1' & whatcut='10'
field:
  print, 'Input Fieldgroup:'
  print, 'Options: 1 - rho, p, par E field'
  print, '         2 - velocity, v'
  print, '         3 - magnetic field b'
  print, '         4 - current density'
  print, '         5 - electric field'
  print, '         q - terminate'
  print, '    return - no changes applied'
  print, 'Present Choice: ', fgroup
  read, group & if group ne '' then fgroup=group
  if group eq '' then print, 'present choice=',group,' not altered'
  if fgroup eq 'q' then stop
  if fgroup eq '1' then begin
    head1='Density' & head2='Pressure' & head3='E parallel'
    f1=rho & f2=2*u^(5.0/3.0) & f3=h1 & endif
  if fgroup eq '2' then begin
    head1='Vel. Vx' & head2='Vel. Vy' & head3='Vel. Vz'
    f1=sx/rho & f2=sy/rho & f3=sz/rho & endif 
  if fgroup eq '3' then begin
    head1='Magn. Field Bx' & head2='Magn. Field By'
    head3='Magn. Field Bz'
    f1=bx & f2=by & f3=bz & endif 
  if (fgroup eq '4') then begin 
    head1='Curr. Dens. Jx' & head2='Curr. Dens. Jy'
    head3='Curr. Dens. Jz'
    f1=c1 & f2=c2 & f3=c3 & endif
  if fgroup eq '5' then begin
    head1='Electr. Field Ex' & head2='Electr. Field Ey'
    head3='Electr. Field Ez'
    f1=-(sy*bz-sz*by)/rho+res*c1
    f2=-(sz*bx-sx*bz)/rho+res*c2
    f3=-(sx*by-sy*bx)/rho+res*c3 & endif

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
  if plane eq 'x' then begin & nplane=nx & coord=x & endif
  if plane eq 'y' then begin & nplane=ny & coord=y & endif
  if plane eq 'z' then begin & nplane=nz & coord=z & endif

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
    xplot=y & yplot=z & sizeratio=(ypmax-ypmin)/(xpmax-xpmin)
  endif
  if plane eq 'y' then begin
    cutata='y=' & cutatb=string(y(igrid),'(f7.2)')
    xtit='x'    & ytit='z'    & xchoice=x   & ychoice=z
    xpmin=xmin  & xpmax=xmax  & ypmin=zmin  & ypmax=zmax 
    fc1=f1(*,igrid,*) & fc1=reform(fc1) 
    fc2=f2(*,igrid,*) & fc2=reform(fc2) 
    fc3=f3(*,igrid,*) & fc3=reform(fc3) 
    xplot=x  &  yplot=z & sizeratio=(ypmax-ypmin)/(xpmax-xpmin)
  endif
  if plane eq 'z' then begin
    cutata='z=' & cutatb=string(z(igrid),'(f7.2)')
    xtit='y'    & ytit='x'    & xchoice=y   & ychoice=x
    xpmin=ymin  & xpmax=ymax  & ypmin=xmin  & ypmax=xmax 
    fc1=f1(*,*,igrid) & fc2=f2(*,*,igrid) & fc3=f3(*,*,igrid)  
    fc1=reform(fc1) & fc2=reform(fc2) & fc3=reform(fc3) 
    fc1=rotate(fc1,4) & fc2=rotate(fc2,4) & fc3=rotate(fc3,4)
    xplot=y  &  yplot=x & sizeratio=(xpmax-xpmin)/(ypmax-ypmin)
  endif

; first plot...
grausig:
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & !P.THICK=1. & endif
    endif else begin
      print, 'With postscript (output in p3.ps)? '
      read, withps
      if withps eq 'y' then begin 
        !P.THICK=2.
        set_plot,'ps'
        device,filename='p3.ps'
        if (plane eq 'z') then begin
           device,/portrait
           device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5 
           device,/inches,ysize=10.,scale_factor=1.0,yoffset=0.5
        endif
        if (plane eq 'x') or (plane eq 'y') then begin
           device,/landscape
;           device,/inches,xsize=10.,scale_factor=1.0,xoffset=0.5 
;           device,/inches,ysize=8.,scale_factor=1.0,yoffset=0.5
        endif
      endif
    endelse
;        device,/times,/bold,font_index=3

; COORDINATES FOR PLOTS
   srat=1.0
   if (sizeratio lt 3.1) then begin
     dpx=0.21 & dpy=0.282*sizeratio
   endif
   if (sizeratio ge 3.1 and sizeratio le 4.5) then begin
     dpy=0.88 & dpx=0.66/sizeratio
   endif
   if (sizeratio gt 4.5) then begin
     dpy=0.88 & dpx=0.66/4.5
     srat=sizeratio/4.5
   endif
   dpy=0.75
   print, sizeratio, dpx, dpy
    xleft=0.06 & xinter=0.12 & hop=0.3*xinter
    xa1=xleft & xe1=xleft+dpx  
    xa2=xe1+xinter & xe2=xa2+dpx
    xa3=xe2+xinter & xe3=xa3+dpx
    ylo=0.06 & yup=ylo+dpy
    
  !X.TICKS=4
  if (plane eq 'z') then !Y.TICKS=4 
  if (plane eq 'x') or (plane eq 'y') then !Y.TICKS=3 
  xrmin=xpmin & xrmax=xpmax & yrmin=ypmin & yrmax=ypmax 
  if (plane eq 'y') then begin $
      xrmin=xpmax & xrmax=xpmin & endif
  if (plane eq 'z') then begin
      yrmin=ypmax & yrmax=ypmin & endif
  xpos =xrmax+0.02*(xrmax-xrmin)
  yposa=yrmin+0.3*(yrmax-yrmin)
  yposb=yrmin+0.22*(yrmax-yrmin)
  ypos1=yrmin+0.4*(yrmax-yrmin)
  ypos2=yrmin+0.6*(yrmax-yrmin)
  ypos3=yrmin+0.75*(yrmax-yrmin)
  ypos4=yrmin+0.67*(yrmax-yrmin)
  ypos5=yrmin+0.95*(yrmax-yrmin)
  ypos6=yrmin+0.87*(yrmax-yrmin)
  xpost=xrmin+0.2*(xrmax-xrmin)
  ypost=yrmax+0.01*(yrmax-yrmin)
  
  if (plane eq 'x') or (plane eq 'y') then begin
      !P.REGION=[0.,0.,1.0,1.0]
      !P.MULTI=[0,3,0]
      pos1=[xa1,ylo,xe1,yup]
      pos2=[xa2,ylo,xe2,yup]
      pos3=[xa3,ylo,xe3,yup]
      yposb=yrmin+0.25*(ypmax-ypmin)
      ypos4=yrmin+0.7*(ypmax-ypmin)
      ypos6=yrmin+0.9*(ypmax-ypmin)
  endif
  if (plane eq 'z') then begin
      !P.REGION=[0.,0.,1.0,1.25]
      !P.MULTI=[0,1,3]
      pos1=[ylo,xa1,yup,xe1]
      pos2=[ylo,xa2,yup,xe2]
      pos3=[ylo,xa3,yup,xe3]
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
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.RANGE=[xrmin,xrmax]
       !Y.RANGE=[yrmin,yrmax]
       !X.THICK=2
       !Y.THICK=2

    if contin eq 's' then fp1=smooth(fc1,3) else fp1=fc1 
    fpmax=max(fp1) & fpmin=min(fp1)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos1
    contour,fp1,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    title=head1,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit
    xyouts,xpos,yposa,cutata
    xyouts,xpos,yposb,cutatb
    xyouts,xpos,ypos1,'t='+string(time,'(i3)')
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)')
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)') 

    if contin eq 's' then fp2=smooth(fc2,3) else fp2=fc2 
    fpmax=max(fp2) & fpmin=min(fp2)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos2
    contour,fp2,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    title=head2,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit 
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)') 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)') 

    if contin eq 's' then fp3=smooth(fc3,3) else fp3=fc3 
    fpmax=max(fp3) & fpmin=min(fp3)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos3
    if fgroup eq '1' then $       
      contour,fp3,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
      c_linestyle=findgen(21)*delf+fpmin gt fptr,$
      title=head3,xstyle=1,ystyle=1,$
      xtitle=xtit,ytitle=ytit  $
    else $
      contour,fp3,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
      c_linestyle=findgen(21)*delf+fpmin lt fptr,$
      title=head3,xstyle=1,ystyle=1,$
      xtitle=xtit,ytitle=ytit 
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)') 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)') 
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & !P.THICK=1. & endif
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
    if plane eq 'x' then begin
      fa=interpolate(fc1,ioyf,iozf,/grid)
      fb=interpolate(fc2,ioyf,iozf,/grid)
      fc=interpolate(fc3,ioyf,iozf,/grid)
      xsu=yf & ysu=zf & endif 
    if plane eq 'y' then begin
      fa=interpolate(fc1,ioxf,iozf,/grid)
      fb=interpolate(fc2,ioxf,iozf,/grid)
      fc=interpolate(fc3,ioxf,iozf,/grid)
      xsu=xf & ysu=zf & endif 
    if plane eq 'z' then begin
      fa=interpolate(fc1,ioyf,ioxf,/grid)
      fb=interpolate(fc2,ioyf,ioxf,/grid)
      fc=interpolate(fc3,ioyf,ioxf,/grid)
      xsu=yf & ysu=xf & endif 
    erase
    if contin eq 's' then begin
      fa=smooth(fa,3) & fb=smooth(fb,3) & fc=smooth(fc,3) & endif
    surface,fa,xsu,ysu,position=[0.1,0.1,0.45,0.4],ax=35,$
      ztitle=head1,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    surface,fb,xsu,ysu,position=[0.6,0.35,0.95,0.65],ax=35,$
      ztitle=head2,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    surface,fc,xsu,ysu,position=[0.1,0.65,0.45,0.95],ax=35,$
      ztitle=head3,xstyle=1,ystyle=1,xtitle=xtit,ytitle=ytit
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & !P.THICK=1. & endif
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
      if fgroup eq '2' then begin
        head4='Velocity Vx' & head5='Velocity Vy/Vz' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field Bx' & head5='Magn. Field By/Bz' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jx' & head5='Curr. Dens. Jy/Jz' & endif
      if fgroup eq '5' then begin
        head4='Electric Field Ex' & head5='Electric Field Ey/Ez' & endif
      xar=yn & yar=zn  
      f0=fc1  
      far1=interpolate(fc2,ioy,ioz,/grid)
      far2=interpolate(fc3,ioy,ioz,/grid)
      fmax=sqrt(max(far1^2+far2^2))
    endif
    if plane eq 'y' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vy' & head5='Velocity Vz/Vx' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field By' & head5='Magn. Field Bz/Bx' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jy' & head5='Curr. Dens. Jz/Jx' & endif
      if fgroup eq '5' then begin
        head4='Electric Field Ey' & head5='Electric Field Ez/Ex' & endif
      xar=xn & yar=zn  
      f0=fc2
      far1=interpolate(fc1,iox,ioz,/grid)
      far2=interpolate(fc3,iox,ioz,/grid)
      fmax=sqrt(max(far1^2+far2^2))
    endif
    if plane eq 'z' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vz' & head5='Velocity Vx/Vy' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field Bz' & head5='Magn. Field Bx/By' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jz' & head5='Curr. Dens. Jx/Jy' & endif
      if fgroup eq '5' then begin
        head4='Electric Field Ez' & head5='Electric Field Ex/Ey' & endif
      xar=yn & yar=xn 
      f0=fc3 
      far1=interpolate(fc2,ioy,iox,/grid)
      far2=interpolate(fc1,ioy,iox,/grid)
      fmax=sqrt(max(far1^2+far2^2))
    endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.CHARSIZE=2.0

    if contin eq 's' then fp1=smooth(f0,3) else fp1=f0 
    fpmax=max(fp1) & fpmin=min(fp1)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos1
    contour,fp1,xchoice,ychoice,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    title=head4,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit
    xyouts,xpos,yposa,cutata
    xyouts,xpos,yposb,cutatb
    xyouts,xpos,ypos1,'t='+string(time,'(i3)')
    xyouts,xpos,ypos2,' '+run
    xyouts,xpos,ypos3,'Max='
    xyouts,xpos,ypos4,' '+string(fpmax,'(f7.3)')
    xyouts,xpos,ypos5,'Min='
    xyouts,xpos,ypos6,' '+string(fpmin,'(f7.3)')

    fpmax=sqrt(max(far1^2+far2^2))
    !P.POSITION=pos2
    lv=1.0
    if fgroup eq 3 then lv=1.0
    if fgroup eq 5 then lv=0.2 
;    if plane eq 'y' then lv=0.2   
    vect, far1, far2, xar, yar, length=lv*fpmax,$
    title=head5
    ;,sizerat=srat
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,'  '+string(fpmax,'(f7.3)') 
    if withps eq 'y' then begin 
      print, 'close postscipt file?'
      read, closeps
      if closeps eq 'y' then begin & device,/close
        print,'postscipt file p3.ps closed'
        set_plot,'x' & withps='' & !P.THICK=1. & endif
    endif
  endif

  print, 'Plot 4. Page (3-D Surface)?'
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

    s=size(res)
    if s(0) ne 3 then write, 'fehler'
    amin=min(bx*bx) & amax=max(bx*bx)
    thresh=amin+0.1*(amax-amin)
    print,amin,amax,thresh
;    surface, fltarr(2,2), /nodata, /save, $
;      xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]
;    if n_elements(low) eq 0 then low=0
    shade_volume, bx*bx, thresh, v, p;, low=low
    tv, polyshade(v,p,/t3d)
    surface, fltarr(2,2), /nodata, /save, /noerase,$
      xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]

    read, contin
    amin=min(bz) & amax=max(bz)
    thresh=0.0
    print,amin,amax,thresh
;    surface, fltarr(2,2), /nodata, /save, $
;      xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]
;    if n_elements(low) eq 0 then low=0
    shade_volume, bz, thresh, v, p;, low=low
    tv, polyshade(v,p,/t3d)
    surface, fltarr(2,2), /nodata, /save, /noerase,$
      xrange=[0,s(1)-1],yrange=[0,s(2)-1],zrange=[0,s(3)-1]
  endif

end



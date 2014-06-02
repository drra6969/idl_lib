; START OF MAIN PROGRAM
;
; program needs update for vect, and laser 600 resolution
  nxn=21 & nyn=15 & nzn=21     ;grid for arrow presentation
  nxf=41 & nyf=15 & nzf=41     ;grid for uniform interpol fields
  nx=long(131) & ny=long(126) & nz=long(126) & nzzz=long(126) 
  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  time=0.0 & fnumber=1
  fgroup='1' & group='1'
  name='' & contin='' & again='y' & withps='n' & run=''  
  closeps='' & jnew='y' & enew='y' 
  plane='y' & whatcut='x' & newgrid='f' & igrid=2
               I = 1


  
  print, 'Input filenumber'
  read, fnumber
;  print, 'Which case?'
;  read, run
;  name='magall'+string(fnumber,'(i1)')
  name='nagtap'+string(fnumber,'(i1)')
FILESTATUS = FSTAT(8)
IF FILESTATUS.OPEN NE 0 THEN CLOSE, 8
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time, nzzz
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz,'   nzz=',nzzz
    print, 'time=',time
    NV= NX * NY
XVEC = FLTARR (NV)
YVEC = FLTARR (NV)
DXVEC = FLTARR (NV)
DYVEC = FLTARR (NV)
    x=fltarr(nx,/NOZERO) & dx=x & dxh=xvec
    y=fltarr(ny,/NOZERO) & dy=y & dyh=yvec
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z & rho0=z & p0=z
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    snx=bx & sny=bx & snz=bx & rhon=bx & un=bx & res=bx 
    ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx & nu12s = bx
    
;-----------PARAMETER----------------
  xmin =-4. & ymin = 0.  & zmin = 0.
  xmax = 4. & ymax = 30.   & zmax = 10.
  readu, 8,  xvec,dxvec,dxh,dxh,dxh,dxh,dxh,yvec,dyvec,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
             
             x= xvec(0:nx-1) 
             FOR I=0,NY-1 DO Y(I) = YVEC(I*NX)
             dx=dxvec(0:nx-1)
             FOR I=0,NY-1 DO DY(I) = DYVEC(I*NX)
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
  readu, 8,  snx,sny,snz
  readu, 8,  rhon,un,nu12s
ISCHRITT = 1l & IAUS = 1l & ISAFE = 1l & IEND = 1l
ITART = 1l & IVISRHO = 1l & NGLATT = 1l & NAUS = 1l
IFEHLER = 1l & IGRID = 2l & IDIAG = 1l
ISAT = 1l & PROF = BX
DT = 0.1 & GAMMA = 0.1 & ETA = 0.1 & grav = 0.1 & rhon0 = 0.1 
READU, 8, ISCHRITT,IAUS,ISAFE,IEND,DT,GAMMA,ETA,grav,rhon0    
;READU, 8, XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX   
  close, 8
  
  rho0(nzzz-1) = rhon0*exp(-grav*z(nzzz-1)*2.)
  rho0(nzzz-2) = rhon0*exp(-grav*z(nzzz-2)*2.)
  for iz=nzzz-2,1,-1 do  rho0(iz-1) = rho0(iz+1) + 2.*grav/dz(iz)*rho0(iz)
  for iz=nzzz,nz-1 do  rho0(iz) = rho0(nzzz-1)
  p0 = rho0
  
  amin=min(rhon) & amax=max(rhon) &  thresh=amin+0.5*(amax-amin)
  print,'rhon:',amin,amax,thresh
  amin=min(un) & amax=max(un) &  thresh=amin+0.5*(amax-amin)
  print,'un:',amin,amax,thresh
  amin=min(snx) & amax=max(snx) &  thresh=amin+0.5*(amax-amin)
  print,'snx:',amin,amax,thresh
  amin=min(sny) & amax=max(sny) &  thresh=amin+0.5*(amax-amin)
  print,'sny:',amin,amax,thresh
  amin=min(snz) & amax=max(snz) &  thresh=amin+0.5*(amax-amin)
  print,'snz:',amin,amax,thresh

;--- generation of new grid for velocity vectors---
  xn=findgen(nxn) & yn=findgen(nyn) & zn=findgen(nzn)
  dxn=(xmax-xmin)/float(nxn-1) &  xn=xn*dxn+xmin
  dyn=(ymax-ymin)/float(nyn-1) &  yn=yn*dyn+ymin
  dzn=(zmax-zmin)/float(nzn-1) &  zn=zn*dzn+zmin
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xn(in) ge x(k+1) do k=k+1
    iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yn(in) ge y(k+1) do k=k+1
    ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyn-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zn(in) ge z(k+1) do k=k+1
    ioz(in) = float(k) + (zn(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzn-1

  print, 'hier1'
; generation of grid for uniform interpol fields
  xf=findgen(nxf) & yf=findgen(nyf) & zf=findgen(nzf) 
  dxf=(xmax-xmin)/float(nxf-1) &  xf=xf*dxf+xmin
  dyf=(ymax-ymin)/float(nyf-1) &  yf=yf*dyf+ymin
  dzf=(zmax-zmin)/float(nzf-1) &  zf=zf*dzf+zmin 
  in=-1 &  k=0
  repeat begin &    in=in+1
    while xf(in) ge x(k+1) do k=k+1
    ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
  endrep until in eq nxf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while yf(in) ge y(k+1) do k=k+1
    ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
  endrep until in eq nyf-1
  in=-1 &  k=0
  repeat begin &    in=in+1
    while zf(in) ge z(k+1) do k=k+1
    iozf(in) = float(k) + (zf(in)-z(k))/(z(k+1)-z(k))        
  endrep until in eq nzf-1

  f1=rhon & pn=2*un^(5.0/3.0) & f2=pn & f4=pn & f3=pn+bx^2+by^2+bz^2
  head1='Density' & head2='Pressure' & head3='NU12S'
field:
  print, 'Input Fieldgroup:'
  print, 'Options: 1 - rhon, pn, tn'
  print, '         2 - velocity, vn'
  print, '         3 - perturbations rhon, pn, tn'
  print, '         q - terminate'
  print, '    return - no changes applied'
  print, 'Present Choice: ', fgroup
  read, group & if group ne '' then fgroup=group
  if group eq '' then print, 'present choice=',group,' not altered'
  if fgroup eq 'q' then stop
  if fgroup eq '1' then begin
    head1='Density' & head2='Pressure' & head3='Temp'
;head3='NU12S'
    f1=rhon & f2=2*un^(5.0/3.0) & f3=f2/f1 & endif
  if fgroup eq '2' then begin
    head1='Vel. Vnx' & head2='Vel. Vny' & head3='Vel. Vnz'
    f1=snx/rhon & f2=sny/rhon & f3=snz/rhon & endif 
  if fgroup eq '3' then begin
    head1='rhon' & head2='pn'
    head3='TN'
    for iz=0,nz-1 do f1(*,*,iz)=rhon(*,*,iz)-rho0(iz)
    for iz=0,nz-1 do f2(*,*,iz)=pn(*,*,iz)-p0(iz)
    for iz=0,nz-1 do f3(*,*,iz)=pn(*,*,iz)/rhon(*,*,iz)-p0(iz)/rho0(iz)
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
  strf1='(f8.3)'
  if fgroup eq '2' then  strf1='(f9.6)'

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

    if contin eq 's' then fp1=smooth(fa,3) else fp1=fa 
    fpmax=max(fp1) & fpmin=min(fp1)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos1
    contour,fp1,xsu,ysu,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    title=head1,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit 
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,strf1) 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,strf1) 

    if contin eq 's' then fp2=smooth(fb,3) else fp2=fb 
    fpmax=max(fp2) & fpmin=min(fp2)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos2
    contour,fp2,xsu,ysu,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    title=head2,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit 
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,strf1) 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,strf1) 

    if contin eq 's' then fp3=smooth(fc,3) else fp3=fc 
    fpmax=max(fp3) & fpmin=min(fp3)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/20. & if delf lt 0.000000001 then delf=0.0001
    !P.POSITION=pos3
    contour,fp3,xsu,ysu,levels=findgen(21)*delf+fpmin,$
    c_linestyle=findgen(21)*delf+fpmin lt fptr,$
    title=head3,xstyle=1,ystyle=1,$
    xtitle=xtit,ytitle=ytit 
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,strf1) 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,strf1) 
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
      if fgroup eq '2' then begin
        head4='Velocity Vnx' & head5='Velocity Vny/Vnz' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field Bx' & head5='Magn. Field By/Bz' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jx' & head5='Curr. Dens. Jy/Jz' & endif
      xar=yn & yar=zn  
      f0=fc1  
      far1=interpolate(fc2,ioy,ioz,/grid)
      far2=interpolate(fc3,ioy,ioz,/grid)
      
      fmax=sqrt(max(far1^2+far2^2))
    endif
    if plane eq 'y' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vny' & head5='Velocity Vnz/Vnx' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field By' & head5='Magn. Field Bz/Bx' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jy' & head5='Curr. Dens. Jz/Jx' & endif
      xar=xn & yar=zn  
      f0=fc2
      far1=interpolate(fc1,iox,ioz,/grid)
      far2=interpolate(fc3,iox,ioz,/grid)
      fmax=sqrt(max(far1^2+far2^2))
    endif
    if plane eq 'z' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vnz' & head5='Velocity Vnx/Vny' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field Bz' & head5='Magn. Field Bx/By' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jz' & head5='Curr. Dens. Jx/Jy' & endif

      xar=xn & yar=yn 
      f0=fc3 
      far1=interpolate(fc1,iox,ioy,/grid)
      far2=interpolate(fc2,iox,ioy,/grid)
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
    xyouts,xpos,ypos4,' '+string(fpmax,strf1) 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,strf1) 

    fpmax=sqrt(max(far1^2+far2^2))
    !P.POSITION=pos2
;    vect, far1, far2, xar, yar, length=1.2*fpmax,$
    vect, far1, far2, xar, yar, length=1.2,$
    title=head5
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,'  '+string(fpmax,strf1) 
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



; START OF MAIN PROGRAM
;
; program needs update for vect, and laser 600 resolution
  nxn=21 & nyn=21 & nzn=21     ;grid for arrow presentation
  nxf=121 & nyf=121 & nzf=121     ;grid for uniform interpol fields
  nx=long(131) & ny=long(126) & nz=long(126) 
  iox=fltarr(nxn) & ioy=fltarr(nyn) & ioz=fltarr(nzn)
  ioxf=fltarr(nxf) & ioyf=fltarr(nyf) & iozf=fltarr(nzf)
  time=0.0 & fnumber=1
  fgroup='1' & group='1'
  name='' & wsmooth='' & again='y' & withps='n' & run=''  
  closeps='n' & jnew='y' & newplot='a' & coltab='o'
  plane='x' & whatcut='x' & choice='f' & igrid=21
  jnew= 'y' & enew= 'y' & plform='p' 
  names=strarr(15)
  names=replicate(' ',15)
   ctab=findgen(nxf)
   cbary=fltarr(2,nxf)

;-----------PARAMETER----------------
  xmin = -8. & ymin = -50  & zmin = 0
  xmax =  8. & ymax = 50   & zmax = 120
  
  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
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

  fgroup = '3' 
  head1='Magn. Field Bx' & head2='Magn. Field By'
  head3='Magn. Field Bz' & imt='b'
  f1=bx & f2=by & f3=bz & p=2*u^(5.0/3.0) & f4=p 
  
    choice = 'x' 
    plane='x' & nplane=nx & coord=x 
    if (igrid lt 0) or (igrid ge nplane) then igrid=nplane/2
    cutata='x=' & cutatb=string(x(igrid),'(f5.1)')
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

menu:
        if withps eq 'y' then  begin 
           print, 'close postscript device? <y> or <n>'
           read, closeps
        endif
        if closeps eq 'y' then begin
            withps = 'n' & closeps='n'
            device,/close
            set_plot,'x'
            !P.THICK=1.
        endif
        if (withps eq 'y') and closeps ne 'y' then $
          print, 'postscript device is still open!'
  print, plane, ' Coordinates:
  for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Options:   <integer> -> grid index'
  print, '                 <x> -> y,z-plane, x=const'
  print, '                 <y> -> z,x-plane, y=const'
  print, '                 <z> -> x,y-plane, z=const'
  print, '                 <a> -> contour plots'
  print, '                 <b> -> arrow plots'
  print, '            <return> -> no changes applied'
  print, '                 <f> -> choose fields'
  print, '                 <g> -> gif output'
  print, '                 <p> -> postscript output'
  print, '                 <r> -> close postscript'
  print, '                 <c> -> load colortable'
  print, '                 <o> -> use original colortables'
  print, '                 <s> -> smooth grid oscillations'
  print, '                 <t> -> no smoothing'
  print, '                 <q> -> terminate'
  print, 'Present Choice: ', igrid
  read, choice 
  if choice eq 'q' then stop
  if choice eq 'x' then plane='x' 
  if choice eq 'y' then plane='y' 
  if choice eq 'z' then plane='z' 
  if choice eq 'a' then newplot='a'
  if choice eq 'b' then newplot='b'
  if choice eq '' then if newplot eq 'a' then newplot='b' else newplot='a' 
  if (choice ne '') and (choice ne 'f') and (choice ne 'g') $ 
     and (choice ne 'x') and (choice ne 'y') and (choice ne 'z') $ 
     and (choice ne 'a') and (choice ne 'b')  $ 
     and (choice ne 'p') and (choice ne 'r')  $ 
     and (choice ne 'c') and (choice ne 'o') then igrid=fix(choice)
  if (igrid lt 0) or (igrid ge nplane) then igrid=nplane/2
  if choice eq 'c' then begin
     xloadct
     coltab='c'
     goto, menu
  endif
  if choice eq 'o' then coltab='o'
  if choice eq 'g' then begin
    if (withps eq 'n') then begin
           tvlct,r,g,b,/get
           imag=tvrd()
           write_gif,'im'+imt+newplot+'_t'+string(time,'(i3.3)')+'_'+plane+ $
                      string(igrid,'(i3.3)')+'.gif',imag, r,g,b
    endif
    if (withps ne 'n') then print, 'switch off postscript output'
    goto, menu
  endif
  if choice eq 'p' then begin 
    withps = 'y'
    set_plot,'ps'
    device,filename= 'im'+imt+newplot+'_t'+string(time,'(i3.3)')+'_'+plane+$
                      string(igrid,'(i3.3)')+'.ps'
    !P.THICK=2.
    if abs(xpmax-xpmin) ge abs(ypmax-ypmin) then begin 
          device,/portrait
          device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5 
          device,/inches,ysize=10.,scale_factor=1.0,yoffset=0.5
    endif
    if abs(xpmax-xpmin) lt abs(ypmax-ypmin) then device,/landscape
  endif

  if choice eq 'r' then begin
     withps = 'n' & closeps='n'
     device,/close
     set_plot,'x'
     !P.THICK=1.
     goto, menu
  endif  
  
  if choice eq 's' then wsmooth='s'
  if choice eq 't' then wsmooth=''

if choice eq 'f' then begin
  print, 'Input Fieldgroup:'
  print, 'Options: 1 - rho, p'
  print, '         2 - velocity, v'
  print, '         3 - magnetic field b'
  print, '         4 - current density'
  print, '         5 - electric field'
  print, '         6 - res, ptot'
  print, '         7 - j_parall, e_parall'
  print, '         q - terminate'
  print, '    return - no changes applied'
  print, 'Present Choice: ', fgroup
  read, group & if group ne '' then fgroup=group
  if group eq '' then print, 'present choice=',group,' not altered'
  if fgroup eq 'q' then stop
  if fgroup eq '1' then begin
    head1='Density' & head2='Pressure' & imt='rp'
    f1=rho & f2=2*u^(5.0/3.0) & endif
  if fgroup eq '2' then begin
    head1='Vel. Vx' & head2='Vel. Vy' & head3='Vel. Vz' & imt='v'
    f1=sx/rho & f2=sy/rho & f3=sz/rho & endif 
  if fgroup eq '3' then begin
    head1='Magn. Field Bx' & head2='Magn. Field By' & imt='b'
    head3='Magn. Field Bz'
    f1=bx & f2=by & f3=bz & endif 
  if (fgroup eq '4') or (fgroup eq '5') or (fgroup eq '6') $
     or (fgroup eq '7') then begin $
    head1='Curr. Dens. Jx' & head2='Curr. Dens. Jy' & imt='j'
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
    head3='Electr. Field Ez' & imt='e'
    if enew eq 'y' then begin
      ex=(sy*bz-sz*by)/rho+res*jx
      ey=(sz*bx-sx*bz)/rho+res*jy
      ez=(sx*by-sy*bx)/rho+res*jz 
      enew='n'
    endif
    f1=ex & f2=ey & f3=ez 
  endif
  if fgroup eq '6' then begin
    head1='Resistivity' & head2='Ptot' & imt='respt'
       f1 = res
       f2=2*u^(5.0/3.0)+bx*bx+by*by+bz*bz
  endif
  if fgroup eq '7' then begin
    head1='E parallel' & head2='J parallel' & imt='epjp'
       f3 = sqrt(bx*bx+by*by+bz*bz)
       f2 = (jx*bx+jy*by+jz*bz)/f3
       f1= res*f2
  endif
  
endif

  if plane eq 'x' then begin 
    plane='x' & nplane=nx & coord=x 
    if (igrid lt 0) or (igrid ge nplane) then igrid=nplane/2
    cutata='x=' & cutatb=string(x(igrid),'(f5.1)')
    xtit='y'    & ytit='z'    & xchoice=y   & ychoice=z
    xpmin=ymin  & xpmax=ymax  & ypmin=zmin  & ypmax=zmax 
    if (igrid lt 0) or (igrid ge nplane) then igrid=nplane/2
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
    plane='y' & nplane=ny & coord=y 
    cutata='y=' & cutatb=string(y(igrid),'(f5.1)')
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
    plane='z' & nplane=nz & coord=z 
    if (igrid lt 0) or (igrid ge nplane) then igrid=nplane/2
    cutata='z=' & cutatb=string(z(igrid),'(f5.1)')
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
  
  if abs(xpmax-xpmin) ge abs(ypmax-ypmin) then plform='p' else plform='l' 

  
  xpos =xpmax+0.04*(xpmax-xpmin)
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
     breit=1.25*abs(xpmax-xpmin)/abs(ypmax-ypmin)*hoch else breit=.75
      !P.MULTI=[0,0,3]
      pos1=[0.1,0.05,0.1+breit,0.05+hoch]
      pos2=[0.1,0.38,0.1+breit,0.38+hoch]
      pos3=[0.1,0.71,0.1+breit,0.71+hoch]
  endif
  if abs(xpmax-xpmin) lt abs(ypmax-ypmin) then begin 
   if abs(ypmax-ypmin)/abs(xpmax-xpmin) gt 1.6 then $
                              breit=0.2 else breit=0.25
   if abs(ypmax-ypmin)/abs(xpmax-xpmin) lt 2.4 then $
     hoch=1.25*abs(ypmax-ypmin)/abs(xpmax-xpmin)*breit else hoch=.75
      !P.MULTI=[0,3,0]
    hoch =1.1*hoch & breit=1.1*breit
    xleft=0.07 & xinter=0.11 & xinter1=0.065 & hop=0.025 
    xa1=xleft & xe1=xleft+hop  
    xa2=xe1+xinter1 & xe2=xa2+breit
    xa3=xe2+xinter & xe3=xa3+breit
    xa4=xe3+xinter1 & xe4=xa4+hop
    ylo=0.08 & yup=ylo+hoch
      
      pos1=[xa1,ylo,xe1,yup]
      pos2=[xa2,ylo,xe2,yup]
      pos3=[xa3,ylo,xe3,yup]
      pos3a=[xa3-0.5*hop,ylo,xe3-0.5*hop,yup]
      pos4=[xa4,ylo,xe4,yup]
      pos4a=[xa4+hop,ylo,xe4+hop,yup]
      
  xposxt =xpmin+0.6*(xpmax-xpmin)
  yposxt =ypmin-0.035*(ypmax-ypmin)/hoch
  xposyt =xpmin-0.035*(xpmax-xpmin)/breit
  yposyt =ypmin+0.6*(ypmax-ypmin)
      
  endif
  
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,6,0,0,0]
       !P.CHARSIZE=1.7
       !P.FONT=3
       !X.TICKS=4
       !Y.TICKS=4
       !X.THICK=2
       !Y.THICK=2
       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ypmin,ypmax]
    if newplot eq 'b' then goto, plotb


plota:  
;   PLOTA
    if coltab eq 'o' then setcol,'n'
    
  if (fgroup eq '1') or (fgroup eq '6') or (fgroup eq '7') then begin 
     fp1=fa & fp2=fb 
  endif else begin
    if plane eq 'x' then begin 
      if fgroup eq '2' then begin
        head1='Velocity Vy' & head2='Velocity Vz' & endif
      if fgroup eq '3' then begin
        head1='Magn. Field By' & head2='Magn. Field Bz' & endif
      if fgroup eq '4' then begin
        head1='Curr. Dens. Jy' & head2='Curr. Dens. Jz' & endif
      if fgroup eq '5' then begin
        head1='Electric Field Ey' & head2='Electric Field Ez' & endif
      fp1=fb
      fp2=fc
    endif
    if plane eq 'y' then begin 
      if fgroup eq '2' then begin
        head1='Velocity Vx' & head2='Velocity Vz' & endif
      if fgroup eq '3' then begin
        head1='Magn. Field Bx' & head2='Magn. Field Bz' & endif
      if fgroup eq '4' then begin
        head1='Curr. Dens. Jx' & head2='Curr. Dens. Jz' & endif
      if fgroup eq '5' then begin
        head1='Electric Field Ex' & head2='Electric Field Ez' & endif
      fp1=fa
      fp2=fc
    endif
    if plane eq 'z' then begin 
      if fgroup eq '2' then begin
        head1='Velocity Vx' & head2='Velocity Vy' & endif
      if fgroup eq '3' then begin
        head1='Magn. Field Bx' & head2='Magn. Field By' & endif
      if fgroup eq '4' then begin
        head1='Curr. Dens. Jx' & head2='Curr. Dens. Jy' & endif
      if fgroup eq '5' then begin
        head1='Electric Field Ex' & head2='Electric Field Ey' & endif
      fp1=fa
      fp2=fb
    endif
  endelse


    if wsmooth eq 's' then fp1=smooth(fp1,3) 
    fmax=max(fp1) & fmin=min(fp1)
    
    nl=9
;   colorbar        
    !P.POSITION=pos1
    
    if (head1 eq 'Resistivity') or (head1 eq 'E parallel') then $
      ytickf='(f5.3)' else ytickf='(f5.2)' 
    ddel=(fmax-fmin) & del=ddel/float(nxf-1)
    ctab=findgen(nxf) & ctab=del*ctab+fmin
    cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
    IMAGE_C, cbary
    contour,cbary,[0,1],ctab,levels=findgen(nl)*ddel/(nl-1.)+fmin,$
    c_linestyle=1,xstyle=1,ystyle=1,$
    xrange=[0,1],yrange=[fmin,fmax],$
    xtickname=names,xticks=1,ytickformat=ytickf,/noerase
    
    
    fpmax=max(fp1) & fpmin=min(fp1) & hh=fpmax*fpmin/(fpmax-fpmin)^2
;    fptr=0.8
    print, 'hh, fmin,fmax,:', head1,hh,fpmin,fpmax
    if (hh lt -0.1) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/(nl-1.) & if delf lt 0.00000001 then delf=0.0001

    !P.POSITION=pos2  
    if plane eq 'y' then IMAGE_C,rotate(fp1,5) else IMAGE_C,fp1
        
    if fpmax ne fpmin then begin
    contour,fp1,xsu,ysu,levels=findgen(nl)*delf+fpmin,$
    c_linestyle=1,$
;    c_linestyle=findgen(nl)*delf+fpmin lt fptr,$
    title=head1,xstyle=1,ystyle=1
    xyouts,xposxt,yposxt,xtit 
    xyouts,xposyt,yposyt,ytit
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
;    xyouts,xpos,ypos3,'Max=' 
;    xyouts,xpos,ypos4,+string(fpmax,'(f6.3)') 
;    xyouts,xpos,ypos5,'Min=' 
;    xyouts,xpos,ypos6,+string(fpmin,'(f6.3)') 
   endif

    !P.POSITION=pos3  
    if wsmooth eq 's' then fp2=smooth(fp2,3) 
    if plane eq 'y' then IMAGE_C,rotate(fp2,5) else IMAGE_C,fp2
    
    fpmax=max(fp2) & fpmin=min(fp2) & hh=fpmax*fpmin/(fpmax-fpmin)^2
;    fptr=1.9
    print, 'hh, fmin,fmax,:', head2,hh,fpmin,fpmax
    if (hh lt -0.1) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/(nl-1) & if delf lt 0.00000001 then delf=0.0001
   if fpmax ne fpmin then begin
    contour,fp2,xsu,ysu,levels=findgen(nl)*delf+fpmin,$
    c_linestyle=1,$
;    c_linestyle=findgen(nl)*delf+fpmin lt fptr,$
    title=head2,xstyle=1,ystyle=1
    xyouts,xposxt,yposxt,xtit 
    xyouts,xposyt,yposyt,ytit
;    xyouts,xpos,yposa,cutata 
;    xyouts,xpos,yposb,cutatb 
;    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
;    xyouts,xpos,ypos2,' '+run 
;    xyouts,xpos,ypos3,'Max=' 
;    xyouts,xpos,ypos4,+string(fpmax,'(f6.3)') 
;    xyouts,xpos,ypos5,'Min=' 
;    xyouts,xpos,ypos6,+string(fpmin,'(f6.3)') 
   endif
   
;       colorbar        
	!P.POSITION=pos4
	fmax=fpmax & fmin=fpmin
	ddel=(fmax-fmin) & del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+fmin
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        IMAGE_C, cbary
	contour,cbary,[0,1],ctab,levels=findgen(nl)*ddel/(nl-1.)+fmin,$
	xstyle=1,ystyle=1,$
        c_linestyle=1,xrange=[0,1],yrange=[fmin,fmax],$
        xtickname=names,xticks=1,ytickformat='(f5.2)',/noerase
   


  goto, menu


plotb:  
;   PLOTB
  if (fgroup eq '1') or (fgroup eq '6') or (fgroup eq '7') then begin
    print, 'NO VECTORPLOT'
    goto, menu
  endif
  
    if coltab eq 'o' then setcol
    
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
      fp2=fa 
      f0=fc1
      far1=interpolate(fc2,ioy,ioz,/grid)
      far2=interpolate(fc3,ioy,ioz,/grid)
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
      fp2=fb 
      f0=fc2
      far1=interpolate(fc1,iox,ioz,/grid)
      far2=interpolate(fc3,iox,ioz,/grid)
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
      xar=xn & yar=yn 
      fp2=fc 
      f0=fc3 
      far1=interpolate(fc1,iox,ioy,/grid)
      far2=interpolate(fc2,iox,ioy,/grid)
    endif

    !P.REGION=[0.,0.,1.0,1.0]
    !P.CHARSIZE=2.0
    !P.MULTI=[0,6,0,0,0]
    

    if wsmooth eq 's' then fp1=smooth(f0,3) else fp1=f0 
    !P.POSITION=pos1
;       colorbar        
    nl=9 
    fpmax=max(fp1) & fpmin=min(fp1)
    fmax=fpmax & fmin=fpmin
        ddel=(fmax-fmin) & del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+fmin
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        IMAGE_C, cbary
	contour,cbary,[0,1],ctab,levels=findgen(nl)*ddel/(nl-1.)+fmin,$
	c_linestyle=1,xstyle=1,ystyle=1,$
        xrange=[0,1],yrange=[fmin,fmax],$
        xtickname=names,xticks=1,ytickformat='(f5.2)',/noerase
    !P.POSITION=pos4a
        IMAGE_C, cbary
	contour,cbary,[0,1],ctab,levels=findgen(nl)*ddel/(nl-1.)+fmin,$
	c_linestyle=1,xstyle=1,ystyle=1,$
        xrange=[0,1],yrange=[fmin,fmax],$
        xtickname=names,xticks=1,ytickformat='(f5.2)',/noerase
    
    !P.POSITION=pos2
    if plane eq 'y' then IMAGE_C,rotate(fp2,5) else IMAGE_C,fp2
    
    fpmax=max(fp1) & fpmin=min(fp1)
    if (fpmax*fpmin lt 0.0) then fptr=0.0 else fptr=0.5*(fpmax+fpmin)
    delf=(fpmax-fpmin)/(nl-1.) & if delf lt 0.000000001 then delf=0.0001
    if fpmax ne fpmin then begin
    contour,fp1,xchoice,ychoice,levels=findgen(nl)*delf+fpmin,$
;    c_linestyle=findgen(nl)*delf+fpmin lt fptr,$
    c_linestyle=1,$
    title=head4,xstyle=1,ystyle=1
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xposxt,yposxt,xtit 
    xyouts,xposyt,yposyt,ytit
   endif

    !P.POSITION=pos3a
    if plane eq 'y' then IMAGE_C,rotate(fp2,5) else IMAGE_C,fp2
    
    fpmax=sqrt(max(far1^2+far2^2))
    vect, far1, far2, xar, yar, length=1.2,$
    title=head5
;    xyouts,xpos,yposa,cutata 
;    xyouts,xpos,yposb,cutatb 
;    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
;    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,+string(fpmax,'(f5.2)') 
    xyouts,xposxt,yposxt,xtit 
    xyouts,xposyt,yposyt,ytit


  goto, menu



end



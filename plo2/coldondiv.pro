; MAIN PROGRAM
;   program reads data from 2D simulations(x/y) 
;   and substitutes the simulation y direction with 
;   z for plotting data from MP simulations
;      PLOT        SIMULATION
;     -------       -------
; -x !       !    y!       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;     ------->      ------->
;           y             x

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

; GRID FOR VELOCITY VECTORS
   nxn = 21   &   nyn = 21
   fn1=fltarr(nxn,nyn) & fn2=fn1
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 201   &   nyf = 201
   fa=fltarr(nxf,nyf) & fb=fa
   
;----PARAMETER-------
  xmin = -14. & ymin = -20.0
  xmax =  10. & ymax =  20.0
  xmin0 = xmin & ymin0 = ymin
  xmax0 = xmax & ymax0 = ymax
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & closeps='n'
   run='' & plot='0'  & coltab='o' &  withgif = 'n' &  contonly = 'n'
   glatt='n' & colps ='n' & grayplot ='n' 
   names=strarr(15) & names=replicate(' ',15)
   nl1=17  &  nl2=9  &  phi=25.0  &  pi = 3.14159265536  &  phir = phi*pi/180.0
   xtit='y' & ytit='-x' & smo='n'   & nsh=0  & nshtot=nsh  &  galpar='a'
   nx=long(103) & ny=long(103) & orient='l'
   wlong=900 & wshort=720 & wfact=1.0 &  wxf=1.0 & wyf=1.0 & psfact=0.9
   wsize=[700,900] & wsold=[0,0] & wino=0 & srat0=1.1

; READ INPUT DATA OF DIMENSION NX, NY
read2d, g1,g2,g3,h1,h2,h3, bx,by,bz, vx,vy,vz, rho,u,res
   bsq=bx & p=bx & jz=bx & ez=bx
   bzpro=bx & vzpro=bx & vypro=bx
   a=fltarr(nx,ny) 

;   print, 'Which case?'
;   read, run

   testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
;   xpmin=xmax  &  xpmax=xmin
   xpmin=xmin  &  xpmax=xmax
   
; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
jande, nx,ny,g1,h1,bx,by,vx,vy,res,jz,ez

; Plasma compression (-div v)

   divv=rho & f1=shift(vx,-1,0)-shift(vx,1,0)
              f2=shift(vy,0,-1)-shift(vy,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   divv=bz*(f1+f2)
   divv(0,*)=0. & divv(nx-1,*)=0. & divv(*,0)=0. & divv(*,ny-1)=0.

   bvz=rho & f1=shift(vz,-1,0)-shift(vz,1,0)
             f2=shift(vz,0,-1)-shift(vz,0,1)
   for j=0,ny-1 do f1(*,j) = bx(*,j)*g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = by(i,*)*h1*f2(i,*)
   bvz=f1+f2 & bvz(0,*)=0. & bvz(nx-1,*)=0. & bvz(*,0)=0. & bvz(*,ny-1)=0.

   bbz=rho & f1=shift(bz,-1,0)-shift(bz,1,0)
             f2=shift(bz,0,-1)-shift(bz,0,1)
   for j=0,ny-1 do f1(*,j) = bx(*,j)*g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = by(i,*)*h1*f2(i,*)
   bbz=f1+f2 & bbz(0,*)=0. & bbz(nx-1,*)=0. & bbz(*,0)=0. &
             bbz(*,ny-1)=0.

   vdotb=vx*bx+vy*by
   divv=smooth(divv,3) & bvz=smooth(bvz,3) & bbz=smooth(bbz,3)
 

 
; VECTORPOTENTIAL:
vecpot, nx,ny,x,y,bx,by,a,fmin,fmax
    print, 'vectorpotential:',fmax, fmin
    
    y=y(1:ny-2)
    rho=rho(*,1:ny-2) & u=u(*,1:ny-2)   & res=res(*,1:ny-2)
    vx=vx(*,1:ny-2)   & vy=vy(*,1:ny-2) & vz=vz(*,1:ny-2) 
    bx=bx(*,1:ny-2)   & by=by(*,1:ny-2) & bz=bz(*,1:ny-2)
    jz=jz(*,1:ny-2)   & ez=ez(*,1:ny-2)
    divv=divv(*,1:ny-2)  & bvz=bvz(*,1:ny-2)  & bbz=bbz(*,1:ny-2)
    vdotb=vdotb(*,1:ny-2)
    p=2*u^(5.0/3.0)   & bsq=by*by+bx*bx+bz*bz
    ny=ny-2
    
; GRID FOR VELOCITY VECTORS 
grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn

; GRID FOR CONTOUR/SURFACE PLOTS
grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf

rotconf, by,bz,bypro,bzpro,phi  
rotconf, vy,vz,vypro,vzpro,phi  

asymval, nx,ny,vypro,vy1,vy2,vyav  
 print, 'vy_msp_orig = ', vy1,'  vy_msh_orig = ',vy2,'   vyav = ',vyav
 v_ymsp = vy1
asymval, nx,ny,vzpro,vz1,vz2,vzav  
 print, 'vz_msp_orig = ', vz1,'  vz_msh_orig = ',vz2,'   vzav = ',vzav

;   f1=a(*,0:ny-2)   & a(*,0:ny-2)=shift(f1,0,nsh)   & a(*,ny-1)=a(*,0)
   f1=rho(*,0:ny-2) & rho(*,0:ny-2)=shift(f1,0,nsh) & rho(*,ny-1)=rho(*,0)
   f1=u(*,0:ny-2)   & u(*,0:ny-2)=shift(f1,0,nsh)   & u(*,ny-1)=u(*,0)
   f1=divv(*,0:ny-2) & divv(*,0:ny-2)=shift(f1,0,nsh) & divv(*,ny-1)=divv(*,0)
   f1=bvz(*,0:ny-2) & bvz(*,0:ny-2)=shift(f1,0,nsh) & bvz(*,ny-1)=bvz(*,0)
   f1=bbz(*,0:ny-2) & bbz(*,0:ny-2)=shift(f1,0,nsh) & bbz(*,ny-1)=bbz(*,0)
   f1=vdotb(*,0:ny-2) & vdotb(*,0:ny-2)=shift(f1,0,nsh) 
     vdotb(*,ny-1)=vdotb(*,0)
   f1=vx(*,0:ny-2)  & vx(*,0:ny-2)=shift(f1,0,nsh)  & vx(*,ny-1)=vx(*,0)
   f1=vy(*,0:ny-2)  & vy(*,0:ny-2)=shift(f1,0,nsh)  & vy(*,ny-1)=vy(*,0)
   f1=vz(*,0:ny-2)  & vz(*,0:ny-2)=shift(f1,0,nsh)  & vz(*,ny-1)=vz(*,0)
   f1=bx(*,0:ny-2)  & bx(*,0:ny-2)=shift(f1,0,nsh)  & bx(*,ny-1)=bx(*,0)
   f1=by(*,0:ny-2)  & by(*,0:ny-2)=shift(f1,0,nsh)  & by(*,ny-1)=by(*,0)
   f1=bz(*,0:ny-2)  & bz(*,0:ny-2)=shift(f1,0,nsh)  & bz(*,ny-1)=bz(*,0)
   f1=jz(*,0:ny-2)  & jz(*,0:ny-2)=shift(f1,0,nsh)  & jz(*,ny-1)=jz(*,0)
   f1=ez(*,0:ny-2)  & ez(*,0:ny-2)=shift(f1,0,nsh)  & ez(*,ny-1)=ez(*,0)
   f1=p(*,0:ny-2)   & p(*,0:ny-2)=shift(f1,0,nsh)   & p(*,ny-1)=p(*,0)
   f1=bsq(*,0:ny-2) & bsq(*,0:ny-2)=shift(f1,0,nsh) & bsq(*,ny-1)=bsq(*,0)
   f1=bzpro(*,0:ny-2) & bzpro(*,0:ny-2)=shift(f1,0,nsh) 
     bzpro(*,ny-1)=bzpro(*,0)
   f1=vypro(*,0:ny-2) & vypro(*,0:ny-2)=shift(f1,0,nsh)
     vypro(*,ny-1)=vypro(*,0)
   f1=vzpro(*,0:ny-2) & vzpro(*,0:ny-2)=shift(f1,0,nsh)
     vzpro(*,ny-1)=vzpro(*,0)
   vecpot, nx,ny,x,y,bx,by,a,fmin,fmax
      print, 'vectorpotential:',fmax, fmin


;    a=shift(a,0,nsh) & rho=shift(rho,0,nsh) & u=shift(u,0,nsh)
;    vx=shift(vx,0,nsh) & vy=shift(vy,0,nsh) & vz=shift(vz,0,nsh)
;    bx=shift(bx,0,nsh) & by=shift(by,0,nsh) & bz=shift(bz,0,nsh)
;    jz=shift(jz,0,nsh) & ez=shift(ez,0,nsh) & bzpro=shift(bzpro,0,nsh) 
;    p=shift(p,0,nsh) & bsq=shift(bsq,0,nsh) 
;    vypro=shift(vypro,0,nsh) & vzpro=shift(vzpro,0,nsh) 

;   !P.REGION=[0.,0.,1.0,1.25]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.MULTI=[0,5,0,0,0]
  !P.CHARSIZE=1.5
  !P.FONT=3
  !P.THICK=1.
  !X.TICKS=3
  !Y.TICKS=4
  !Y.TICKlen=0.04
  !X.THICK=2
  !Y.THICK=2

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
  print, 'Last plot:',plot
  print, 'Input:'
  print, 'Options: <return> -> next plot, plasma velocity;'
  print, '         <1> -> 1. plot, plasma velocity;'
  print, '         <2> -> 2. plot, magnetic field;'
  print, '         <3> -> 3. plot, density and temperature;'
  print, '         <4> -> 4. plot, thermal and magnetic pressure;'
  print, '         <5> -> 5. plot, total pressure and current density;'
  print, '         <6> -> 6. plot, normal velocity and magnetic field;'
  print, '         <s> -> smooth data'
  print, '         <t> -> switch off smoothing'
  print, '         <w> -> change window size'
  print, '         <x> -> new boundaries in x'
  print, '         <y> -> new boundaries in y'
  print, '         <z> -> original boundaries'
  print, '         <a> -> rotation angle to obtain MSP coordinates'
  print, '         <v> -> gallilei transformation of the velocity'
  print, '                V_ymsp =',v_ymsp
  print, $
        '         <i> -> shift no of gridspacings along y (periodic in y only)'
  print, '         <o> -> scale postscipt output'
  print, '         <p> -> postscript for present plot'
  print, '         <r> -> close postscript'
  print, '         <n> -> plot ID'
  print, '         <g> -> output on con*.gif'
  print, '         <k> -> contour plot only'
  print, '         <l> -> switch to color'
  print, '         <e> -> switch to color postscript'
  print, '         <f> -> switch to greyscale postscript (default)'
  print, '         <c> -> load colortable'
  print, '         <o> -> use original colortables'
  print, '         <q> -> terminate'
  read, contin
  print, 'contin:',contin
  if contin eq 'q' then stop
  if contin eq '' then begin
      iplot=fix(plot)+1 & print,'iplot:',iplot
      if (iplot gt 7) or (iplot lt 1) then iplot=1 & print,'iplot:',iplot
      plot=string(iplot,'(i1)') & print,'plot:',plot 
  endif
  if (contin eq '1') or (contin eq '2') or (contin eq '3') or $
     (contin eq '4') or (contin eq '5') or $
     (contin eq '6') or (contin eq '7') then plot=contin
  if contin eq 's' then  smo = 'y'
  if contin eq 't' then  smo = 'n'
  if contin eq 'w' then begin
     print, '  input factor to re-scale window'
     read, wfact
  endif
  if contin eq 'x' then begin
     print, '  input xmin and xmax'
     read, xmin, xmax
     testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
     xpmin=xmin  &  xpmax=xmax
;     xpmin=xmax  &  xpmax=xmin
;     if (ytit eq 'y') then begin
;       xpmin=xmin  &  xpmax=xmax
;     endif
  endif
  if contin eq 'y' then begin
     print, '  input ymin and ymax'
     read, ymin, ymax
     testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  endif
  if contin eq 'z' then begin
     xmin = xmin0 & ymin = ymin0
     xmax = xmax0 & ymax = ymax0
     testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  endif
  if contin eq 'a' then begin
     print, '  input rotation angle phi'
     read, phi
     rotconf, by,bz,bypro,bzpro,phi  
     rotconf, vy,vz,vypro,vzpro,phi  
  endif
  if contin eq 'v' then begin
    asymval, nx,ny,vypro,vy1,vy2,vyav  
    print, 'vy_msp_orig = ', vy1,'  vy_msh_orig = ',vy2,'   vyav = ',vyav
    asymval, nx,ny,vzpro,vz1,vz2,vzav  
    asymval, nx,ny,rho,rho1,rho2,rhoav  
    mom1=vy1*rho1 & mom2=vy2*rho2
    print, 'vz_msp_orig = ', vz1,'  vz_msh_orig = ',vz2,'   vzav = ',vzav
     print, '  input option:'
     print, '        (a): centerd velocity frame '
     print, '        (b): centerd momentum frame frame'
     print, '        (any number) = y velocity on magnetosph side (orig coord.)'
     read, galpar
     if galpar eq 'a' then begin
       vypro = vypro - vy1 + 0.5*(vy1-vy2)
       vzpro = vzpro - vz1 + 0.5*(vz1-vz2)
       vy=vypro*cos(phir)+vzpro*sin(phir) & vz=-vypro*sin(phir)+vzpro*cos(phir)
     endif
     if galpar eq 'b' then begin
       delvy = (vy1*rho1+vy2*rho2)/(rho1+rho2)
       delvz = (vz1*rho1+vz2*rho2)/(rho1+rho2)
       vypro = vypro - delvy
       vzpro = vzpro - delvz
       vy=vypro*cos(phir)+vzpro*sin(phir) & vz=-vypro*sin(phir)+vzpro*cos(phir)
     endif
     if (galpar ne 'a') and (galpar ne 'b') then begin
       v_ymsp = float(galpar)
       vypro = vypro - vy1 + v_ymsp
       vy=vypro*cos(phir)+vzpro*sin(phir) & vz=-vypro*sin(phir)+vzpro*cos(phir)
     endif
  endif
  if contin eq 'e' then colps='y'
  if contin eq 'f' then colps='n'
  if contin eq 'k' then contonly='y'
  if contin eq 'l' then contonly='n'
  if contin eq 'n' then begin
     print, 'input plot id to append to ps/gif files '
     read, run
  endif
  if contin eq 'p' then begin
      withps = 'y' 
      if contonly eq 'n' then begin 
        set_plot,'ps' & ncol=!D.TABLE_SIZE & print,!D.TABLE_SIZE
        !p.color=0
        if colps eq 'n' then $
          device,/color,bits_per_pixel=8,$
             filename='conp'+plot+string(time,'(i3.3)')+run+'g.ps'$
        else $
          device,/color,bits_per_pixel=8,$
             filename='conp'+plot+string(time,'(i3.3)')+run+'col.ps'
      endif
      if contonly eq 'y' then begin 
        set_plot,'ps' 
        device,bits_per_pixel=2,$
             filename='conp'+plot+string(time,'(i3.3)')+run+'c.ps'
      endif
      if srat gt srat0 then  begin
        device,/landscape
        device,/inches,xsize=10.,scale_factor=1.0,xoffset= 0.3
        device,/inches,ysize=8.0,scale_factor=1.0,yoffset=10.25
      endif
      if srat le srat0 then  begin
        device,/portrait
        device,/inches,xsize=8.,scale_factor=1.0,xoffset= 0.2
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.75
      endif
      !P.THICK=2.
;        device,/times,/bold,font_index=3
  endif
  if contin eq 'r' then begin
     withps = 'n' & closeps='n'
     device,/close
     set_plot,'x'
     !P.THICK=1.
     goto, menu
  endif
  if contin eq 'i' then begin
     print, 'present total shift is ', nshtot, ' gridpoints, from ny =',ny
     print, 'input relative shift in y by no of gridpoints'
     read, nsh
      nshtot = nshtot+nsh
      a=shift(a,0,nsh) & rho=shift(rho,0,nsh) & u=shift(u,0,nsh)
      vx=shift(vx,0,nsh) & vy=shift(vy,0,nsh) & vz=shift(vz,0,nsh)
      bx=shift(bx,0,nsh) & by=shift(by,0,nsh) & bz=shift(bz,0,nsh)
      jz=shift(jz,0,nsh) & ez=shift(ez,0,nsh) & bzpro=shift(bzpro,0,nsh) 
      p=shift(p,0,nsh) & bsq=shift(bsq,0,nsh) 
      vypro=shift(vypro,0,nsh) & vzpro=shift(vzpro,0,nsh) 
      divv=shift(divv,0,nsh) & bvz=shift(bvz,0,nsh)
        bbz=shift(bbz,0,nsh) & vdotb=shift(vdotb,0,nsh)

  endif
  if contin eq 'c' then begin
     xloadct
     coltab='c'
     goto, menu
  endif
  if contin eq 'o' then coltab='o'
  if contin eq 'g' then begin
    if (withps eq 'n') then begin
           tvlct,r,g,b,/get
           imag=tvrd()
           write_gif, 'conp'+plot+string(time,'(i3.3)')+run+'.gif',imag, r,g,b
    endif
    if (withps ne 'n') then print, 'switch off postscript output'
    goto, menu
  endif

  !X.RANGE=[xpmin,xpmax]
  !Y.RANGE=[ymin,ymax]

; COORDINATES FOR PLOTS
   delx=xmax-xmin & dely=ymax-ymin & srat=(ymax-ymin)/(xmax-xmin)
   if srat gt srat0 then orient='l' else orient='p'
   wxf=1. & wyf=1.  & xyrat=1.25
   if orient eq 'l' then begin
     ytit1=''
     xtit0=xtit
     psize=0.3 & sb0=2.4 & sb1=4.0
     if (srat lt sb0) then  begin
       wyf=(srat/sb0)^(0.7)
       dpx=psize
       if withps eq 'y' then  dpy=xyrat*srat*psize
       if withps eq 'n' then  dpy=xyrat*psize*sb0*(srat/sb0)^(0.3)
     endif
     if (srat ge sb0) and (srat lt sb1) then  begin
       wxf=(sb0/srat)^(0.7)
       dpy=xyrat*sb0*psize
       if withps eq 'y' then dpx=psize*sb0/srat
       if withps eq 'n' then dpx=psize*(sb0/srat)^0.3 
     endif
     if (srat ge sb1) then  begin
       wxf=(sb0/sb1)^(0.7)
       dpy=xyrat*sb0*psize
       if withps eq 'y' then dpx=psize*sb0/sb1
       if withps eq 'n' then dpx=psize*(sb0/sb1)^0.3 
     endif
     if withps eq 'y' then begin
       dpx=psfact*dpx & dpy=psfact*dpy
     endif 
     print, srat, dpx, dpy
     xinter=0.05 & cbthick=0.1*dpx & cbdist1=0.045 & cbdist2=0.055
     if withps eq 'n' then begin
      xinter=xinter/wxf & cbthick=cbthick/wxf 
      cbdist1=cbdist1/wxf & cbdist2=cbdist2/wxf
     endif
     xleft=0.5-0.5*xinter-dpx 
     xa1=xleft                & xe1=xleft+dpx  
     xa2=xe1+xinter           & xe2=xa2+dpx
     xcb1=xa1-cbdist1-cbthick & xcb2=xe2+cbdist2
     ylo=0.065                & yup=ylo+dpy
     pos1=[xa1,ylo,xe1,yup]  & pos2=[xcb1,ylo,xcb1+cbthick,yup]
     pos3=[xa2,ylo,xe2,yup]  & pos4=[xcb2,ylo,xcb2+cbthick,yup]

     wsize=[wlong,wshort]  &  wsize=fix(wfact*wsize)
     wsize(0)=wxf*wsize(0) & wsize(1)=wyf*wsize(1)
     if (wsize(0) ne wsold(0)) or (wsize(1) ne wsold(1))  then $
       window,wino,xsize=wsize(0),ysize=wsize(1),title=run 
     wsold = wsize
   endif
     
   if orient eq 'p' then begin
     ytit1=ytit
     xtit0=''
     psize=0.4 & sb0=0.65 & sb1=0.3
     if (srat gt sb0) then  begin
       wxf=(sb0/srat)^(0.7)
       dpy=psize
       if withps eq 'y' then  dpx=psize*xyrat/srat
       if withps eq 'n' then  dpx=psize*xyrat/sb0*(sb0/srat)^(0.3)
     endif
     if (srat gt sb1) and (srat le sb0) then  begin
       wyf=(srat/sb0)^(0.7)
       dpx=psize*xyrat/sb0
       if withps eq 'y' then  dpy=psize/sb0*srat
       if withps eq 'n' then  dpy=psize*(srat/sb0)^(0.3)
     endif
     if (srat le sb1) then  begin
       dpx=psize*xyrat/sb0
       wyf=(sb1/sb0)^(0.7)
       if withps eq 'y' then  dpy=psize/sb0*sb1
       if withps eq 'n' then  dpy=psize*(sb1/sb0)^(0.3)
     endif
     if withps eq 'y' then begin
       dpx=psfact*dpx & dpy=psfact*dpy
     endif 
     xleft=0.08 & cbdist1=0.07 & cbthick=0.03*dpx  & yinter=0.05 
     if withps eq 'n' then begin
      yinter=yinter/wyf & cbthick=cbthick/wxf 
      cbdist1=cbdist1/wxf 
     endif
     ylo=0.5-0.5*yinter-dpy
     print, srat, dpx, dpy
     xa1=xleft & xe1=xleft+dpx  & xcb1=xe1+cbdist1
     ylo1=ylo & yup1=ylo+dpy & ylo2=yup1+yinter & yup2=ylo2+dpy 
     pos1=[xa1,ylo2,xe1,yup2]  & pos2=[xcb1,ylo2,xcb1+cbthick,yup2]
     pos3=[xa1,ylo1,xe1,yup1]  & pos4=[xcb1,ylo1,xcb1+cbthick,yup1]

     wsize=[wshort,wlong]  &  wsize=fix(wfact*wsize)
     wsize(0)=wxf*wsize(0) & wsize(1)=wyf*wsize(1)
     if (wsize(0) ne wsold(0)) or (wsize(1) ne wsold(1))  then $
       window,wino,xsize=wsize(0),ysize=wsize(1),title=run 
     wsold = wsize
   endif
   
   xpos=findgen(4)
   xpos(3)=xpmax-0.005*delx/dpx
   xpos(0)=xpmax+0.006*delx/dpx
   xpos(1)=xpmin-0.03*delx/dpx
   xpos(2)=xpmax-0.12*delx
   ypos=findgen(10)
   ypos(0)=ymin+0.2*dely            ; location for 'time'
   ypos(1)=ypos(0)-0.025*dely/dpy   ; next line
   ypos(2)=ypos(0)+0.075*dely/dpy   ; location 'CASE'
   ypos(3)=ymin+.85*dely             ; location 'Max='
   ypos(4)=ypos(3)-0.025*dely/dpy   ; next line
   ypos(5)=ymin+.65*dely    ; location 'Min='
   ypos(6)=ypos(5)-0.025*dely/dpy   ; next line
   ypos(7)=ymin+.9*dely
   ypos(8)=ymax+.02*dely
   ypos(9)=ymin-.05*dely

  grayplot=withps & if colps eq 'y' then grayplot='n'

  if plot eq '0' then plot='1'
  if plot eq '1' then goto, p1
  if plot eq '2' then goto, p2
  if plot eq '3' then goto, p3
  if plot eq '4' then goto, p4
  if plot eq '5' then goto, p5
  if plot eq '6' then goto, p6
  if plot eq '7' then goto, p7


p1:  
    !P.CHARSIZE=2.0
    !P.MULTI=[0,3,0,0,0]
    if coltab eq 'o' then setcol, grayplot
        
    fa=interpolate(vz,ioxf,ioyf,/grid) 
    fb=interpolate(a,ioxf,ioyf,/grid)
    fc=interpolate(bvz,ioxf,ioyf,/grid) 
    fn1=interpolate(vx,iox,ioy,/grid)
    fn2=interpolate(vy,iox,ioy,/grid)
    tit=' Plasma Velocity' & tit2='B grad Vz'  
   if contonly eq 'y' then begin
    contvec, fb,fn1,fn2,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fa,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    !P.MULTI=[0,5,0,0,0]
    vecplot, fa,fb,fn1,fn2,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fc,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
	
p2:
    !P.CHARSIZE=2.0
    !P.MULTI=[0,3,0,0,0]
    if coltab eq 'o' then setcol, grayplot

    fa=interpolate(bz,ioxf,ioyf,/grid) 
    fb=interpolate(a,ioxf,ioyf,/grid)
    fc=interpolate(bbz,ioxf,ioyf,/grid) 
    fn1=interpolate(bx,iox,ioy,/grid)
    fn2=interpolate(by,iox,ioy,/grid)
    tit=' Magnetic Field' & tit2='B grad Bz' 
   if contonly eq 'y' then begin
    contvec, fb,fn1,fn2,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fc,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    !P.MULTI=[0,5,0,0,0]
    vecplot, fa,fb,fn1,fn2,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fc,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p3:  
    !P.CHARSIZE=2.0
    !P.MULTI=[0,3,0,0,0]
    if coltab eq 'o' then setcol, grayplot
        
    fa=interpolate(vdotb,ioxf,ioyf,/grid) 
    fb=interpolate(a,ioxf,ioyf,/grid)
    fc=interpolate(-divv+bvz,ioxf,ioyf,/grid) 
    fn1=interpolate(vx,iox,ioy,/grid)
    fn2=interpolate(vy,iox,ioy,/grid)
    tit=' Plasma Velocity , V . B' & tit2='B grad Vz - Bz Div V'  
   if contonly eq 'y' then begin
    contvec, fb,fn1,fn2,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fa,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    !P.MULTI=[0,5,0,0,0]
    vecplot, fa,fb,fn1,fn2,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fc,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
	
        
p4:
    !P.CHARSIZE=2.0
    !P.MULTI=[0,4,0,0,0]
    if coltab eq 'o' then setcol, grayplot

    fa=interpolate(rho,ioxf,ioyf,/grid) 
    fb=interpolate(p/rho,ioxf,ioyf,/grid) 
    tit='Density' &  tit2='Temperature' 
   if contonly eq 'y' then begin
    contsca1, fa,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fb,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scaplot1, fa,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fb,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
        
p5:
    !P.CHARSIZE=2.0
    !P.MULTI=[0,4,0,0,0]
    if coltab eq 'o' then setcol, grayplot

    fa=interpolate(p,ioxf,ioyf,/grid) 
    fb=interpolate(bsq,ioxf,ioyf,/grid) 
    tit='Therm. Pressure' & tit2='Magn. Pressure'
   if contonly eq 'y' then begin
    contsca1, fa,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fb,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scaplot1, fa,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fb,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
        
p6:
    !P.CHARSIZE=2.0
    !P.MULTI=[0,4,0,0,0]
    if coltab eq 'o' then setcol, grayplot

    fa=interpolate(p+bsq,ioxf,ioyf,/grid) 
    fb=interpolate(jz,ioxf,ioyf,/grid) 
    tit='Total Pressure' & tit2='Current Density, J_z'
   if contonly eq 'y' then begin
    contsca1, fa,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fb,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scaplot1, fa,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fb,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p7:
    !P.CHARSIZE=2.0
    !P.MULTI=[0,4,0,0,0]
    if coltab eq 'o' then setcol, grayplot

    fa=interpolate(bx,ioxf,ioyf,/grid) 
    fb=interpolate(vx,ioxf,ioyf,/grid)
    tit='Magn Field, B_y' & tit2='Velocity, V_y'
   if contonly eq 'y' then begin
    contsca1, fa,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    contsca2, fb,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scaplot1, fa,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scaplot2, fb,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu


end


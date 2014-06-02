
; MAIN PROGRAM
;   program reads data from 2D simulations(x/y) 
;   and substitutes the simulation y direction with 
;   z for plotting data from MP simulations
;      PLOT        SIMULATION
;     -------       -------
;  z !       !    y!       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;     ------->      ------->
;          -x             x

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

; GRID FOR VELOCITY VECTORS
   nxn = 21   &   nyn = 21
   fn1=fltarr(nxn,nyn) & fn2=fn1
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 201   &   nyf = 201
   fa=fltarr(nxf,nyf) & fb=fa
   
;----PARAMETER-------
  xmin = -2. & ymin = -48.
  xmax =  2. & ymax =  48.
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & closeps='n'
   run='' & plot='0'  & coltab='o' &  withgif = 'n' &  contonly = 'n'
   glatt='n' & colps ='n' & grayplot ='n' 
   names=strarr(15) & names=replicate(' ',15)
   nl1=17  &  nl2=9  &  phi=0.0  &  pi = 3.14159265536  &  phir = phi*pi/180.0
   xtit='x' & ytit='z' & smo='n'   & nsh=0  & nshtot=nsh  &  galpar='a'
   nx=long(103) & ny=long(103) & orient='l' & aval=100.0
   wlong=900 & wshort=720 & wfact=1.0 &  wxf=1.0 & wyf=1.0 & psfact=0.9
   wsize=[700,900] & wsold=[0,0] & wino=0 & srat0=1.1

; READ INPUT DATA OF DIMENSION NX, NY
read2hall, g1,g2,g3,h1,h2,h3, bx,by,bz, vx,vy,vz,vex,vey,vez, rho,u,res
   bsq=bx & p=bx & jx=bx & jy=bx & jz=bx & ez=bx & divi=bx & dive=bx
   divni=bx & divne=bx & bdivvy=bx & divj=bx  & divjv=bx & lambda=bx
   f1=bx & f2=bx & bzpro=bx & vzpro=bx & vypro=bx 
   a=fltarr(nx,ny) 
   xmindd=1. &  xmaxdd=2.5 & xarg=y & proff=bx
   xamp=0.5*(xmaxdd-xmindd)  & xwidth=1. & xav=0.5*(xmindd+xmaxdd)
   xarg=xav+xamp*sin(pi*y/y(ny-2))
   for i=0, ny-1 do $
	 proff(*,i) = 0.5*( tanh( (x(*)-xarg(i))*xwidth/xarg(i) )$
                 -tanh( (x(*)+xarg(i))*xwidth/xarg(i) )$
                +2.*tanh(xwidth) )
      xmind=1. & xmaxd=2.5 & xamp=0.5*(xmaxd-xmind) & xav=0.5*(xmind+xmaxd)
      xwidth=6. & xw0=8. & dd0=3.
   for i=0, ny-1 do $
      lambda(*,i)  = 0.5*( tanh((x(*)+xw0)/dd0) - tanh((x(*)-xw0)/dd0) )
p=2*u^(5.0/3.0)
;   
;   res=res-0.8*proff

;   print, 'Which case?'
;   read, run

   testbd1, nx,ny,x,y,xmin,xmax,ymin,ymax   
   xpmin=xmax  &  xpmax=xmin
;   xpmin=xmin  &  xpmax=xmax
   
; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
jandeh1, nx,ny,g1,h1,bx,by,bz,vex,vey,res,jx,jy,jz,ez
divv, nx,ny,g1,h1,rho,bx,by,vx,vy,vex,vey,vez,divi,dive,divni,divne,bdivvy
jvx=rho*(vx-vex)/lambda & jvy=rho*(vy-vey)/lambda & jvz=rho*(vz-vez)/lambda
divcurr, nx,ny,g1,h1,jx,jy,divj
divcurr, nx,ny,g1,h1,jvx,jvy,divjv
divcurr, nx,ny,g1,h1,bx,by,divb
forces, nx,ny,g1,h1,bx,by,bz,jx,jy,jz,rho,p,fpx,fpy,fjx,fjy,fjz


; VECTORPOTENTIAL:
vecpotx, nx,ny,x,y,bx,by,a,fmin,fmax
    print, 'vectorpotential:',fmax, fmin
    
    y=y(1:ny-2) & a=a(*,1:ny-2)
    rho=rho(*,1:ny-2) & u=u(*,1:ny-2)   & res=res(*,1:ny-2)
    vx=vx(*,1:ny-2)   & vy=vy(*,1:ny-2) & vz=vz(*,1:ny-2) 
    vex=vex(*,1:ny-2)   & vey=vey(*,1:ny-2) & vez=vez(*,1:ny-2) 
    bx=bx(*,1:ny-2)   & by=by(*,1:ny-2) & bz=bz(*,1:ny-2)
    jx=jx(*,1:ny-2)   & jy=jy(*,1:ny-2) & jz=jz(*,1:ny-2) & ez=ez(*,1:ny-2) 
;he=he(*,1:ny-2)
    p=p(*,1:ny-2)   & bsq=by*by+bx*bx+bz*bz
    ny=ny-2
    
; GRID FOR VELOCITY VECTORS 
grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn

; GRID FOR CONTOUR/SURFACE PLOTS
grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf

print, 'Present rotation angle phi is:',phi
rotconf, by,bz,bypro,bzpro,phi  
rotconf, vy,vz,vypro,vzpro,phi  

asymvel, nx,ny,vypro,vy1,vy2,vyav  
 print, 'vy_msp_orig = ', vy1,'  vy_msh_orig = ',vy2,'   vyav = ',vyav
asymvel, nx,ny,vzpro,vz1,vz2,vzav  
 print, 'vz_msp_orig = ', vz1,'  vz_msh_orig = ',vz2,'   vzav = ',vzav

    a=shift(a,0,nsh) & rho=shift(rho,0,nsh) & u=shift(u,0,nsh)
    vx=shift(vx,0,nsh) & vy=shift(vy,0,nsh) & vz=shift(vz,0,nsh)
    bx=shift(bx,0,nsh) & by=shift(by,0,nsh) & bz=shift(bz,0,nsh)
    jz=shift(jz,0,nsh) & ez=shift(ez,0,nsh) & bzpro=shift(bzpro,0,nsh) 
    p=shift(p,0,nsh) & bsq=shift(bsq,0,nsh) 
    vypro=shift(vypro,0,nsh) & vzpro=shift(vzpro,0,nsh) 

;   !P.REGION=[0.,0.,1.0,1.25]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.MULTI=[0,5,0,0,0]
  !P.CHARSIZE=1.5
  !P.FONT=3
  !P.THICK=1.
  !X.TICKS=4
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
  print, '         <1> -> 1. plot, ion velocity+current;'
  print, '         <2> -> 2. plot, electron velocity+current;'
  print, '         <3> -> 3. plot, el velocity, el field, density;'
  print, '         <3> -> 3. plot, thermal and magnetic pressure;'
  print, '         <4> -> 4. plot, total pressure and temperature;'
  print, '         <5> -> 5. plot, normal velocity and magnetic field;'
  print, '         <s> -> smooth data'
  print, '         <t> -> switch off smoothing'
  print, '         <w> -> change window size'
  print, '         <x> -> new boundaries in x'
  print, '         <y> -> new boundaries in y'
  print, '         <a> -> rotation angle to obtain MSP coordinates'
  print, '         <b> -> input A value for additional field line'
  print, '         <v> -> gallilei transformation of the velocity (V_ymsp)'
  print, '         <i> -> shift no of gridspacings along y (periodic in y only)'
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
      if (iplot gt 12) or (iplot lt 1) then iplot=1 & print,'iplot:',iplot
      plot=string(iplot,'(i1)') & print,'plot:',plot 
  endif
  if (contin eq '1') or (contin eq '2') or (contin eq '3') or $
     (contin eq '4') or (contin eq '5') or (contin eq '6') or $
     (contin eq '7') or (contin eq '8') or (contin eq '9') or $
     (contin eq '10') or (contin eq '11') or (contin eq '12')  then plot=contin
  if contin eq 's' then  smo = 'y'
  if contin eq 't' then  smo = 'n'
  if contin eq 'w' then begin
     print, '  input factor to re-scale window'
     read, wfact
  endif
  if contin eq 'x' then begin
     print, '  input xmin and xmax'
     read, xmin, xmax
     testbd1, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
;     xpmin=xmin  &  xpmax=xmax
     xpmin=xmax  &  xpmax=xmin
;     if (ytit eq 'y') then begin
;       xpmin=xmin  &  xpmax=xmax
;     endif
  endif
  if contin eq 'y' then begin
     print, '  input ymin and ymax'
     read, ymin, ymax
     testbd1, nx,ny,x,y,xmin,xmax,ymin,ymax   
     grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn
     grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf
     delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  endif
  if contin eq 'a' then begin
     print, '  input rotation angle phi, present choice is:',phi
     read, phi
     rotconf, by,bz,bypro,bzpro,phi  
     rotconf, vy,vz,vypro,vzpro,phi  
  endif
  if contin eq 'b' then begin
     print, '  input A level value'
     read, aval
  endif
  if contin eq 'v' then begin
    asymvel, nx,ny,vypro,vy1,vy2,vyav  
    print, 'vy_msp_orig = ', vy1,'  vy_msh_orig = ',vy2,'   vyav = ',vyav
    asymvel, nx,ny,vzpro,vz1,vz2,vzav  
    asymvel, nx,ny,rho,rho1,rho2,rhoav  
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
        write_gif, 'conp'+plot+string(100*time,'(i3.3)')+run+'.gif',imag, r,g,b
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
   xpos(1)=xpmin+0.03*delx/dpx
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

   !P.CHARSIZE=2.0
   !P.MULTI=[0,4,0,0,0]
   grayplot=withps & if colps eq 'y' then grayplot='n'
    if coltab eq 'o' then $
      if withps ne 'y' then loadct,file='/home/ao/idl_lib/colortab.priv', 41 $
        else $
        if colps eq 'y' then loadct,file='/home/ao/idl_lib/colortab.priv', 45 $
          else loadct,file='/home/ao/idl_lib/colortab.priv', 51

  if plot eq '0' then plot='1'
  if plot eq '1' then goto, p1
  if plot eq '2' then goto, p2
  if plot eq '3' then goto, p3
  if plot eq '4' then goto, p4
  if plot eq '5' then goto, p5
  if plot eq '6' then goto, p6
  if plot eq '7' then goto, p7
  if plot eq '8' then goto, p8
  if plot eq '9' then goto, p9
  if plot eq '10' then goto, p10
  if plot eq '11' then goto, p11
  if plot eq '12' then goto, p12


p1:  
    tit='Plasma Velocity / Ion Current' & tit2='Current J_y'    
   if contonly eq 'y' then begin
    ctvec, a,vx,vy,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, jz,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, rho*vz,a,vx,vy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
;    vcplot, rho*vz,a,rho*vx,rho*vy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    scplot2, jz,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
	
p2:
    tit=' Electron Velocity / El. Current J_ey' & tit2='Magn Field B_y' 
   if contonly eq 'y' then begin
    ctvec, a,vex,vey,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, bz,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, -rho*vez,a,vex,vey,pos1,pos2,xpos,ypos,nl1,nl2,names,$
              tit,xtit0,ytit,smo,aval
;    vcplot, -rho*vez,a,rho*vex,rho*vey,pos1,pos2,xpos,ypos,nl1,nl2,names,$
;              tit,xtit0,ytit,smo,aval
    scplot2, bz,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

        
p3:
    tit=' Electric Current' & tit2='Density' 
   if contonly eq 'y' then begin
    ctvec, a,jx,jy,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, rho,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, jz,a,jx,jy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    scplot2, rho,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

        
p4:
;    tit=' Electron Velocity / El. Field E_y' & tit2='Electric Field' 
    tit=' Electron Velocity / Resist.' & tit2=' Electric Field' 
   if contonly eq 'y' then begin
    ctvec, a,vex,vey,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, rho,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, res,a,vex,vey,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    scplot2, ez,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p5:
   tit=' JxB Accel' & tit2='Density' 
   if contonly eq 'y' then begin
    ctvec, a,fjx,fjy,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, rho,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, fjz,a,fjx,fjy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    scplot2, rho,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p6:
   tit=' Grad P Accel' & tit2='pressure' 
   if contonly eq 'y' then begin
    ctvec, a,fpx,fpy,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, p,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, p,a,fpx,fpy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    scplot2, p,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

        
p7:
   tit=' Total Accel' & tit2='Current' 
   if contonly eq 'y' then begin
    ctvec, a,fpx+fjx,fpy+fjy,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    ctsca2, jz,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    !P.MULTI=[0,5,0,0,0]
    vcplot, fjz,a,fpx+fjx,fpy+fjy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
    scplot2, jz,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

        
p8:
    tit='Therm. Pressure' & tit2='Magn. Pressure'
   if contonly eq 'y' then begin
    contsca1, p,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    ctsca2, bsq,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scplot1, p,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scplot2, bsq,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
        
p9:
    tit='Total Pressure' & tit2='Temperature'
   if contonly eq 'y' then begin
    contsca1, p+bsq,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    ctsca2, p/rho,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scplot1, p+bsq,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scplot2, p/rho,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p10:
    tit='Magn Field, B_x' & tit2='Velocity, V_x'
   if contonly eq 'y' then begin
    contsca1, bx,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    ctsca2, vx,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scplot1, bx,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scplot2, vx,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p11:
;    tit='-n Div V' & tit2='-n Div V_e'
    tit='-Div nV' & tit2='-Div nV_e'
   if contonly eq 'y' then begin
    contsca1, -rho*divi,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    ctsca2, -rho*dive,ppos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
;    scplot1, (divni-rho*divi),pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
;    scplot2, (divne-rho*dive),pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
    scplot1, divni,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scplot2, divne,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu

p12:
    tit='B.grad V_ey' & tit2='div J' 
;    tit='Div J_v' & tit2='div J' 
   if contonly eq 'y' then begin
    contsca1, bdivvy,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    ctsca2, divj,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   end else begin
    scplot1, bdivvy,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
;    scplot1, divjv,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo
    scplot2, divb,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu




end


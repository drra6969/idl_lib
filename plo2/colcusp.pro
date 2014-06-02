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
   nx=long(103) & ny=long(103) 
   nxn = 21   &   nyn = 21
   fn1=fltarr(nxn,nyn) & fn2=fn1
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 201   &   nyf = 201
   fa=fltarr(nxf,nyf) & fb=fa
   
;----PARAMETER-------
  xmin = -12. & ymin = -4.0
  xmax =  6. & ymax =  4.0
  xmin0 = xmin & ymin0 = ymin
  xmax0 = xmax & ymax0 = ymax
;--------------------
   time=0.0 & fnumber=1
   name='' & contin='' & again='y' & withps='n' & closeps='n'
   run='' & plot='0'  & coltab='o' &  withgif = 'n' &  contonly = 'n'
   glatt='n' & colps ='n' & grayplot ='n' 
   names=strarr(15) & names=replicate(' ',15)
   nl1=27  &  nl2=9  &  phi=0.0  &  pi = 3.14159265536  &  phir = phi*pi/180.0
   xtit='x' & ytit='y' & smo='n'   & nsh=0  & nshtot=nsh  &  galpar='a'
   nx=long(103) & ny=long(103) & orient='l' & aval=100.0
   wlong=900 & wshort=720 & wfact=1.2 &  wxf=1.0 & wyf=1.0 & psfact=0.9
   wsize=[700,900] & wsold=[0,0] & wino=0 & srat0=1.1

; Grid and Arrays Definitions
   x=findgen(nx) & y=findgen(ny)
   dx=(xmax-xmin)/float(nx-3) & x=x*dx+xmin-dx
   dy=(ymax-ymin)/float(ny-3) & y=y*dy+ymin-dy
   ddx=1./2./dx & ddy=1./2./dy

   hx=x & xt=x 
   a=fltarr(nx,ny,/NOZERO) & bx=a & by=a & bz=a
   r=a & p=a & xy=a & x2y2=a & xx=a & yy=a & yp=a & ym=a
   rp=a & rm=a 
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)

; Vectorpotential, Magnetic field
; 1. Dipole
   c0 = 1000. & c1 = -10. & b_0=10.
   y0 = 10. &  y1 = - 10. 
   psi=90./180.*!pi & b_y0=b_0*sin(psi) & b_x0=b_0*cos(psi)
   for j=0,nx-1 do r(j,*) = sqrt(x(j)^2+y(*)^2)
   if min(r) le 3.0 then begin intr=where(r le 3.0) & r(intr) = 3. & endif
   r2=r^2
   for j=0,nx-1 do begin xx(j,*)= x(j)  & yy(j,*)= y(*) & endfor
   xy= xx*yy & x2y2= xx^2-yy^2 
; Dipole
;   a = c0*xx/r2
;   bx = -2.*c0*xy/r2^2
;   by =  c0*x2y2/r2^2
; Dipole + IMF
;   a = c0*xx/r2-b_y0*xx+b_x0*yy
;   bx = -2.*c0*xy/r2^2 + b_x0
;   by =  c0*x2y2/r2^2 + b_y0
;   for j=1,nx-2 do bz(j,*)= 0.0
; Rotated dipole
;   phi0 = 15./180.*!pi
;   a = c0/r2*( xx*cos(phi0) + yy*sin(phi0) )
;   bx = c0/r2^2 * ( -2.*xy*cos(phi0) + x2y2*sin(phi0) )
;   by = c0/r2^2 * ( x2y2*cos(phi0) + 2.*xy*sin(phi0)  )
;   for j=1,nx-2 do bz(j,*)= 0.0
; Two dipoles
   for j=0,nx-1 do begin yp(j,*)= y(*)-y0 & ym(j,*)=y(*)-y1  & endfor
   r2p = xx^2+yp^2  &  r2m = xx^2+ym^2  & x2y2p= xx^2-yp^2  & x2y2m= xx^2-ym^2 
   xyp = xx*yp & xym = xx*ym
   phi0 = -0./180.*!pi & phi1 = 0./180.*!pi

;   a = c0/r2p*( xx*cos(phi0)+yp*sin(phi0) ) $
;          + c1/r2m*( xx*cos(phi1)+ym*sin(phi1) )
;   bx = c0/r2p^2 *( -2.*xyp*cos(phi0) + x2y2p*sin(phi0) )$
;          + c1/r2m^2 *( -2.*xym*cos(phi1) + x2y2m*sin(phi1) )
;   by = c0/r2p^2 *( x2y2p*cos(phi0) + 2.*xyp*sin(phi0) )$
;          + c1/r2m^2 *( x2y2m*cos(phi1) + 2.*xym*sin(phi1) )
; shielded dipole
   ly1 = 1.9 & dy1 = 2. & dy2 = 0.4 & ly2 = 0.4
   fy1 =(yy+dy1)/ly1 & fy2 =(yy+dy2)/ly2
   tgy1=1.- tanh(fy1) & tgy2=1.- tanh(fy2)
   a = c1/r2m*( xx*cos(phi1)+ym*sin(phi1) )*tgy1*tgy2
   bx = c1/r2m^2 *( -2.*xym*cos(phi1) + x2y2m*sin(phi1) )*tgy1*tgy2 $
         -c1/r2m/ly1*( xx*cos(phi1)+ym*sin(phi1) )/cosh(fy1)^2*tgy2 $
         -c1/r2m/ly2*( xx*cos(phi1)+ym*sin(phi1) )/cosh(fy2)^2*tgy1
   by = c1/r2m^2 *( x2y2m*cos(phi1) + 2.*xym*sin(phi1) )*tgy1*tgy2

   for j=0,nx-1 do bz(j,*)= 0.0
   
   bsq=by*by+bx*bx+bz*bz

;!!!!!!!!!!!!!!!!!CUT?
;   read2df, g1,g2,g3,h1,h2,h3, bx,by,bz, vx,vy,vz, rho,u,res,fnumber   
;   bsq=bx & p=bx & jx=bx & jy=bx & jz=bx & ez=bx
;   bzpro=bx & vzpro=bx & vypro=bx
;   a=fltarr(nx,ny) 
;   f1=bx & f2=bx & f3=bx & f4=bx  
   
;   print, 'Which case?'
;   read, run

   testbd, nx,ny,x,y,xmin,xmax,ymin,ymax   
;   xpmin=xmax  &  xpmax=xmin
   xpmin=xmin  &  xpmax=xmax
   print, 'after testbd',xpmin,xpmax   
   





; CURRENT DENSITY J_Z AND ELECTRIC FIELD E_Z
jandec, nx,ny,ddx,ddy,bx,by,bz,jx,jy,jz
;      jx=smooth(jx,3)   & jy=smooth(jy,3) 

  
; VECTORPOTENTIAL:
;    y=y(1:ny-2)
;    rho=rho(*,1:ny-2) & u=u(*,1:ny-2)   & res=res(*,1:ny-2)
;    vx=vx(*,1:ny-2)   & vy=vy(*,1:ny-2) & vz=vz(*,1:ny-2) 
;    bx=bx(*,1:ny-2)   & by=by(*,1:ny-2) & bz=bz(*,1:ny-2)
;    jx=jx(*,1:ny-2)   & jy=jy(*,1:ny-2) & jz=jz(*,1:ny-2)
;    ez=ez(*,1:ny-2)   & p=2*u^(5.0/3.0) & bsq=by*by+bx*bx+bz*bz
;    ny=ny-2

;vecpotkh, nx,ny,x,y,bx,by,a,fmin,fmax
;    print, 'vectorpotential:',fmax, fmin
    
    
; GRID FOR VELOCITY VECTORS 
grid2d, x,y,xmin,xmax,ymin,ymax,nxn,nyn,xn,yn,iox,ioy,dxn,dyn

; GRID FOR CONTOUR/SURFACE PLOTS
grid2d, x,y,xmin,xmax,ymin,ymax,nxf,nyf,xf,yf,ioxf,ioyf,dxf,dyf

;asymval, nx,ny,vypro,vy1,vy2,vyav  
; print, 'vy_msp_orig = ', vy1,'  vy_msh_orig = ',vy2,'   vyav = ',vyav
; v_ymsp = vy1
;asymval, nx,ny,vzpro,vz1,vz2,vzav  
; print, 'vz_msp_orig = ', vz1,'  vz_msh_orig = ',vz2,'   vzav = ',vzav

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
  print, 'Options: <return> -> next plot, magnetic field;'
  print, '         <2> -> 2. plot, magnetic field;'
  print, '         <s> -> smooth data'
  print, '         <t> -> switch off smoothing'
  print, '         <w> -> change window size'
  print, '         <x> -> new boundaries in x'
  print, '         <y> -> new boundaries in y'
  print, '         <z> -> original boundaries'
  print, '         <b> -> input A value for additional field line'
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
      if (iplot gt 2) or (iplot lt 1) then iplot=1 & print,'iplot:',iplot
      plot=string(iplot,'(i1)') & print,'plot:',plot 
  endif
  if (contin eq '1') or (contin eq '2') then plot=contin
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
  if contin eq 'b' then begin
     print, '  input A level value'
     read, aval
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
;        device,/encaps
        device,/landscape
        device,/inches,xsize=10.,scale_factor=1.0,xoffset= 0.3
        device,/inches,ysize=8.0,scale_factor=1.0,yoffset=10.25
      endif
      if srat le srat0 then  begin
        device,/encaps
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
  if contin eq 'c' then begin
     xloadct,file='/home/ao/idl_lib/colortab.priv', block=1
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
  

p1:  
    tit='Magnetic Field and Current Density' & tit2='Magnetic Pressure'    
    if contonly eq 'y' then begin
     ctvec, a,bx,by,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
     ctsca2, bsq,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
    end else begin
    !P.MULTI=[0,5,0,0,0]
    vcfplotc, jz,a,bx,by,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,$
             smo,aval
    scplot2, bsq,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu
	
p2:
    tit='Magnetic Field and Magnetic Pressure' & tit2='Vectorpotential' 
    if contonly eq 'y' then begin
     ctvec, a,bx,by,pos1,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,smo,aval
     ctsca2, bsq,pos3,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
    end else begin
    !P.MULTI=[0,5,0,0,0]
    vcfplotc, bsq,a,bx,by,pos1,pos2,xpos,ypos,nl1,nl2,names,tit,xtit0,ytit,$
             smo,aval
    scplot2, a,pos3,pos4,xpos,ypos,nl1,nl2,names,tit2,xtit,ytit1,smo
   endelse
  goto, menu


end


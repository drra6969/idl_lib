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
;    <-------       ------->
;      x                  x

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, $
                    xn,yn, iox,ioy,ioxf,ioyf, run, time, $
                    xpos,ypos0,ypos1,ypos2,ypos3,ypos4, $
                    ypos5,ypos6,xpmin,xpmax,ymin,ymax

; GRID FOR VELOCITY VECTORS
   nxn = 15   &   nyn = 21
   iox=fltarr(nxn) & ioy=fltarr(nyn)
   fn1=fltarr(nxn,nyn) & fn2=fn1 & fn3=fn1 & fn4=fn1 
; GRID FOR CONTOUR AND SURFACE PLOTS
   nxf = 61   &   nyf = 61
   ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
   fa=fltarr(nxf,nyf) & fb=fa
  names=strarr(15)
  names=replicate(' ',15)

;----PARAMETER-------
  xmin =  -8. & ymin = 200.
  xmax =  8. & ymax = 1100
;--------------------
   time=0.0 & fnumber='1'
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303) & nyy=long(303)

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx &
   vx=bx & vy=bx & vz=bx & vex=bx & vey=bx & vez=bx
   vnx=bx & vny=bx & vnz=bx & meff=bx & nu12s=bx
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx
   rhono=bx & rhono2=bx & rhonn2=bx 
   rhon=bx & rhona=bx & rhont=bx & mo=bx
   un=bx & una=bx & pn=bx & pna=bx
   pe=bx & gradpe=bx & gradpex=bx & gradpz=bx & divb=bx & divv=bx
   gpx=bx & pp=bx & byx=bx & bxy=bx 
   nuei=bx & nuen=bx & nuin=bx & res=bx 
   ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
   ex_dpe=bx & ey_dpe=bx
   sigmap=bx & sigmah=bx & s0=bx & jp=bx & jh=bx
   sigmapi=bx & sigmape=bx & sigmahi=bx & sigmahe=bx
   sigma0=bx & sigma0i=bx & sigma0e=bx
   bsq=bx & p=bx & p1=bx
   f1=bx & f2=bx & f3=bx & f4=bx & ff=bx
   a=fltarr(nx,ny) 
   
   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin
   readu, 8,  jx,jy,jz
   close, 8
      p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0) & pe=rho*te
   vx=vx/rho &  vz=vz/rho & vy=vy/rho  
   vex=vex/rho &  vez=vez/rho & vey=vey/rho

   name='magnap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhona,una,nu12s
   close, 8
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon

   pn=2*un^(5.0/3.0) & pn1=2*una^(5.0/3.0)
   rhont=rhono+rhono2/2+rhonn2*4/7
   meff = rhona/rhont
   tna = pn1/rhont & tia=tna & tea=p1/rho1-tia
   
	y = h0+y
	dy = dy
	by = by
	vy = vy
	vey = vey
        vny = vny
   f1 = shift(p,-1,0)-shift(p,1,0)
   for j=1,ny-2 do gradpz(*,j)=dy(j)*f1(*,j)
   
   for i=1,nx-2 do gpx(i,*)=dx(i)*f1(i,*)
      gradpe=(shift(pe,0,-1)-shift(pe,0,1))
      for j=1,ny-2 do gradpe(*,j)=dy(j)*gradpe(*,j)
      for i=0,nx-1 do gradpe(i,0)=0. 
      for i=0,nx-1 do gradpe(i,ny-1)=0.
      gradpex=(shift(pe,-1,0)-shift(pe,1,0))
      for j=1,nx-2 do gradpex(j,*)=dx(j)*gradpex(j,*)
      for i=0,ny-1 do gradpex(0,i)=0. 
      for i=0,ny-1 do gradpex(nx-1,i)=0.

   f1 = shift(bx,-1,0)-shift(bx,1,0)
   f2 = shift(by,0,-1)-shift(by,0,-1)
   for i=1,nx-2 do f1(i,*)=dx(i)*f1(i,*)
   for j=1,ny-2 do f2(*,j)=dy(j)*f2(*,j)
   difb=f1+f2
   f1=rho*vx & f2=rho*vy
   f1 = shift(f1,-1,0)-shift(f1,1,0)
   f2 = shift(f2,0,-1)-shift(f2,0,-1)
   for i=1,nx-2 do f1(i,*)=dx(i)*f1(i,*)
   for j=1,ny-2 do f2(*,j)=dy(j)*f2(*,j)
   difv=f1+f2

    if (xmin lt x(1)) then begin
     print, 'warning! xmin:',xmin,' out of bounds:',x(1),' Reset!'
     xmin=x(1)
   endif  
   if (xmax gt x(nx-2)) then begin
     print, 'warning! xmax:',xmax,' out of bounds:',x(nx-2),' Reset!'
     xmax=x(nx-2)
   endif  
   if (ymin lt y(1)) then begin
     print, 'warning! ymin:',ymin,' out of bounds:',y(1),' Reset!'
     ymin=y(1)
   endif  
   if (ymax gt y(ny-2)) then begin
     print, 'warning! ymax:',ymax,' out of bounds:',y(ny-2),' Reset!'
     ymax=y(ny-2)
   endif  
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  
; GRID FOR VELOCITY VECTORS 
   xn=findgen(nxn) & yn=findgen(nyn)
   dxn=(xmax-xmin)/float(nxn-1) & xn=xn*dxn+xmin
   dyn=(ymax-ymin)/float(nyn-1) & yn=yn*dyn+ymin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xn(in) gt x(k+1) do k=k+1
     iox(in) = float(k) + (xn(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxn-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while yn(in) gt y(k+1) do k=k+1
     ioy(in) = float(k) + (yn(in)-y(k))/(y(k+1)-y(k))        
   endrep until in eq nyn-1

; GRID FOR CONTOUR/SURFACE PLOTS
   xf=findgen(nxf) & yf=findgen(nyf)
   dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
   dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xf(in) gt x(k+1) do k=k+1
     ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxf-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while yf(in) gt y(k+1) do k=k+1
     ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
   endrep until in eq nyf-1

; VECTORPOTENTIAL:
    a=0.0*bx
    for k=3, ny-2, 2 do $
        a(1,k)=a(1,k-2)+bx(1,k-1)*(y(k)-y(k-2))
    for k=2, ny-3, 2 do $
        a(1,k)=a(1,k-1)+0.5*(bx(1,k-1)+bx(1,k))*(y(k)-y(k-1))
    for k=1, ny-1 do begin
     for l=3, nx-2,2 do begin
        a(l,k)=a(l-2,k)-by(l-1,k)*(x(l)-x(l-2))
     endfor  
    endfor
    for k=1, ny-1 do $
     for l=2, nx-3,2 do $
        a(l,k)=a(l-1,k)-0.5*(by(l-1,k)+by(l,k))*(x(l)-x(l-1))
    fmax=max(a((nx-1)/2,2:ny-1))
    fmin=min(a((nx-1)/2,2:ny-1))
    print, fmax, fmin

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
   print, sizeratio, dpx, dpy
    dpx =0.18 & dpy=0.8
    xleft=0.06 & xinter=0.14 & hop=0.47*xinter
    xa1=xleft & xe1=xleft+dpx  
    xa2=xe1+xinter & xe2=xa2+dpx
    xa3=xe2+xinter & xe3=xa3+dpx
    ylo=0.06 & yup=ylo+dpy
   
   xpmin=xmax
   xpmax=xmin
   ytit='z'
     
   b_units=b0*rescal*100000.                     ; in nT
   n_units=dens0                                 ; in cm^(-3)
   v_units=2.18*1e6/4.*rescal*b0/sqrt(dens0)     ; for oxygen in km/s
   vv_units=v_units*1000.                        ; vel in m/s 
   l_units=length0/1e5                           ; in km
   ta_time=l_units/v_units                       ; in s
   j_units = b0*rescal/l_units/4./3.1415*1e6     ; in micro A/m^2
   e_units = v_units*b_units                     ; in micro V/m
   res_units = e_units/j_units                   ; Ohm m
   t_units = b0^2*rescal^2/8./3.1415/1.38/dens0*1e13 ; in 1000K
   p_units=n_units*1.38*1.e-7*t_units*100.       ; nPa
   print, 'b_units =', b_units,' nT'
   print, 'n_units =', n_units,' cm^(-3)'
   print, 'v_units km =', v_units,' km/s'
   print, 'v_units m =', vv_units,' m/s'
   print, 'l_units km =', l_units,' km'
   print, 't_units =', ta_time,' s'
   print, 'j_units =', j_units,' micro A/m^2'
   print, 'temp_units =', t_units,' 1000K'

   lambdam = ta_time*(30/5.64)*sqrt(1./16./1836.2/dens0)
   ex_dpe=gradpex*t_units*1.38/1.6e-2/rho/e_units
   ey_dpe=gradpe*t_units*1.38/1.6e-2/rho/e_units
   ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx-ex_dpe
   ey=nuen*vy*lambdam-vez*bx+vex*bz+res*jy-ey_dpe
   ez=nuen*vz*lambdam-vex*by+vey*bx+res*jz

;   nuei = nuei
;   nuen = nuen/meff
;   nuin = nuin/meff
   w_ce = 8.78E+6
   w_ci = 299.

; Pedersen and Hall Conductivity 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   s0 = 1.6E-8*rho*n_units/5.
   sigmapi = s0*nuin*w_ci/(w_ci*w_ci+nuin*nuin)
   sigmape = s0*nuen*w_ce/(w_ce*w_ce+nuen*nuen)
   sigmap = sigmapi + sigmape
   sigmahi = -s0*w_ci*w_ci/(w_ci*w_ci+nuin*nuin)
   sigmahe = s0*w_ce*w_ce/(w_ce*w_ce+nuen*nuen)
   sigmah = sigmahi + sigmahe
   sigma0i = s0*w_ci/nuin
   sigma0e = s0*w_ce/(nuen+nuei)
   sigma0 = sigma0i + sigma0e
;   s0 = 1.6*5.*1.67*nuin $
;        /( 0.91E-6*(nuei+nuen)*1.67*nuin+1.6*2500.)
;   sigmap = 1.6E-8*rho*n_units/5.*s0/( 1. + s0*s0 )
;   sigmah = s0*sigmap
   jp = ( sigmap*ex-sigmah*ez )*e_units/j_units
   jh = ( sigmah*ex+sigmap*ez )*e_units/j_units
   for i=0,nx-1 do jx(i,0) = jp(i,0)
   for i=0,nx-1 do jx(i,1) = jp(i,1)
   for i=0,nx-1 do jz(i,0) = jh(i,0)
   for i=0,nx-1 do jz(i,1) = jh(i,1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        device,/landscape
        !P.THICK=2.
;        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif

        xpos=xpmax-0.01*delx/dpx
        ypos0=ymin+0.25*dely         ; location for 'time'
        ypos1=ypos0-0.025*dely/dpy   ; next line
        ypos2=ypos0+0.075*dely/dpy   ; location 'CASE'
        ypos3=ymin+.8*dely          ; location 'Max='
        ypos4=ypos3-0.025*dely/dpy   ; next line
        ypos5=ypos4-0.05*dely/dpy         ; location 'Min='
        ypos6=ypos5-0.025*dely/dpy   ; next line
        
   tt=time*ta_time
   tit=' '
   xtit=' '

  print, 'plot first page? Vz, Vy, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot first page (Vy, By, Jz)
;---------------------------------------------------------------
;       !P.REGION=[0.,0.,1.0,1.25]
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=4
       !Y.TICKS=7
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2

       tit = ' Horizontal Velocity Vy (Km/s) '
       xtit = ' '
       ff = -v_units*vz
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,16,1
       
       tit = ' Magnetic Field By (nT) '
       xtit = ' '
       ff = -b_units*bz
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,10,1
       
       tit = ' J_z  (= -J_|| (uA/m^2))'
       xtit = ' '
       ff = j_units*jy
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,15,1
  endif

  print, 'continue with next plot, Vx,Vy',$
         '  and Vz'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 2 ( Vy, Vz, Vx)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = 'Vx (Km/s)'
       xtit = ' '
       ff = v_units*vx
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,16,1

       tit = 'Vz (Km/s)'
       xtit = ' '
       ff = v_units*vy
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = 'Vy (Km/s)'
       xtit = ' '
       ff = -v_units*vz
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif

       
  print, 'continue with next plot, Vex,Vey',$
         '  and Vez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 3 ( Vey, Vez, Vex)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = 'Vex (Km/s)'
       xtit = ' '
       ff = v_units*vex
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,16,1

       tit = 'Vez (Km/s)'
       xtit = ' '
       ff = v_units*vey
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = 'Vey (Km/s)'
       xtit = ' '
       ff = -v_units*vez
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif

       
  print, 'continue with next plot, Jx, Jy',$
         '  and Jz'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 4 ( jy, jz, jx)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = 'Jx ( uA/m^2 )'
       xtit = ' '
       ff = j_units*jx
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,16,1

       tit = 'Jz ( uA/m^2 )'
       xtit = ' '
       ff = j_units*jy
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = 'Jy ( uA/m^2 )'
       xtit = ' '
       ff = -j_units*jz
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif

       
  print, 'continue with next page? Density, temperature'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 5 ( by, bz, bx)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = 'Bx (nT)'
       xtit = ' '
       ff = -b_units*bx & ff=smooth(ff,3)
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,16,1

       tit = 'Bz (nT)'
       xtit = ' '
       ff = b_units*by
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E12.4)',xtit,tit,tt,15,1

       tit = 'By (nT)'
       xtit = ' '
       ff = -b_units*bz
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif

       
  print, 'continue with next page? Density, temperature'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 6 ( Rho, Ti, Te)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

;       tit = ' Density !9 r !X'
;       xtit = ' (1/cm^3)'
;       ff = n_units*rho
;;       tt = ta_time*time
;       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Pressure (nPa)'
       xtit = ' (nPa)'
       ff = p_units*p
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

       tit = 'Ti (1000K)'
       xtit = ' '
       ff = t_units*(p/rho-te)
       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f9.4)',xtit,tit,tt,15,1

       tit = ' Te (1000K) '
       xtit = ' '
       ff = t_units*te
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

  endif
       
  print, 'continue with next page? Density, delT'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 7 ( rho, Del Ti, Del Te)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

;       tit = ' Pressure (nPa)'
;       xtit = ' (nPa)'
;       ff = p_units*p
;;       tt = ta_time*time
;       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

;       tit = ' Density !9 r !X (1/cm^3)'
       tit = ' Density n (1/cm^3)'
       xtit = ' '
       ff = n_units*rho
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Del Ti (1000K) '
       xtit = ' '
       ff = t_units*(p/rho-te-tia)
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Del Te (1000K) '
       xtit = ' '
       ff = t_units*(te-tea)
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif
       
  print, 'continue with next page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------------
;print page 8 ( B & J, V, Ve)
;---------------------------------------------------------------------
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0

       tit = ' '
       xtit = ' '
       ff = -a
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,16,0

        fn1=interpolate(j_units*jx,iox,ioy,/grid)
        fn2=interpolate(j_units*jy,iox,ioy,/grid)
;        fmax=max(sqrt(fn1^2+fn2^2))
        fmax=max(j_units*sqrt(jx^2+jy^2))
	!P.POSITION=[xa1,ylo,xe1,yup]
        vect, fn1*1.5, fn2, xn, yn, length=1.5,$
        title=' B Field and current density (uA/m^2)'
	xyouts,charsize=1,xpos,ypos0,'time'
	xyouts,charsize=1,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+' s'
	xyouts,charsize=1,xpos,ypos3,'Max=' 
	xyouts,charsize=1,xpos,ypos4,' '+string(fmax,'(f7.2)')+ ' '

        fn1=interpolate(v_units*vx,iox,ioy,/grid)
        fn2=interpolate(v_units*vy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa2,ylo,xe2,yup]
        vect, fn1, fn2, xn, yn, length=1.2,$
        title=' Velocity (Km/s) ',/noerase
        ;,sizerat=srat
	xyouts,charsize=1,xpos,ypos0,'time'
	xyouts,charsize=1,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+' s'
	xyouts,charsize=1,xpos,ypos2,run
	xyouts,charsize=1,xpos,ypos3,'Max='
	xyouts,charsize=1,xpos,ypos4,' '+string(fmax,'(E9.1)')+' '

        fn1=interpolate(v_units*vex,iox,ioy,/grid)
        fn2=interpolate(v_units*vey,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa3,ylo,xe3,yup]
        vect, fn1, fn2, xn, yn, length=1.5,$
        title='Electron Velocity (Km/s)'
	xyouts,charsize=1,xpos,ypos0,'time'
	xyouts,charsize=1,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+' s'
	xyouts,charsize=1,xpos,ypos2,run
	xyouts,charsize=1,xpos,ypos3,'Max='
	xyouts,charsize=1,xpos,ypos4,' '+string(fmax,'(f7.2)')+' '
  endif

  print, 'continue with next plot Ex,Ez, and Ey'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot  page 9 ( Ex, Ez, Ey)
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = ' Ex (mV/m) '
       xtit = ' '
       ff = e_units*ex*0.001
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,20,1

       tit = ' Ez (uV/m) '
       xtit = ' '
       ff = e_units*ey
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,20,1

       tit = ' Ey (mV/m) '
       xtit = ' '
       ff = -e_units*ez*0.001
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

  endif


  print, 'continue with next plot, grad Pe_x, Pe_z, and Pe'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot  page 10 ( Ex, Ez, Res)
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = ' Grad Pe_x (uV/m) '
       xtit = ' '
       ff = e_units*ex_dpe
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,20,1

       tit = ' Grad Pe_z (uV/m) '
       xtit = ' '
       ff = e_units*ey_dpe
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,20,1

       tit = 'Electron Pressure (nPa)'
       xtit = ' '
       ff = p_units*pe
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1


  endif


  print, 'continue with next plot, grad Pe_x, Pe_z, and Pe'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
;---------------------------------------------------------------
; plot  page 11 ( DIV)
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = ' Div V '
       ff = difv
       xtit = ' '
;       tit = ' Grad P_z'
;       ff = gradpz
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E9.2)',xtit,tit,tt,15,1

       tit = ' Div B '
       xtit = ' '
       ff = difb
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(e9.2)',xtit,tit,tt,20,1

       tit = 'JxB (z comp)'
       xtit = ' '
       ff = jz*bx-jx*bz
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E9.2)',xtit,tit,tt,15,1

  endif
  
     !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


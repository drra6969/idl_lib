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
  xmin =  0. & ymin = 300.
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
   pe=bx & gradpe=bx & gradpex=bx
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
      p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0) 
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
;   rescal=1.0
   
	y = h0+y/rescal
	dy = dy*rescal
	by = by/rescal
	vy = vy/rescal
	vey = vey/rescal
        vny = vny/rescal
   f1 = shift(p,-1,0)-shift(p,1,0)
   for i=1,nx-2 do gpx(i,*)=dx(i)*f1(i,*)
      gradpe=(shift(pe,0,-1)-shift(pe,0,1))
      for j=1,ny-2 do gradpe(*,j)=dy(j)*gradpe(*,j)
      for i=0,nx-1 do gradpe(i,0)=0. 
      for i=0,nx-1 do gradpe(i,ny-1)=0.
      gradpex=(shift(pe,-1,0)-shift(pe,1,0))
      for j=1,nx-2 do gradpex(j,*)=dx(j)*gradpex(j,*)
      for i=0,ny-1 do gradpex(0,i)=0. 
      for i=0,ny-1 do gradpex(nx-1,i)=0.

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
   print, 'b_units =', b_units
   print, 'v_units km =', v_units
   print, 'v_units m =', vv_units
   print, 'l_units km =', l_units
   print, 't_units =', ta_time
   print, 'j_units =', j_units
   print, 'j_units =', j_units
   print, 'temp_units =', t_units

   lambdam = ta_time*(30/5.64)*sqrt(1./16./1836.2/dens0)
   ex_dpe=-gradpex*t_units*1.38/1.6e-2/rho/e_units
   ey_dpe=-gradpe*t_units*1.38/1.6e-2/rho/e_units
   ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx+ex_dpe
   ey=nuen*vy*lambdam-vez*bx+vex*bz+res*jy+ey_dpe
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
       !Y.TICKS=8
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2

       tit = ' Horizontal velocity Vy (Km/s) '
       xtit = ' '
       ff = v_units*vz
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,16,1
       
       tit = ' Magnetic Field By (nT) '
       xtit = ' '
       ff = b_units*bz
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,10,1
       
       tit = ' Field-aligned Current Density (uA/m^2)'
       xtit = ' '
       ff = -j_units*jy
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,15,1
  endif

  print, 'continue with next plot, Vnz,Vny',$
         '  and overlay current and B lines'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 2 ( VNy, VNz, VNx)
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
       ff = v_units*vz
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif

       
  print, 'continue with next page? Density, temperature'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 3 ( Rho, Ti, Te)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

;       tit = ' Density !9 r !X'
;       xtit = ' (1/cm^3)'
;       ff = -n_units*rho
;;       tt = ta_time*time
;       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Pressure (nPa)'
       xtit = ' (nPa)'
       ff = -p_units*p
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

       tit = 'Ti (1000K)'
       xtit = ' '
       ff = -t_units*(p/rho-te)
       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

       tit = ' Te (1000K) '
       xtit = ' '
       ff = -t_units*te
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

  endif
       
  print, 'continue with next page? Density, delT'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;--------------------------------------------------------------------
; plot page 4 ( rho, Del Ti, Del Te)
;--------------------------------------------------------------------

        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

;       tit = ' Pressure (nPa)'
;       xtit = ' (nPa)'
;       ff = -p_units*p
;;       tt = ta_time*time
;       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

;       tit = ' Density !9 r !X (1/cm^3)'
       tit = ' Density n (1/cm^3)'
       xtit = ' '
       ff = -n_units*rho
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Del Ti (1000K) '
       xtit = ' '
       ff = -t_units*(p/rho-te-tia)
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Del Te (1000K) '
       xtit = ' '
       ff = -t_units*(te-tea)
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif
       
  print, 'continue with next page? Magnetic field, flow vectors'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------------
;print page 5 ( B & J, V, Ve)
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
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+' s'
	xyouts,xpos,ypos3,'Max=' 
	xyouts,xpos,ypos4,' '+string(fmax,'(f7.2)')+ ' '

        fn1=interpolate(v_units*vx,iox,ioy,/grid)
        fn2=interpolate(v_units*vy,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa2,ylo,xe2,yup]
        vect, fn1, fn2, xn, yn, length=1.2,$
        title=' Velocity (Km/s) ',/noerase
        ;,sizerat=srat
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+' s'
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,' '+string(fmax,'(E9.1)')+' '

        fn1=interpolate(v_units*vex,iox,ioy,/grid)
        fn2=interpolate(v_units*vey,iox,ioy,/grid)
        fmax=sqrt(max(fn1^2+fn2^2))
	!P.POSITION=[xa3,ylo,xe3,yup]
        vect, fn1, fn2, xn, yn, length=1.5,$
        title='Electron Velocity (Km/s)'
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)')+' s'
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,' '+string(fmax,'(f7.2)')+' '
  endif

  print, 'continue with next plot,B, Ex,Ez, and Resistivity'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot  page 6 ( Ex, Ez, Res)
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]

       tit = ' Ex (mV/m) '
       xtit = ' '
       ff = -e_units*ex*0.001
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,20,1

       f1=e_units*ey
       tit = ' Ez (uV/m) '
       xtit = ' '
       ff = -f1
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,20,1

       tit = 'Density n (1/cm^3)'
       xtit = ' '
       ff = -n_units*rho
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

  endif

  print, 'continue with next plot, surface Rho, p_B'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
;---------------------------------------------------------------
;plot page 7 ( surface Rho, p_B )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        
        p=2*u^(5.0/3.0)
        bsq=bx*bx+by*by+bz*bz
;        bsq=bsq+p
        bsq=bsq
        f1=smooth(bsq,5)
        fa=interpolate(f1,ioxf,ioyf,/grid)
        fb=interpolate(rho,ioxf,ioyf,/grid)

    surface,fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='rho',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit
    surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='p_B',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit,/noerase

  endif

  print, 'continue with next plot, bn, and vn'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 8 ( BN, VN, BZ )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

       f1=smooth(bx,3)
       tit = ' BX Component (nT)'
       xtit = ' '
       ff = -b_units*f1
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f9.5)',xtit,tit,tt,15,1

       f1=smooth(vx,3)
       tit = ' VX Component (m/s)'
       xtit = ' '
       ff = -vv_units*f1
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.3)',xtit,tit,tt,15,1

       f1=smooth(by,3)
       tit = ' -BZ (nT)'
       xtit = ' '
       ff = b_units*f1
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E11.4)',xtit,tit,tt,15,1

   endif

  print, 'continue with next plot, surface bn and vn'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
;plot page 9 ( surface BN, VN )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        f1=smooth(bx,5)
        fb=interpolate(f1,ioxf,ioyf,/grid)
        f2=smooth(vx,5)
        fa=interpolate(f2,ioxf,ioyf,/grid)
    surface,fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='BN',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit
    surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='VN',xstyle=1,ystyle=1,xtitle='x',ytitle=ytit,/noerase
  endif

  print, 'continue with next plot, jx and jy'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 10 ( Jx, Jy )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

       tit = ' Jx (uA/m^2) '
       xtit = ' '
       ff = -jx*j_units
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,10,1

       tit = ' Jy (uA/m^2)'
       xtit = ' '
       ff = jz*j_units
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,11,1

       tit = ' Field-aligned Current Density (uA/m^2)'
       xtit = ' '
       ff = -j_units*jy
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,14,1

;       f1=smooth(ex*e_units*.001,5)
;       tit = ' Ex '
;       xtit = '(mV/m)'
;       ff = -f1
;       tt = ta_time*time
;       contplot, ff,xa3,xe3,ylo,yup,'(f7.2)',xtit,tit,tt,10,1
  endif

  print, 'continue with next plot, Pedersen conductivity sigmap'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 11 ( Sigmap )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

;       f1=smooth(sigmap,3) 
       tit = 'Pedersen Conductivity'
       xtit = ' '
;       ff = -f1
       ff = -sigmap
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

;       f1=smooth(sigmah,3) 
       tit = 'Hall Conductivity'
       xtit = ' '
;       ff = -f1
       ff = -sigmah
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

;       f1=smooth(sigma0,3)
       tit = 'Parallel Conductivity'
       xtit = ' '
;       ff = -f1
       ff = -sigma0
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

  endif

  print, 'continue with next plot, Hall conductivity'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 12 ( Sigmah )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

;       f1=smooth(sigmah,3)
       tit = ' Hall conductivity '
       xtit = ' '
;       ff = -f1
       ff = -sigmah
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

;       f1=smooth(sigmahi,3) 
       tit = 'Sh, ion contribution'
       xtit = ' '
;       ff = -f1
       ff = -sigmahi
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

;       f1=smooth(sigmahe,3)
       tit = 'Sh, e contribution'
       xtit = ' '
;       ff = -f1
       ff = -sigmahe
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,10,1
  endif

  print, 'continue with next plot, Parallel conductivity'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 12 ( Sigma0 )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

;       f1=smooth(sigma0,3)
       tit = ' Sigma0 '
       xtit = ' '
;       ff = -f1
       ff = -sigma0
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

;       f1=smooth(sigma0i,3) 
       tit = 'S0, ion contribution'
       xtit = ' '
       ff = -f1
       ff = -sigma0i
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

;       f1=smooth(sigma0e,3)
       tit = 'S0, e contribution'
       xtit = ' '
;       ff = -f1
       ff = -sigma0e
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,10,1
  endif

  print, 'continue with next plot, Hall and Pedersen current'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 13 ( Jx, Jy )
;---------------------------------------------------------------
        !P.CHARSIZE=1.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=3

       tit = ' Pedersen Current (uA/m^2) '
       xtit = ' '
       ff = -jp*j_units
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,10,1

       tit = ' Hall current (uA/m^2) '
       xtit = ' '
       ff = jh*j_units
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

  endif

  print, 'continue with next plot, dj'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 13 ( dJx, dJy )
;---------------------------------------------------------------
        !P.CHARSIZE=1.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=3

       tit = ' (jx-jp) '
       xtit = ' '
       ff = -(jx-jp)/(abs(jx)+1.e-9)
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

       tit = ' (jz-jh) '
       xtit = ' '
       ff = (jz-jh)/(abs(jz)+1.e-9)
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(f7.2)',xtit,tit,tt,15,1

  endif

  print, 'continue with next plot, nuen, nuin, and nuei'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 14 ( Jx, Jy )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

       tit = ' Nuei '
       xtit = '(1/s)'
       ff = -nuei/ta_time
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

       tit = ' Nuen '
       xtit = '(1/s)'
       ff = -nuen/ta_time
       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Nuin '
       xtit = '(1/s)'
       ff = -nuin/ta_time
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,10,1
  endif

  print, 'continue with next plot, tao_en, tao_in, and tao_ei'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
; plot page 15 ( Jx, Jy )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,3,0,0,0]
        !P.FONT=3

       tit = ' Tao_ei '
       xtit = '(s)'
       ff = -ta_time/nuei
;       tt = ta_time*time
       contplot, ff,xa1,xe1,ylo,yup,'(E8.1)',xtit,tit,tt,10,1

       tit = ' Tao_en '
       xtit = '(s)'
       ff = -ta_time/nuen
;       tt = ta_time*time
       contplot, ff,xa2,xe2,ylo,yup,'(E8.1)',xtit,tit,tt,15,1

       tit = ' Tao_in '
       xtit = '(s)'
       ff = -ta_time/nuin
;       tt = ta_time*time
       contplot, ff,xa3,xe3,ylo,yup,'(E8.1)',xtit,tit,tt,10,1
  endif

  print, 'continue with next plot, surface djx and djy'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
;plot page 16 ( surface dJx, dJy )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.MULTI=[0,2,0,0,0]
        !P.FONT=-1
        f1=smooth((jx-jp)/(abs(jx+1.e-9)+abs(jp+1.e-9)),5)
        fb=interpolate(f1,ioxf,ioyf,/grid) ; & fb=smooth(fb,3)
;        for i=0, nx-1 do fb(i,0)=0.
        f2=smooth(-(jz-jh)/(abs(jz+1.e-9)+abs(jh+1.e-9)),5)
        fa=interpolate(f2,ioxf,ioyf,/grid)
      surface,-fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='(jx-jp)/|jx|',xstyle=1,ystyle=1,$
                                xtitle='x',ytitle=ytit
      surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle=' (jy-jh)/|jy| ',xstyle=1,ystyle=1,$
                              xtitle='x',ytitle=ytit; ,/noerase
  endif

  print, 'continue with next plot, surface jz and ez'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;---------------------------------------------------------------
;plot page 16 ( surface J, E )
;---------------------------------------------------------------
        !P.CHARSIZE=2.0
        !P.FONT=-1
        f1=smooth(jy,5)
        fb=interpolate(f1,ioxf,ioyf,/grid) & fb=smooth(fb,3)
        f2=smooth(ex,5)
        fa=interpolate(f2,ioxf,ioyf,/grid)
      surface,-fb,xf,yf,position=[0.2,0.1,0.45,1.1,0.1,0.5],ax=35,$
      ztitle='-Current Density',xstyle=1,ystyle=1,$
                                xtitle='x',ytitle=ytit
      surface,fa,xf,yf,position=[0.6,0.2,0.85,1.2,0.4,0.9],ax=35,$
      ztitle='Electric Field',xstyle=1,ystyle=1,$
                              xtitle='x',ytitle=ytit,/noerase
  endif
     !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


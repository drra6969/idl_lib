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
  plane='x' & whatcut='x' & igrid=2 & newgrid='2'
  unit='s' & whatunit='s'

;----PARAMETER-------
  xmin =  -12. & ymin = 100.
  xmax =   12. & ymax = 900.
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
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx & tp=bx & t0=bx
   rhono=bx & rhono2=bx & rhonn2=bx & tea=bx & tna=bx & tia=bx
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   p=bx & pp=bx & pe=bx & pn=bx & p1=bx & pn1=bx
   gradp=bx & gradpe=bx & gradpex=bx
   nuei=bx & nuen=bx & nuin=bx & res=bx
   ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
   exs=bx & eys=bx & ezs=bx & angle=bx & ex_dpe=bx
   ey_iner=bx & ey_g=bx & ey_dpe=bx & ey_res=bx & ey_veXb=bx
   sigmap=bx & sigmah=bx & s0=bx & jp=bx & jh=bx
   sigmapi=bx & sigmape=bx & sigmahi=bx & sigmahe=bx
   sigma0=bx & sigma0i=bx & sigma0e=bx
   f1=bx & f2=bx & f3=bx & f4=bx 
   b0=1. & bdip=1. & length0=1. & lengthy0=1. & dens0=1. & h0=1.

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  b0,bdip,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin
   readu, 8,  jx,jy,jz
   close, 8
; b0 normalization orig in G now in nT
; rescal =b0/bdip with bdip in G now in nT
; length0 in cm; lengthy0=bdip/b0*length0
; dens0 in cm^-3; h0=hight0 in km 

; SI units
   ee  = 1.6022e-19                   ; elementary charge
   kb  = 1.3807e-23                   ; Boltzmann cosnt
   mu0 = 4.*!pi*1e-7                  ; Permeability
   me = 9.1094e-31                    ; electron mass
   mp = 1.6726e-27                    ; proton mass
   mo = 8.*mp                         ; oxygen mass
   b_si = 1e-9*b0                     ; b0 in T   
   n_si = 1e6*dens0                   ; dens0 in m^-3
   v_si = b_si/sqrt(mu0*n_si*mo)      ; alfven velocity for oxygen
   l_si = length0/1e2                 ; length0 in m
   t_si = l_si/v_si                   ; time
   j_si = b_si/l_si/mu0               ; current density
   e_si = v_si*b_si                   ; electric field
   p_si = b_si^2/2./mu0               ; pressure
   res_si = e_si/j_si                 ; resitivity (Ohm m)
   temp_si = p_si/kb/n_si             ; temperature

   bnorm    = 1e9*b_si                    ; in nT
   nnorm    = 1e-6*n_si               ; in cm^(-3)
   vnorm    = 1e-3*v_si               ; for oxygen in km/s
   vvnorm   = v_si*1000.              ; vel in m/s 
   lnorm    = 1e-3*l_si               ; in km
   tnorm    = l_si/v_si               ; in s
   jnorm    = 1e6*j_si                ; in micro A/m^2
   enorm    = 1e6*e_si                ; in micro V/m
   resnorm  = res_si                  ; Ohm m
   tempnorm = 1e-3*temp_si             ; in 1000K
   pnorm    = 1e9*p_si                ; nPa
; old
;   b_units=b0*rescal*100000.                     ; in nT
;   n_units=dens0                                 ; in cm^(-3)
;   v_units=2.18*1e6/4.*rescal*b0/sqrt(dens0)     ; for oxygen in km/s
;   vv_units=v_units*1000.                        ; vel in m/s 
;   l_units=length0/1e5                           ; in km
;   ta_time=l_units/v_units                       ; in s
;   j_units = b0*rescal/l_units/4./3.1415*1e6     ; in micro A/m^2
;   e_units = v_units*b_units                     ; in micro V/m
;   res_units = e_units/j_units                   ; Ohm m
;   t_units = b0^2*rescal^2/8./3.1415/1.38/dens0*1e13 ; in 1000K
;   p_units=n_units*1.38*1.e-7*t_units*100.       ; nPa

   vx=vx/rho &  vz=vz/rho & vy=vy/rho
   vex=vex/rho &  vez=vez/rho & vey=vey/rho
   p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0)
   pe=rho*te & pp=p-pe & t0=p1/rho1/2.

   name='magnap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhon1,un1,nu12s
   close, 8
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon

   pn=2*un^(5.0/3.0) & pn1=2*un1^(5.0/3.0)
   rhont=rhono+rhono2/2+rhonn2*4/7
   meff = rhon1/rhont
   tna = pn1/rhont & tia=tna & tea=p1/rho1-tia

;    old by = by/rescal
;        vy = vy/rescal
;       vey = vey/rescal
;       vny = vny/rescal
      gradp=(shift(p,0,-1)-shift(p,0,1))
      for j=1,ny-2 do gradp(*,j)=dy(j)*gradp(*,j)
      gradpe=(shift(pe,0,-1)-shift(pe,0,1))
      for j=1,ny-2 do gradpe(*,j)=dy(j)*gradpe(*,j)
      for i=0,nx-1 do gradpe(i,0)=0. 
      for i=0,nx-1 do gradpe(i,ny-1)=0.
      gradpex=(shift(pe,-1,0)-shift(pe,1,0))
      for j=1,nx-2 do gradpex(j,*)=dx(j)*gradpe(j,*)
      for i=0,ny-1 do gradpe(0,i)=0. 
      for i=0,ny-1 do gradpe(nx-1,i)=0.
;      ex=-vey*bz+vez*by+res*jx
;      ey=-vez*bx+vex*bz+res*jy
;      ez=-vex*by+vey*bx+res*jz
     
   if (xmin lt lnorm*x(1)) then begin
     print, 'warning! xmin:',xmin,' out of bounds:',lnorm*x(1),' Reset!'
     xmin=lnorm*x(1)
   endif  
   if (xmax gt lnorm*x(nx-2)) then begin
     print, 'warning! xmax:',xmax,' out of bounds:',lnorm*x(nx-2),' Reset!'
     xmax=lnorm*x(nx-2)
   endif  
   if (ymin lt lnorm*y(0)+h0) then begin
     print, 'warning! ymin:',ymin,' out of bounds:',lnorm*y(0),' Reset!'
     ymin=lnorm*y(0)+h0
   endif  
   if (ymax gt lnorm*y(ny-2)+h0) then begin
     print, 'warning! ymax:',ymax,' out of bounds:',lnorm*y(ny-2),' Reset!'
     ymax=lnorm*y(ny-2)+h0
   endif  
   delx=xmax-xmin & dely=ymax-ymin & sizeratio=(ymax-ymin)/(xmax-xmin)
  
  
  print, 'What unit?'
  print, 'Options: s: plot in simulation unit'
  print, '         p: plot in physical unit'
  print, '    return -> no changes applied'
  print, 'Present Choice: ', unit
  read, whatunit
  if (whatunit eq 's') or (whatunit eq 'p') then unit=whatunit
  print, 'current unit= ', unit     
    print, 'Which case?'
    read, run
  if unit eq 'p' then begin
   b_units  = bnorm                 ; in nT
   n_units  = nnorm                 ; in cm^(-3)
   v_units  = vnorm                 ; for oxygen in km/s
   vv_units = vnorm*1000.           ; vel in m/s 
   l_units  = lnorm                 ; in km
   ta_time  = tnorm                 ; in s
   j_units  = jnorm                 ; in micro A/m^2
   e_units  = enorm                 ; in micro V/m
   res_units= resnorm               ; Ohm m
   t_units  = tempnorm              ; in 1000K
   p_units  = pnorm                 ; nPa
  endif
  if unit eq 's' then begin
   b_units  = 1.0
   n_units  = 1.0
   v_units  = 1.0
   vv_units = 1000.0
   l_units  = 1.0
   ta_time  = 1.0
   j_units  = 1.0
   e_units  = 1.0
   res_units= 1.0
   t_units  = 1.0
   p_units  = 1.0
  endif
     while (again eq 'y') or (again eq '') do begin

cut:
  print, 'Input - What Cut Through The 2-D System:'
  print, 'Options: x: cut at x =const'
  print, '         y: cut at y =const'
  print, '    return -> no changes applied'
  print, '         q -> terminate'
  print, 'Present Choice: ', plane
  read, whatcut
  if whatcut eq 'q' then stop
  if whatcut eq 'x' then plane='x'
  if whatcut eq 'y' then plane='y'
  if whatcut eq '' then print,'choice=',plane,' not altered'
  if plane eq 'x' then begin 
    nplane=nx & coord=x & xtit='y' & pmax=ymax & pmin=ymin & endif
  if plane eq 'y' then begin 
    nplane=ny & coord=y & xtit='x' & pmax=xmax & pmin=xmin & endif
    
gridindex:
  print, plane, 'Coordinates:'
  for i=0,nplane-1,2 do print, i,'  ',plane,'=',coord(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nplane-1,')'
  print, 'Options:   integer -> grid index'
  print, '            return -> no changes applied'
  print, '                 c -> back to cut'
  print, '                 q -> terminate'
  print, 'Present Choice: ', igrid
  read, newgrid 
  if newgrid eq 'q' then stop
  if newgrid eq 'c' then goto, cut
  if newgrid ne '' then igrid=fix(newgrid)
  if newgrid eq '' then print,'choice=',igrid,' not altered'
  if (igrid lt 0) or (igrid gt nplane-1) then igrid=(nplane+1)/2  

;  me_e=me/qe=9.1e-31/1.6e-19
   me_e=9.1e-31/1.6e-19
   for i=0,ny-1 do begin
      for j=0,nx-1 do begin
        ey_g(j,i)=-me_e*9.8*1.e6/e_units
        ey_iner(j,i)=-me_e*9.8*1.e6/e_units*2000.
      endfor
   endfor
   lambdam = ta_time*(30/5.64)*sqrt(1./16./1836.2/dens0)
   ey_dpe=-gradpe*t_units*1.38/1.6e-2/rho/e_units
   ey_res=nuen*vy*lambdam+res*jy
   ey_veXb=-vez*bx+vex*bz
   ex_dpe=-gradpex*t_units*1.38/1.6e-2/rho/e_units
   ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx+ex_dpe
   ey=nuen*vy*lambdam-vez*bx+vex*bz+res*jy+ey_dpe
   ez=nuen*vz*lambdam-vex*by+vey*bx+res*jz
     for i=0,ny-1 do begin
        for j=0,nx-1 do begin
          angle(j,i)=atan(-ez(j,i)/ex(j,i))*180./3.1415926
          if(ex(j,i) le 0.0 ) $ 
             then angle(j,i) = angle(j,i)+180.0
          if(ex(j,i) ge 0.0 and ez(j,i) gt 0.0) $ 
             then angle(j,i) = angle(j,i)+360.0
        endfor
     endfor
     
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
   sigma0e = s0*w_ce/nuen
   sigma0 = sigma0i + sigma0e
;   exs = (sigmap*jx+sigmah*jz)/(sigmap*sigmap+sigmah*sigmah) $
;        *j_units/e_units
;   ezs = (sigmap*jz-sigmah*jx)/(sigmap*sigmap+sigmah*sigmah) $
;        *j_units/e_units
;   s0 = 1.6*5.*1.67*nuin $
;        /( 0.91E-6*(nuei+nuen)*1.67*nuin+1.6*2500.)
;   sigmap = 1.6E-8*rho*n_units/5.*s0/( 1. + s0*s0 )
;   sigmah = s0*sigmap
   jp = ( sigmap*ex-sigmah*ez )*e_units/j_units
   jh = ( sigmah*ex+sigmap*ez )*e_units/j_units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;    name = 'cut'+fnumber
;    openw, 8, name
;    printf, 8, 'time = '
;    printf, 8, time*ta_time
;    printf, 8, 'y = '
;    printf, 8, y
;    printf, 8, 'Ez = '
;    printf, 8, e_units*ey(11,*)
;    printf, 8, 'density = '
;    printf, 8, n_units*rho(11,*)
;    printf, 8, 'temperature ='
;    printf, 8, p(11,*)/rho(11,*)*t_units
;    close, 8

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        device,/landscape
        !P.THICK=2.
;        device,/inches,xsize=10.,scale_factor=1.0,xoffset=0.5
;        device,/inches,ysize=8.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,0,3,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=2
;       !X.TICKS=3
       !Y.TICKS=6
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       
        if (plane eq 'x') then begin
          plcoo=h0+lnorm*y(0:ny-1) & f1=y & f2=y & f1a=y & f2a=y 
          f3=y & f3a=y & f4=y & f5=y & f6=y & f7=y & f8=y & f9=y & f10=y
          f11=y & f11a=y & f12=y & f12a=y & fpi=y & fpe=y & f13=y & f13a=y
          f14=y & f15=y & f16=y & f14a=y & f15a=y & f16a=y 
          f17=y & f18=y & f19=y & f20=y & f21=y & f22=y & f23=y & f23a=y
	  f01=y & f02=y & f03=y & f04=y & f05=y & f06=y & f30=y
	  fvtex=y & fvtey=y & fvtez=y
	  f40=y & f41=y & f42=y & f43=y & f44=y & f50=y
	  f60=y & f61=y & f62=y & f63=y & f64=y & f65=y

	  f01=j_units*jx(igrid,*) & f02=j_units*jy(igrid,*)
	  f03=j_units*jz(igrid,*) 
	  f04=e_units*ex(igrid,*) & f05=e_units*ey(igrid,*)
	  f06=e_units*ez(igrid,*)
          f1=n_units*rhont(igrid,*) & f2=p_units*pn(igrid,*) & 
          f3=t_units*pn(igrid,*)/rhon(igrid,*)*meff(igrid,*)
          f3a=t_units*tna(igrid,*)
          f1a=n_units*(rhon(igrid,*)-rhon1(igrid,*)) & 
          f2a=p_units*pn1(igrid,*) 
          f4=vv_units*vnx(igrid,*) & f5=vv_units*vny(igrid,*) & 
          f6=vv_units*vnz(igrid,*)
          f7=res_units*res(igrid,*) & f8=nu12s(igrid,*) 
          f11=n_units*rho(igrid,*) & 
          f11a=n_units*(rho(igrid,*)-rho1(igrid,*))
          f12=p_units*p(igrid,*) & f12a=p_units*(p(igrid,*)-p1(igrid,*))
          fpi=p_units*pp(igrid,*) & fpe=p_units*pe(igrid,*)
          f13=t_units*pp(igrid,*)/rho(igrid,*)
          f13a=t_units*tia(igrid,*)
          f23= t_units*te(igrid,*)
          f23a=t_units*tea(igrid,*)
          f14=vv_units*vx(igrid,*) & f15=vv_units*vy(igrid,*) & 
          f16=vv_units*vz(igrid,*) & f14a=v_units*vex(igrid,*) & 
          f15a=v_units*vey(igrid,*) & f16a=v_units*vez(igrid,*)
          f17=b_units*bx(igrid,*) & f18=b_units*(by(igrid,*)+500.)
          f19=b_units*bz(igrid,*)
          f20=gradp(igrid,*)
          f30=bx(igrid,*)*jz(igrid,*)-jx(igrid,*)*bz(igrid,*)
          f31=n_units*rhono(igrid,*)
          f32=n_units*rhono2(igrid,*)
          f33=n_units*rhonn2(igrid,*)
          f21=t_units*(pn(igrid,*)/rhont(igrid,*)-pp(igrid,*)/rho(igrid,*))
          f22=res(igrid,*)*jy(igrid,*)*jy(igrid,*)/rho(igrid,*)
          fvtex=vex(igrid,*)*te(igrid,*)^1.5
          fvtey=vey(igrid,*)*te(igrid,*)^1.5
          fvtez=vez(igrid,*)*te(igrid,*)^1.5
          f40=sigma0(igrid,*) & f41=sigmap(igrid,*) & f42=sigmah(igrid,*)
          f43=j_units*jp(igrid,*) & f44=j_units*jh(igrid,*)
          f50=angle(igrid,*)
          f60=e_units*ey(igrid,*)
          f61=e_units*ey_iner(igrid,*)
          f62=e_units*ey_g(igrid,*)
          f63=e_units*ey_dpe(igrid,*)
          f64=e_units*ey_res(igrid,*)
          f65=e_units*ey_veXb(igrid,*)
        endif
        if (plane eq 'y') then begin
          plcoo=lnorm*x & f1=x & f2=x & f1a=x & f2a=x 
          f3=x & f3a=x & f4=x & f5=x & f6=x & f7=x & f8=x & f9=x & f10=x
          f11=x & f11a=x & f12=x & f12a=y & fpi=x & fpe=x & f13=x & f13a=x
          f14=x & f15=x & f16=x & f14a=x & f15a=x & f16a=x
          f17=x & f18=x & f19=x & f20=x & f21=x & f22=x & f23=x & f23a=x
	  f01=x & f02=x & f03=x & f04=x & f05=x & f06=x & f30=x
	  fvtex=x & fvtey=x & fvtez=x
	  f40=x & f41=x & f42=x & f43=x & f44=x & f50=x
	  f60=x & f61=x & f62=x & f63=x & f64=x & f65=x
	  f01=j_units*jx(*,igrid) & f02=j_units*jy(*,igrid)
	  f03=j_units*jz(*,igrid)
       	  f04=e_units*ex(*,igrid) & f05=e_units*ey(*,igrid)
	  f06=e_units*ez(*,igrid)
          f1=n_units*rhont(*,igrid) & f2=p_units*pn(*,igrid) & 
          f3=t_units*pn(*,igrid)/rhon(*,igrid)*meff(*,igrid)
          f3a=t_units*tna(*,igrid)
          f1a=n_units*(rhon(*,igrid)-rhon1(*,igrid)) & 
          f2a=p_units*pn1(*,igrid) 
          f4=vv_units*vnx(*,igrid) & f5=vv_units*vny(*,igrid) & 
          f6=vv_units*vnz(*,igrid)
          f7=res_units*res(*,igrid) & f8=nu12s(*,igrid) 
          f11=n_units*rho(*,igrid) & 
          f11a=n_units*(rho(*,igrid)-rho1(*,igrid))
          f12=p_units*p(*,igrid) & f12a=p_units*(p(*,igrid)-p1(*,igrid))
          fpi=p_units*pp(*,igrid) & fpe=p_units*pe(*,igrid)
          f13=t_units*pp(*,igrid)/rho(*,igrid) & 
          f13a=t_units*tia(*,igrid)
          f14=vv_units*vx(*,igrid) & f15=vv_units*vy(*,igrid) & 
          f16=vv_units*vz(*,igrid)
          f14a=v_units*vex(*,igrid) & f15a=v_units*vey(*,igrid) & 
          f16a=v_units*vez(*,igrid)
          f17=b_units*bx(*,igrid) & f18=b_units*(by(*,igrid)+500.) 
          f19=b_units*bz(*,igrid)
          f20=gradp(*,igrid)
          f30=bx(*,igrid)*jz(*,igrid)-jx(*,igrid)*bz(*,igrid)
          f31=n_units*rhono(*,igrid)
          f32=n_units*rhono2(*,igrid)
          f33=n_units*rhonn2(*,igrid)
          f21=t_units*(pn(*,igrid)/rhont(*,igrid)-pp(*,igrid)/rho(*,igrid))
          f22=res(*,igrid)*jy(*,igrid)*jy(*,igrid)/rho(*,igrid)
          f23=t_units*te(*,igrid)
          f23a=t_units*tea(*,igrid)
          fvtex=vex(*,igrid)*te(*,igrid)^1.5
          fvtey=vey(*,igrid)*te(*,igrid)^1.5
          fvtez=vez(*,igrid)*te(*,igrid)^1.5
          f40=sigma0(*,igrid) & f41=sigmap(*,igrid) & f42=sigmah(*,igrid)
          f43=j_units*jp(*,igrid) & f44=j_units*jh(*,igrid)
          f50=angle(*,igrid)
          f60=e_units*ey(*,igrid)
          f61=e_units*ey_iner(*,igrid)
          f62=e_units*ey_g(*,igrid)
          f63=e_units*ey_dpe(*,igrid)
          f64=e_units*ey_res(*,igrid)
          f65=e_units*ey_veXb(*,igrid)
        endif
    !Y.RANGE=[pmin,pmax]
    delp=pmax-pmin

    dpx=0.06 & widthx=0.25 
    xa1=0.05     & xe1=xa1+widthx 
    xa2=xe1+dpx & xe2=xa2+widthx 
    xa3=xe2+dpx & xe3=xa3+widthx 
    ylo=0.2 & yup=.8
   
    ytit='z'

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;-----------------------------------------------------------------------
; plot first page ( Rhon, Pn, Rho, P, T)
;-----------------------------------------------------------------------
      if withps ne 'y' then $
        window,0,xsize=800,ysize=600,title='Overview Plot'
	!P.POSITION=[xa1,ylo,xe1,yup]
        fmax=1.e15
        fmax=max([f1]) & print, 'Nn max: ',fmax
        i=-20
        while 10.^i lt fmax do i=i+1
        bmax = 10.^i 
        bmin=bmax*1e-10 & print, 'Nn min: ',bmin
        delb=bmax-bmin
        print,'bmin, bmax:', bmin, bmax
	plot, f1, plcoo, title='Nn (1/cm^3)', $
	xrange=[bmin,bmax],xstyle=3,ystyle=1,ytitle='z',/xlog
        oplot, f31, plcoo, line=1
        oplot, f32, plcoo, line=3
        oplot, f33, plcoo, line=2

	!P.POSITION=[xa2,ylo,xe2,yup]
        fmax=1.e7
        fmax=max([f11]) & print, 'Rho_ max: ',fmax
;        bmin=min([f11,f12]) & print, 'Rho,P min: ',bmin
        i=-20
        while 10.^i lt fmax do i=i+1
        bmax = 10.^i 
        bmin=bmax*1e-4 & print, 'Nn min: ',bmin
        delb=bmax-bmin
	plot, f11, plcoo, title='N (1/cm^3)', $
	xrange=[bmin,bmax],xstyle=3,ystyle=1,ytitle='',/xlog

	!P.POSITION=[xa3,ylo,xe3,yup]
        bmax=max([f3,f13,f23]) & print, 'T max: ',bmax
;        bmax=5000.
        bmax=fix(bmax)+1.
        bmin=min([f3,f13,f23]) & print, 'T min: ',bmin
        bmin=0. & print, 'T min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=bmax+0.01*delb
        ypos0=pmin+0.8*delp    
        ypos1=pmin+0.72*delp 
        ypos3=pmin+0.56*delp 
	plot, f3,plcoo, title=' Tn, Ti, Te (1000K)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle=''
	oplot, f13, plcoo, line=1
	oplot, f23, plcoo, line=2
	xyouts,xpos,ypos0,'time:',charsize=1.2
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.2)')+ ' s',charsize=1.2
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)'),charsize=1.2

  endif
print, 'plot page 2, Plasma pressure, vertical velocities '

;-----------------------------------------------------------------------
; plot 2nd page ( P, vy, vx vz)
;-----------------------------------------------------------------------
;        window,0,xsize=800,ysize=600,title='Overview Plot'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa1,ylo,xe1,yup]
        fmax=max([f12]) & print, 'P max: ',fmax
        i=-20
        while 10.^i lt fmax do i=i+1
        bmax = 10.^i 
        bmin=bmax*1e-6 & print, 'p min: ',bmin
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        print,'bmin, bmax:', bmin, bmax
	plot, f12, plcoo, title='Pressure (nPa)', $
	xrange=[bmin,bmax],xstyle=3,ystyle=1,ytitle='z',/xlog
        oplot, fpe, plcoo, line=1
        oplot, fpi, plcoo, line=2

	!P.POSITION=[xa2,ylo,xe2,yup]
        bmax=max([f15]) & print, 'Vy_ max: ',bmax
        bmin=min([f15]) & print, 'Vy_ min: ',bmin
        delb=bmax-bmin
	plot, f15, plcoo, title='Vert. Vel (m/s)', $
	xrange=[bmin,bmax],xstyle=3,ystyle=1,ytitle=''

	!P.POSITION=[xa3,ylo,xe3,yup]
        bmax=max([f14,f16]) & print, 'Vxz_max: ',bmax
        bmin=min([f14,f16]) & print, 'Vxz_min: ',bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=bmax+0.05*delb
        ypos0=pmin+0.8*delp    
        ypos1=pmin+0.72*delp 
        ypos3=pmin+0.56*delp 
	plot, f14,plcoo, title='V_x and V_z (m/s)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle=''
	oplot, f16, plcoo, line=2
	xyouts,xpos,ypos0,'time:',charsize=1.2
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.2)')+ ' s',charsize=1.2
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)'),charsize=1.2

  endif
print, 'plot page 3, Neutral pressure, vertical velocities '

;-----------------------------------------------------------------------
; plot  page 3 ( Pn, vny, vnx vnz )
;-----------------------------------------------------------------------

  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa1,ylo,xe1,yup]
        fmax=max([f2]) & print, 'P max: ',fmax
        i=-20
        while 10.^i lt fmax do i=i+1
        bmax = 10.^i 
        bmin=bmax*1e-10 & print, 'pn min: ',bmin
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        print,'bmin, bmax:', bmin, bmax
	plot, f12, plcoo, title='Pn (nPa)', $
	xrange=[bmin,bmax],xstyle=3,ystyle=1,ytitle='z',/xlog
        oplot, f2a, plcoo, line=1

	!P.POSITION=[xa2,ylo,xe2,yup]
        bmax=max([f5]) & bmax=25. & print, 'Vny_ max: ',bmax
        bmin=min([f5]) & bmin=-5. & print, 'Vny_ min: ',bmin
        delb=bmax-bmin
	plot, f5, plcoo, title='Vert. Neutr. Vel (m/s)', $
	xrange=[bmin,bmax],xstyle=3,ystyle=1,ytitle=''

	!P.POSITION=[xa3,ylo,xe3,yup]
        bmax=max([f4,f6]) & print, 'Vxz_max: ',bmax
        bmin=min([f4,f6]) & print, 'Vxz_min: ',bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=bmax+0.05*delb
        ypos0=pmin+0.8*delp    
        ypos1=pmin+0.72*delp 
        ypos3=pmin+0.56*delp 
	plot, f4,plcoo, title='Vn_x and Vn_z (m/s)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle=''
	oplot, f6, plcoo, line=2
	xyouts,xpos,ypos0,'time:',charsize=1.2
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.2)')+ ' s',charsize=1.2
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)'),charsize=1.2

  endif
print, 'plot page 4, Terms in Ohm`s Law'

;-----------------------------------------------------------------------
; plot  page 4 ( Terms in Ohm's Law )
;-----------------------------------------------------------------------

  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa1,ylo,xa2,yup]
        bmax=max([f60,f63,f64]) & print, 'Rho max: ',bmax
        bmin=min([f60,f63,f64]) & print, 'Rho min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmin=bmin*.9 & bmax=bmax*1.1
        delb=pmax-pmin
        xpos=bmax+.1*(bmax-bmin)
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
	plot, f60,plcoo, $
	title='Ez, Ez_res, Ez_gradpe, Ez_iner, Ez_g (uv/m)', $
;	xrange=[0.0001*bmax,bmax],xstyle=2,ystyle=1,ytitle='z'
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
	oplot, f61, plcoo, line=1
	oplot, f62, plcoo, line=2
	oplot, f63, plcoo, line=3
	oplot, f64, plcoo, line=4
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

;	!P.POSITION=[xa2,ylo,xe2,yup]
;        bmax=max([f63]) & print, 'E max: ',bmax
;        bmin=min([f63]) & print, 'E min: ',bmin
;        delb=bmax-bmin
;        if bmin eq bmax then bmax=bmin+1.0
;        delb=bmax-bmin
;	plot, f63,plcoo, title='Ey_gradpe (uv/m)', $
;;	xrange=[0.0001*bmax,bmax],xstyle=2,ystyle=1,ytitle='z'
;	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z'

;	!P.POSITION=[xe2,ylo,xe3,yup]
;        bmax=max([f60,f63,f64]) & print, 'Ve max: ',bmax
;        bmin=min([f60,f63,f64]) & print, 'Ve min: ',bmin
;;        bmin=bmin-.5 
;        delb=bmax-bmin
;        if bmin eq bmax then bmax=bmin+1.0
;        delb=pmax-pmin
;        xpos=bmax+.1*(bmax-bmin)
;        ypos0=pmin+0.8*delb    
;        ypos1=pmin+0.72*delb 
;        ypos3=pmin+0.56*delb 
;	plot, f60,plcoo, $
;	title='Ez, Ez_res, Ez_gradpe, Ez_iner, Ez_g (uv/m)', $
;;	xrange=[0.0001*bmax,bmax],xstyle=1,ystyle=1,ytitle='z'
;	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
;	oplot, f61, plcoo, line=1
;	oplot, f62, plcoo, line=2
;	oplot, f63, plcoo, line=3
;	oplot, f64, plcoo, line=4
;	xyouts,xpos,ypos0,'time:'
;	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)') + ' s'
;	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
  
print, 'plot page 3, sigmap,sigmah,vex'

;-----------------------------------------------------------------------
; plot  page 2 ( Sigmap, Sigmah, Ve, )
;-----------------------------------------------------------------------

       !P.MULTI=[0,0,3,0,0]
       !P.FONT=3

  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa1,ylo,xe1,yup]
;        bmax=1.e-3
;        bmin=1.e-9
        bmax=max([f41]) & print, 'Rho max: ',bmax
        bmin=min([f41]) & print, 'Rho min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
	plot_oi, f41,plcoo, title='Pedersen Conductivity', $
	xrange=[0.0001*bmax,bmax],xstyle=2,ystyle=1,ytitle='z'

	!P.POSITION=[xa2,ylo,xe2,yup]
        bmax=max([f42]) & print, 'E max: ',bmax
        bmin=min([f42]) & print, 'E min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
	plot_oi, f42,plcoo, title='Hall Conductivity', $
	xrange=[0.0001*bmax,bmax],xstyle=2,ystyle=1,ytitle='z'

	!P.POSITION=[xa3,ylo,xe3,yup]
        bmax=max([f40]) & print, 'Ve max: ',bmax
        bmin=min([f40]) & print, 'Ve min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=pmax-pmin
        xpos=bmax+5.*(bmax-bmin)
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
;	plot, f14a,plcoo, title=' Ve Components (Km/s)', $
	plot_oi, f40,plcoo, title='Parallel Conductivity', $
;	xrange=[0.0001*bmax,bmax],xstyle=2,ystyle=1,ytitle='z'
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z'
;	oplot, f15a, plcoo, line=1
;	oplot, f16a, plcoo, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.2)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
  

     !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


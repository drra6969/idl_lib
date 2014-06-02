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
  xmax =   12. & ymax = 600.
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
   vnx=bx & vny=bx & vnz=bx & meff=bx & nu12s=bx & nu12a=bx
   rho=bx & srho=bx & rho1=bx & u=bx & u1=bx
   te=bx & ste=bx & ti=bx & t0=bx
   rhono=bx & rhono2=bx & rhonn2=bx & tea=bx &tn=bx & tna=bx & tia=bx
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   p=bx & pp=bx & pe=bx & pn=bx & p1=bx & pn1=bx
   gradp=bx & gradpe=bx & gradpex=bx
   dve=bx & dvex=bx & dvey=bx & gtex=bx & gtey=bx
   nuei=bx & nuen=bx & nuin=bx & res=bx & ioniz=bx & ioniz0=bx
   cli=bx & clei=bx & clen=bx & clo1d=bx
   aa=bx & cln2=bx & clo2=bx & clo=bx
   cln2v=bx & clo2v=bx & clof=bx & cln2r=bx & clo2r=bx
   rjj=bx & hcd=bx & cnv=bx
   ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
   exs=bx & eys=bx & ezs=bx & angle=bx & ex_dpe=bx
   ey_iner=bx & ey_g=bx & ey_dpe=bx & ey_res=bx & ey_veXb=bx
   sigmap=bx & sigmah=bx & s0=bx & jp=bx & jh=bx
   sigmapi=bx & sigmape=bx & sigmahi=bx & sigmahe=bx
   sigma0=bx & sigma0i=bx & sigma0e=bx & ff=bx
   d1=bx & d2=bx & d3=bx & dnm=bx & lambdae=bx & lbd=bx & tek=bx
   f1=bx & f2=bx & f3=bx & f4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin,ioniz ; ,ioniz0
   readu, 8,  jx,jy,jz
   close, 8

   for j=0,nx-1 do ste(j,*)=smooth(te(j,*),5)
   for j=0,nx-1 do srho(j,*)=smooth(rho(j,*),7)
;   vx=vx/rho & vz=vz/rho & vy=vy/rho             ; without smooth rho
;   vex=vex/rho & vez=vez/rho & vey=vey/rho       ; without smooth rho
   vx=vx/srho & vz=vz/srho & vy=vy/srho           ; using smooth rho
   vex=vex/srho & vez=vez/srho & vey=vey/srho     ; using smooth rho
   p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0)
   pe=rho*te & pp=p-pe & t0=p1/rho1/2. & ti=pp/rho
   for j=0,nx-1 do pe(j,*)=smooth(pe(j,*),5)

   name='magnap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhon1,un1,nu12s,nu12a
   close, 8
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon

   pn=2*un^(5.0/3.0) & pn1=2*un1^(5.0/3.0)
   rhont=rhono+rhono2/2+rhonn2*4/7
   meff = rhon1/rhont
   tn = pn/rhont
   tna = pn1/rhont & tia=tna & tea=p1/rho1-tia

     y = h0 + y/rescal
     dy = dy*rescal
     by = by/rescal
     vy = vy/rescal
     vey = vey/rescal
     vny = vny/rescal
;      for j=0,5 do te(*,j)=tn(*,j)
      gradp=(shift(p,0,-1)-shift(p,0,1))
      for j=1,ny-2 do gradp(*,j)=dy(j)*gradp(*,j)
      gradpe=(shift(pe,0,-1)-shift(pe,0,1))
      for j=1,ny-2 do gradpe(*,j)=dy(j)*gradpe(*,j)
      for i=0,nx-1 do gradpe(i,0)=0. 
      for i=0,nx-1 do gradpe(i,ny-1)=0.
      gradpex=(shift(pe,-1,0)-shift(pe,1,0))
      for j=1,nx-2 do gradpex(j,*)=dx(j)*gradpex(j,*)
      for i=0,ny-1 do gradpex(0,i)=0. 
      for i=0,ny-1 do gradpex(nx-1,i)=0.
;      gtex=(shift(ste,-1,0)-shift(ste,1,0))        ;  smooth te
      gtex=(shift(te,-1,0)-shift(te,1,0))           ;  no smooth te
      for j=1,nx-2 do gtex(j,*)=dx(j)*gtex(j,*)
      for i=0,ny-1 do gtex(0,i)=0. 
      for i=0,ny-1 do gtex(nx-1,i)=0.
;      gtey=(shift(ste,0,-1)-shift(ste,0,1))        ;  smooth te
      gtey=(shift(te,0,-1)-shift(te,0,1))           ;  no smooth te
      for j=1,ny-2 do gtey(*,j)=dy(j)*gtey(*,j)
      for i=0,nx-1 do gtey(i,0)=0. 
      for i=0,nx-1 do gtey(i,ny-1)=0.
      dvex=(shift(vex,-1,0)-shift(vex,1,0))
      for j=1,nx-2 do dvex(j,*)=dx(j)*dvex(j,*)
      for i=0,ny-1 do dvex(0,i)=0. 
      for i=0,ny-1 do dvex(nx-1,i)=0.
      dvey=(shift(vey,0,-1)-shift(vey,0,1))
      for j=1,ny-2 do dvey(*,j)=dy(j)*dvey(*,j)
      for i=0,nx-1 do dvey(i,0)=0. 
      for i=0,nx-1 do dvey(i,ny-1)=0.
      dve = dvex + dvey
     
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
;   ey=nuen*vy*lambdam+res*jy
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;---------------------------------------------------------------------
;  Different terms contribute to electron heating and cooling
;---------------------------------------------------------------------
;  All units are eV/cm3/sec

   k=8.6173e-5                     ; eV/K
   tek=te*t_units
;  (1) clo1d (red line emission)
;       clo1d=1.07e-10*rho*rhono*n_units*n_units $  ;  eV/cm3/sec
       clo1d=1.07e-10*rhono*n_units $               ;  in eV/sec
             *sqrt(tek*1000.0)*exp(-22.7/tek) $
             *(0.406+0.0357*tek-(0.333+0.0183*tek)*exp(-13.7/tek) $
                               -(0.456+0.0174*tek)*exp(-29.7/tek) )
;  (2) clei
       for i=0,ny-1 do begin
       for j=0,nx-1 do begin
          clei(j,i)=0.0
          if ( te(j,i) gt ti(j,i) ) then $ 
;             clei(j,i) = 3.2e-5*rho(j,i)*rho(j,i)*n_units*n_units $
             clei(j,i) = 3.2e-5*rho(j,i)*n_units $   ;  in eV/sec
                        *(0.75+0.25*tanh((y(i)-250.0)/50.0))*15.0 $
                        *(te(j,i)-ti(j,i))*t_units $
                        /(tek(j,i)*1000.0)^1.5
       endfor
       endfor

;  Contribution from different neutral terms
       for i=0,ny-1 do begin
       for j=0,nx-1 do begin
          if ( te(j,i)*t_units lt 1.0 ) then $ 
             aa(j,i) = 5.71e-8*exp(-3.3526/tek(j,i))
          if ( tek(j,i) ge 1.0 ) $ 
             and ( tek(j,i) le 2.0 ) then $
             aa(j,i) = 2.0e-7*exp(-4.6052/tek(j,i))
          if ( tek(j,i) gt 2.0 ) then $
             aa(j,i) = 2.53e-5*sqrt(tek(j,i)*10.0) $
                      *exp(-17.62/tek(j,i))
       endfor
       endfor     
       for i=0,ny-1 do begin
       for j=0,nx-1 do begin
          cln2(j,i) = 0.0
          clo2(j,i) = 0.0
          clo(j,i) = 0.0
          cln2v(j,i) = 0.0
          clo2v(j,i) = 0.0
          clof(j,i) = 0.0
          cln2r(j,i) = 0.0
          clo2r(j,i) = 0.0
          if ( te(j,i) gt tn(j,i) ) then begin $ 
            cln2(j,i) = 1.77e-3*rhonn2(j,i)/1.75 $  
                       *(1.0-0.121*tek(j,i))*tek(j,i)
            clo2(j,i) = 1.21e-4*rhono2(j,i)/2.0 $ 
                       *(1.0+0.36*sqrt(tek(j,i)*10.0))*sqrt(tek(j,i)*10.0)
            clo(j,i) = 7.9e-5*rhono(j,i) $ 
                      *(1.0+0.57*tek(j,i))*sqrt(tek(j,i)*10.0)
            cln2v(j,i) = 1.3e+6*rhonn2(j,i)/1.75*aa(j,i) $
                        *(1.0-exp(3.2*(1.0/tek(j,i)-1.0/tn(j,i)/t_units)))
            clo2v(j,i) = 3.125e-2*rhono2(j,i)/2.0 $
                        *tek(j,i)*tek(j,i)
            clof(j,i) = 3.4e-2*rhono(j,i)*(1.0-0.07*tek(j,i)) $
                       *(0.15/tek(j,i)+0.4)/tn(j,i)/t_units
            cln2r(j,i) = 2.9e-2*rhonn2(j,i)/1.75 $
                        /sqrt(tek(j,i)*10.0)
            clo2r(j,i) = 6.9e-2*rhono2(j,i)/2.0 $
                        /sqrt(tek(j,i)*10.0)
          endif
       endfor
       endfor
       
;  (3) clen (Total cooling from neutral)
       for i=0,ny-1 do begin
       for j=0,nx-1 do begin
          clen(j,i) = ( ( cln2(j,i) + clo2(j,i) + clo(j,i) + clo2v(j,i) $
                         + clof(j,i) + cln2r(j,i) + clo2r(j,i) ) $
                       *(tek(j,i)-tn(j,i)*t_units) + cln2v(j,i) ) $
                     *rho(j,i)*n_units*n_units*1.0e-10   ;  in eV/cm3/sec
;                     *n_units*1.0e-10                     ;  in eV/sec
       endfor
       endfor

;  (4) rjj=2*res*j*j
       rjj=2.0*res*(jx*jx+jy*jy+jz*jz)*n_units*k*t_units/ta_time*1000.0  ;$
;           /rho/n_units                                   ;  in eV/sec

;  (5) hcd (heat conduction)
       d1=(2.82-.341*te*t_units)*sqrt(te*t_units*10.0)*1.0e-6
       d2=2.2e-6+7.92e-7*sqrt(te*t_units*10.0)
       d3=1.1e-6*(1.0+.57*te*t_units)
       dnm=1.0+3.22*te*te*t_units*t_units/rho $
               *(d1*rhonn2/1.75+d2*rhono2/2.0+d3*rhono)
       lambdae=7.5*(te*t_units*10.0)^2.5/dnm
       lbd=lambdae*1.0e10
       lambdae=lambdae*gtey*t_units*1000.0/l_units
       hcd=(shift(lambdae,0,-1)-shift(lambdae,0,1))
       for j=1,ny-2 do hcd(*,j)=dy(j)*hcd(*,j)
       for i=0,nx-1 do hcd(i,0)=0. 
       for i=0,nx-1 do hcd(i,ny-1)=0.
       hcd=hcd/l_units                                  ;  in  eV/cm3/sec
;       hcd=hcd/l_units/rho/n_units                       ;  in eV/sec

;  (6) cnv (convection term)
       cnv=-k*(te*dve+1.5*(vex*gtex+vey*gtey)) $
           *rho*n_units*t_units*1000.0*v_units/l_units   ;  in eV/cm3/sec
;           *t_units*1000.0*v_units/l_units              ;  in eV/sec

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
  print, plane, 'Coordinates:
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


;---------------------------------------------------------------------
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
       !P.CHARSIZE=1.0
       !P.FONT=3
       !X.TICKS=4
       !Y.TICKS=5
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       
        if (plane eq 'x') then begin
          plcoo=y(0:ny-1) & f1=y & f2=y & f3=y 
          f7=y & f8=y
          f11=y & f12=y & f13=y 
          fpi=y & fpe=y & fne=y
          f14=y & f15=y & f16=y 
          f17=y & f18=y & f19=y
;          f24=y & f25=y & f26=y 
          f31=y & f32=y & f33=y & f34=y & f35=y & f36=y & f37=y & f38=y
	  f01=y & f02=y & f03=y & f04=y & f05=y & f06=y
	  f40=y & f41=y & f42=y & f43=y & f44=y
	  f60=y & f61=y & f62=y & f63=y & f64=y & f65=y
	  f70=y & f71=y
	  f01=j_units*jx(igrid,*) & f02=j_units*jy(igrid,*)
	  f03=j_units*jz(igrid,*) 
	  f04=e_units*ex(igrid,*) & f05=e_units*ey(igrid,*)
	  f06=e_units*ez(igrid,*)
	  fne=nuen(igrid,*)/ta_time
          f1=n_units*rhont(igrid,*) & f2=p_units*pn(igrid,*) & 
          f3=t_units*pn(igrid,*)/rhon(igrid,*)*meff(igrid,*)
          f7=res_units*res(igrid,*) & f8=nu12s(igrid,*) 
          f11=n_units*rho(igrid,*) & f12=p_units*p(igrid,*)
          fpi=p_units*pp(igrid,*) & fpe=p_units*pe(igrid,*)
          f13=t_units*pp(igrid,*)/rho(igrid,*)
          f14=v_units*vx(igrid,*) & f15=v_units*vy(igrid,*) 
          f16=v_units*vz(igrid,*)
          f17=b_units*bx(igrid,*) & f18=b_units*(by(igrid,*)+500.)
          f19=b_units*bz(igrid,*)
          f23= t_units*te(igrid,*)
;          f24=v_units*vex(igrid,*)
;          f25=v_units*vey(igrid,*) & f26=v_units*vez(igrid,*)
          f31=rjj(igrid,*) 
          f32=ioniz(igrid,*)*( 1.75 - 1500.0*k*tek(igrid,*) ) ;$
;              /rho(igrid,*)/n_units
          f33=-clo1d(igrid,*)
          f34=-clen(igrid,*)
          f35=-clei(igrid,*)
          f36=cnv(igrid,*) & f37=hcd(igrid,*)
          f38=f31+f32+f33+f34+f35+f36
          f40=sigma0(igrid,*) & f41=sigmap(igrid,*) & f42=sigmah(igrid,*)
          f43=j_units*jp(igrid,*) & f44=j_units*jh(igrid,*)
          f60=e_units*ey(igrid,*)
          f61=e_units*ey_iner(igrid,*)
          f62=e_units*ey_g(igrid,*)
          f63=e_units*ey_dpe(igrid,*)
          f64=e_units*ey_res(igrid,*)
          f65=e_units*ey_veXb(igrid,*)
          f70=ioniz(igrid,*) & f71=ioniz0(igrid,*)
        endif
        if (plane eq 'y') then begin
          plcoo=x & f1=x & f2=x & f3=x
          f7=x & f8=x
          f11=x & f12=x & f13=x
          fpi=x & fpe=x & fne=x
          f14=x & f15=x & f16=x
          f17=x & f18=x & f19=x
;          f24=x & f25=x & f26=x
          f31=x & f32=x & f33=x & f34=x & f35=x & f36=x & f37=x & f38=x
	  f01=x & f02=x & f03=x & f04=x & f05=x & f06=x
	  f40=x & f41=x & f42=x & f43=x & f44=x
	  f60=x & f61=x & f62=x & f63=x & f64=x & f65=x
	  f70=x & f71=x
	  f01=j_units*jx(*,igrid) & f02=j_units*jy(*,igrid)
	  f03=j_units*jz(*,igrid)
       	  f04=e_units*ex(*,igrid) & f05=e_units*ey(*,igrid)
	  f06=e_units*ez(*,igrid)
	  fne=nuen(*,igrid)/ta_time
          f1=n_units*rhont(*,igrid) & f2=p_units*pn(*,igrid)
          f3=t_units*pn(*,igrid)/rhon(*,igrid)*meff(*,igrid)
          f7=res_units*res(*,igrid) & f8=nu12s(*,igrid) 
          f11=n_units*rho(*,igrid) & f12=p_units*p(*,igrid)
          fpi=p_units*pp(*,igrid) & fpe=p_units*pe(*,igrid)
          f13=t_units*pp(*,igrid)/rho(*,igrid)
          f14=v_units*vx(*,igrid) & f15=v_units*vy(*,igrid) & 
          f16=v_units*vz(*,igrid)
          f17=b_units*bx(*,igrid) & f18=b_units*(by(*,igrid)+500.) 
          f19=b_units*bz(*,igrid)
          f23=t_units*te(*,igrid)
;          f24=v_units*vex(*,igrid)
;          f25=v_units*vey(*,igrid) & f26=v_units*vez(*,igrid)
          f31=rjj(*,igrid)
          f32=ioniz(*,igrid)*( 1.75 - 1500.0*k*tek(*,igrid) ) $
              /rho(*,igrid)/n_units
          f33=-clo1d(*,igrid)
          f34=-clen(*,igrid) 
          f35=-clei(*,igrid)
          f36=cnv(*,igrid) & f37=hcd(*,igrid)
          f38=f31+f32+f33+f34+f35+f36
          f40=sigma0(*,igrid) & f41=sigmap(*,igrid) & f42=sigmah(*,igrid)
          f43=j_units*jp(*,igrid) & f44=j_units*jh(*,igrid)
          f60=e_units*ey(*,igrid)
          f61=e_units*ey_iner(*,igrid)
          f62=e_units*ey_g(*,igrid)
          f63=e_units*ey_dpe(*,igrid)
          f64=e_units*ey_res(*,igrid)
          f65=e_units*ey_veXb(*,igrid)
          f70=ioniz(*,igrid) & f71=ioniz0(*,igrid)
        endif
    !Y.RANGE=[pmin,pmax]

    dpx=0.07 & widthx=0.22 
    xa1=0.08     & xe1=xa1+widthx 
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

	!P.POSITION=[xa1,ylo,xe1,yup]
        bmax=1.e14
        bmin=1.e2
;        bmax=max([f1,f2]) & print, 'rhon,pn max: ',bmax
;        bmin=min([f1,f2]) & print, 'rhon,pn min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
	plot_oi, f1, plcoo, title='Nn (1/cm^3), Pn (nPa)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z'
	oplot, f2, plcoo, line=1

	!P.POSITION=[xa2,ylo,xe2,yup]
        bmax=max([fne,f11]) & print, 'Rho,P max: ',bmax
        bmin=min([fne,f11]) & print, 'Rho,P min: ',bmin
        bmax=6.e5
       delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
;	plot_oi, f11, plcoo, title='N (1/cm^3), P (nPa)', $
	plot_oi, fne, plcoo, title=' nuen (1/s)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z'
	oplot, f11, plcoo, line=2
;	oplot, fpe, plcoo, line=1

	!P.POSITION=[xa3,ylo,xe3,yup]
        bmax=max([f3,f13,f23]) & print, 'T max: ',bmax
;        bmax=fix(bmax)+1.
;        bmax = 0.5
;        bmin=min([f3,f13,f23]) & print, 'T min: ',bmin
        bmin=0. & print, 'T min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=pmax-pmin
        xpos=bmax+0.5
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
	plot, f3,plcoo, title=' Tn, Ti, Te (1000K)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z'
	oplot, f13, plcoo, line=1
	oplot, f23, plcoo, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
print, 'plot page 2, Terms in Ohm`s Law'

;-----------------------------------------------------------------------
; plot  page 2 ( Terms in Ohm's Law )
;-----------------------------------------------------------------------

       !P.MULTI=[0,0,2,0,0]
       !P.FONT=2
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa1,ylo,xa2,yup]
;        bmax=max([f60,f63,f64]) & print, 'Rho max: ',bmax
;        bmin=min([f60,f63,f64]) & print, 'Rho min: ',bmin
        bmax=max([f02]) & print, 'Rho max: ',bmax
        bmin=min([f02]) & print, 'Rho min: ',bmin
        delb=bmax-bmin
        if (bmin eq bmax) then begin
        bmin=bmin*.9 & bmax=bmax*1.1
        endif
        delb=pmax-pmin
        xpos=bmax+.1*(bmax-bmin)
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
	plot, f02,plcoo, $
;	title='Ez, Ez_res, Ez_gradpe, Ez_iner, Ez_g (uv/m)', $
	title='Jz (uA/m^2)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
;	oplot, f61, plcoo, line=1
;	oplot, f62, plcoo, line=2
;	oplot, f63, plcoo, line=3
;	oplot, f64, plcoo, line=4
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xe2,ylo,xe3,yup]
;        bmax=max([f60,f63,f64]) & print, 'Rho max: ',bmax
;        bmin=min([f60,f63,f64]) & print, 'Rho min: ',bmin
        bmax=max([f60]) & print, 'Rho max: ',bmax
        bmin=min([f60]) & print, 'Rho min: ',bmin
;        bmax = 0.5
;        bmin = -5.0
        delb=bmax-bmin
;        if bmin eq bmax then bmin=bmin*.9 & bmax=bmax*1.1
        delb=pmax-pmin
        xpos=bmax+.1*(bmax-bmin)
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
	plot, f60,plcoo, $
;	title='Ez, Ez_res, Ez_gradpe, Ez_iner, Ez_g (uv/m)', $
	title='Ez (uv/m)', $
;	title='By (nT)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
;	oplot, f61, plcoo, line=1
;	oplot, f62, plcoo, line=2
;	oplot, f63, plcoo, line=3
;	oplot, f64, plcoo, line=4
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
print, 'plot page 3, Terms in Electron Heating'

;-----------------------------------------------------------------------
; plot  page 2 ( Terms in Electron Heating )
;-----------------------------------------------------------------------

       !P.MULTI=[0,0,1,0,0]
       !P.FONT=2
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xe1,ylo,xa3,yup]
        bmax=max([f31,f32,f33,f34,f35,f36,f37]) & print, 'Rho max: ',bmax
        bmin=min([f31,f32,f33,f34,f35,f36,f37]) & print, 'Rho min: ',bmin
;        bmax=1.0
;        bmin=-1.0
        delb=bmax-bmin
        if (bmin eq bmax) then begin 
           bmin=bmin*.9
           bmax=bmax*1.1
        endif
        delb=pmax-pmin
        xpos=bmax+.1*(bmax-bmin)
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
	plot, f31,plcoo, $
	title='rjj, ioniz, co1d, clen, clei, cnv, hcd (eV/s)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
	oplot, f32, plcoo, line=1
	oplot, f33, plcoo, line=2
	oplot, f34, plcoo, line=3
	oplot, f35, plcoo, line=4
	oplot, f36, plcoo, line=5
	oplot, f37, plcoo, line=1.5
	oplot, f38, plcoo, line=1,thick=3
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

;	!P.POSITION=[xa2,ylo,xe2,yup]
;        bmax=max([f33,f34,f35]) & print, 'Rho max: ',bmax
;        bmin=min([f33,f34,f35]) & print, 'Rho min: ',bmin
;        delb=bmax-bmin
;        if bmin eq bmax then bmin=bmin*.9 & bmax=bmax*1.1
;        delb=pmax-pmin
;        xpos=bmax+.1*(bmax-bmin)
;        ypos0=pmin+0.8*delb    
;        ypos1=pmin+0.72*delb 
;        ypos3=pmin+0.56*delb 
;	plot, f33,plcoo, $
;	title='clo1d,clen,clei', $
;	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
;	oplot, f34, plcoo, line=1
;	oplot, f35, plcoo, line=2
;	xyouts,xpos,ypos0,'time:'
;	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
;	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

;	!P.POSITION=[xa3,ylo,xe3,yup]
;        bmax=max([f36,f37]) & print, 'Rho max: ',bmax
;        bmin=min([f36,f37]) & print, 'Rho min: ',bmin
;        delb=bmax-bmin
;        if bmin eq bmax then bmin=bmin*.9 & bmax=bmax*1.1
;        delb=pmax-pmin
;        xpos=bmax+.1*(bmax-bmin)
;        ypos0=pmin+0.8*delb    
;        ypos1=pmin+0.72*delb 
;        ypos3=pmin+0.56*delb 
;	plot, f36,plcoo, $
;	title='cnv,hcd', $
;	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
;	oplot, f37, plcoo, line=1
;	xyouts,xpos,ypos0,'time:'
;	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
;	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
;
  endif
  
print, 'plot page 2, Resistivity'

;-----------------------------------------------------------------------
; plot  page 2 ( Resistivity )
;-----------------------------------------------------------------------

       !P.MULTI=[0,0,2,0,0]
       !P.FONT=2
  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa1,ylo,xa2,yup]
        bmax=max([f7]) & print, 'Res max: ',bmax
        bmin=min([f7]) & print, 'Res min: ',bmin
;        bmax=2.5
        delb=bmax-bmin
        if bmin eq bmax then bmin=bmin*.9 & bmax=bmax*1.1
        delb=pmax-pmin
        xpos=bmax+.1*(bmax-bmin)
        ypos0=pmin+0.8*delb    
        ypos1=pmin+0.72*delb 
        ypos3=pmin+0.56*delb 
	plot_oi, f7,plcoo, $
	title='Resistivity (Ohm m)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

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
;        bmax=0.0004 & print, 'Sp max: ',bmax
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
;	plot_oi, f41,plcoo, title='Pedersen Conductivity', $
	plot, f41,plcoo, title='Pedersen Conductivity', $
	xrange=[0.0,bmax],xstyle=1,ystyle=1,ytitle='z'

	!P.POSITION=[xa2,ylo,xe2,yup]
        bmax=max([f42]) & print, 'E max: ',bmax
        bmin=min([f42]) & print, 'E min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
;	plot_oi, f42,plcoo, title='Hall Conductivity', $
	plot, f42,plcoo, title='Hall Conductivity', $
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
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
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


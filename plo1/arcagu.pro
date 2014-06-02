; START OF PROGRAM

  nr=101 
  r=findgen(nr) & rkm=r
  j=fltarr(nr,/NOZERO) & the=j & n=j & nl=j & m=j 
  b0=j & lb=j & db=j & dx=j 
  dcs=j & js=j & vash=j & vab0=j & vab1=j & tash=j & eash=j & ec=j 
;-------------
; PARAMETER:
;-------------
; 1) AREA
;    l0-McIlwain Par; r0,r1-radial bounds in RE; re-RE
  re=6400.0
  l0=7 & r0=1.016 & r1=5.0
  dr=(r1-r0)/float(nr-1) & r=dr*r+r0 & rkm=(r-1.0)*re

; 2) Dipole field strength
;    b0-dipole field in nT; the-dipole latitude from north; 
;    lb-field line length for l0=const in RE;
  the = 180.0/3.14159*asin(sqrt(r/l0))
  fromzenith=180.0/3.14159*atan( 0.5*3.14159/180.0*the(0) )
  print, 'latitude from north    =', the(0)
  print, 'field elev from zenith =', fromzenith
  b0 = 29500.0*(4.0-3.0*r/l0)/r^3.0
  lb = re*l0*4.0/3.0*( $ 
          sqrt( (1.0-0.75*r0/l0)*(1.0-r0/l0) )$
        - sqrt( (1.0-0.75*r/l0)*(1.0-r/l0) )$
        + 0.5/sqrt(3.0)*alog( $
              (sqrt(0.75)*sqrt(1.0-r0/l0)+sqrt(1.0-0.75*r0/l0))$
             /(sqrt(0.75)*sqrt(1.0-r/l0)+sqrt(1.0-0.75*r/l0)) )  )

; 3) Mass density
;    n-number density r>1.05 !!!; m-effective ion mass;
  n0=10.0^5 & z0=200.0 & nmsp=5.0 & nbl=2.0
;  n = 5.0*n0/( exp(-2.173*(rkm-z0)/100.0)+exp(0.522*(rkm-z0)/100.0) )$
  n = 3.0*n0/( exp(-(rkm-z0)/50.0)+exp((rkm-z0)/500.0) )$
      + nmsp/(r-1.0)^1.5 + nbl/cosh(0.5*(r-10.0))
;  nl = n0/( exp(-4.173*(rkm-z0)/100.0)+exp(0.522*(rkm-z0)/100.0) )$
  nl = n0/( exp(-(rkm-z0)/50.0)+exp((rkm-z0)/500.0) )$
      + nmsp/(r-1.0)^1.5 + nbl/cosh(0.5*(r-10.0))
;Lysak:  
;  nl = 0.5*n0*exp( -10.0*(r-1.05) ) + nmsp/(r-1.0)^1.5
  z0=700.0 
  m = 0.0*m+8.0+3.5*( 1.0 - tanh((rkm-z0)/500.0) )

; 4) Electron Temperature in degrees Kelvin
  z0=150.0
  temp= 2800.0/( exp(-(rkm-z0)/20.0)+exp(-(rkm-z0)/3000.0) + 0.001)
  templ= 1500.0/( exp(-(rkm-z0)/20.0)+exp(-(rkm-z0)/3000.0) + 0.001)

; 5) Alfven velocity
;    vab0-Alfven vel for b0 in km/s; 
    vab0 = 21.8*b0/sqrt(n*m)
    vab1 = 21.8*b0/sqrt(nl*m)

; 6) Magnetic shear and field aligned current
;    db-magn shear in nT;
;    db1-magn. shear at 1; br0,br1-magn field at bounds;
;    dcs-current sheet width in km; dcs1-curr sheet width in km at 1;
;    js-sheet current in A/m; j-current density in A/m^2
  db0=100 & dcs0=5.0
  br0=29500.0*(4-3*r0/l0)/r0^3
  br1=29500.0*(4-3*r1/l0)/r1^3
  db = db0*sqrt(b0/br0)
  db1 = db0*(b0/br0)^(1.0/3.0)
  dcs = dcs0*sqrt(br0/b0)
  dcs1 = dcs0*sqrt(br0/b0)/sqrt(r/r0)
  js = 0.796/10^3*db
  j = js/1000.0/dcs
;   dx-footpoint displacement in RE;
  dx = db/sqrt(br0*b0)*lb
  dx1 = db/sqrt(br0*b0)*((lb/re+1.0)^1.5-1.0)*re

; 7) Alfven velocity and electric field and Alfven travel time
;    vash-Alfven vel for db in km/s; 
;    tash-Alfven time for db amd dcs in s; 
;    eash, ec-conv electric field for vash in mV/m;
  vash = 21.8*db/sqrt(n*m)
  vash1 = 21.8*db1/sqrt(n*m)
  tash = dcs/vash
  tash1 = dcs1/vash1
  eash = vash*db/1000.0
  eash1 = vash1*db1/1000.0
  ec = vash*b0/1000.0
  ec1 = vash1*b0/1000.0

; 8) Neutral Density

  nn0=10.0^10 & zn0=140.0 
  nn = 2.0*nn0*( exp(-(rkm-z0)/5.0)+exp(-(rkm-z0)/120.0) )+1.0
  nn1 = 0.5*nn0*( exp(-(rkm-z0)/5.0)+exp(-(rkm-z0)/80.0) )+1.0
  mn = 8.0+3.5*( 1.0 - tanh((rkm-z0)/50.0) )

; 9) Collision frequencies
; nei=electron-ion,  nin=ion-neutral, nen=electron-neutral

  nei=55.0*n/temp^1.5
  nei1=55.0*nl/templ^1.5
  nen=10.0^(-10)*nn*(1.0+0.6/1000.0*temp)*sqrt(temp)      ;for O
  nen1=10.0^(-10)*nn1*(1.0+0.6/1000.0*templ)*sqrt(templ)
  nin=5.0*10.0^(-10)*nn          ;rough guess for an average
  nin1=5.0*10.0^(-10)*nn1        ;rough guess for an average

;10) Normalized collision frequencies
; noei=nei*tash, 
; note noin gives plasma neutral friction 
;    compared to electromagnetic forces
  noei=nei*tash
  noei1=nei1*tash1
  noen=nen*tash
  noen1=nen1*tash1
  noin=nin*tash
  noin1=nin1*tash1

;11) Electron Inertia term compared to Electromagnetic terms

  lambda=(5.3/sqrt(n)/dcs)^2
  lambda1=(5.3/sqrt(nl)/dcs)^2

;12) Normalised resitivities (Inverse=magnetic Reynolds Number)

  etaei=noei*lambda
  etaei1=noei1*lambda1
  etaen=noen*lambda
  etaen1=noen1*lambda1
  etain=noin*lambda
  etain1=noin1*lambda1
; total:
  eta=etaei+etaen+etain
  eta1=etaei1+etaen1+etain1

;13) Properties
; E in micro V/m

  taudiff=tash/eta
  taudiff1=tash1/eta1
  taurek=tash/sqrt(eta)
  taurek1=tash1/sqrt(eta1)
  ediff=eta*eash*1000.0
  ediff1=eta1*eash1*1000.0
  erek=sqrt(eta)*eash*1000.0
  erek1=sqrt(eta)*eash1*1000.0



    again='y' & withps='n' & contin='y'
    while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='arc.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot 1. page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=2  
    !P.MULTI=[0,0,4]
    !P.POSITION=[0.2,0.78,0.7,0.98]
    amax = max(b0)
    amin = min(b0)
    plot_io, r, b0,$
        title='Dipole Field strength B [nT]',$
        font=3, yrange=[amin, amax]        
    !P.POSITION=[0.2,0.53,0.7,0.73]
    amax = max(lb)
    amin = min(lb)
    plot, r, lb,$
        title='Length of Fieldline [km]',$
        font=3, yrange=[amin, amax], ystyle=1
    !P.POSITION=[0.2,0.28,0.7,0.48]
    amax = max([n,nl])
    amin = min([n,nl])
    plot_io, r, n,$
        title='Number Density [cm!U-3!N]',$
        font=3, yrange=[amin, amax]
;    oplot, r, nl, line=2
;    oplot, k, q(2,0:nx-1), line=2
;    oplot, k, q(3,0:nx-1), line=3
    !P.POSITION=[0.2,0.03,0.7,0.23]
    amax = max([vab0,vab1])
    amin = min([vab0,vab1])
    plot_io, r, vab0,$
        title='Alfven Velocity in Dipole Field [km s!U-1!N]',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]
;    oplot, r, vab1, line=2
  endif


  print, 'plot 2. page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=2  
    !P.MULTI=[0,0,4]
    !P.POSITION=[0.2,0.78,0.7,0.98]
    amax = max([db,db1])
    amin = min([db,db1])
    plot, r, db,$
        title='Shear Magnetic Field [nT]',$
        font=3, yrange=[0, amax], ystyle=1
    oplot, r, db1, line=2
    !P.POSITION=[0.2,0.53,0.7,0.73]
    amax = max([dcs,dcs1])
    amin = min([dcs,dcs1])
    plot, r, dcs,$
        title='Current Sheet Width [km]',$
        font=3, yrange=[0,amax], ystyle=1
    oplot, r, dcs1, line=2
    !P.POSITION=[0.2,0.28,0.7,0.48]
    amax = max([dx,dx1])
    amin = min([dx,dx1])
    plot, r, dx,$
        title='Footpoint Displacement [km]',$
        font=3, yrange=[amin,amax], ystyle=1
    oplot, r, dx1, line=2
    !P.POSITION=[0.2,0.03,0.7,0.23]
    amax = max([vash,vash1])
    amin = min([vash,vash1])
    plot_io, r, vash,$
        title='Alfven Velocity in Shear Field [km s!A-1!N]',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]
    oplot, r, vash1, line=2
  endif


  print, 'plot 3. page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=2  
    !P.MULTI=[0,0,4]
    !P.POSITION=[0.2,0.78,0.7,0.98]
    amax = max([tash,tash1])
    amin = min([tash,tash1])
    plot_io, r, tash,$
        title='Alfven Time in Shear Field [s]',$
        font=3, yrange=[amin,amax]
    oplot, r, tash1, line=2
    !P.POSITION=[0.2,0.53,0.7,0.73]
    amax = max([eash,eash1])
    amin = min([eash,eash1])
    plot, r, eash,$
        title='Shear Electric Field [mV m!U-1!N]',$
        font=3, yrange=[0,amax]
    oplot, r, eash1, line=2
    !P.POSITION=[0.2,0.28,0.7,0.48]
    amax = max([ec,ec1])
    amin = min([ec,ec1])
    plot, r, ec,$
        title='Convection Electric Field [mV m!U-1!N]',$
        xtitle='r / R!BE!N', font=3, yrange=[0,amax]
    oplot, r, ec1, line=2
  endif
  
  
      print, 'again?'
      read, again



  if withps eq 'y' then device,/close
  set_plot,'x'

  endwhile

end



; START OF PROGRAM

  nr=101 
  r=findgen(nr) & rkm=r
  j=fltarr(nr,/NOZERO) & the=j & n=j & nl=j & m=j 
  b0=j & lb=j & db=j & dx=j 
  dcs=j & js=j & vash=j & vab0=j & vab1=j & tash=j & eash=j & ec=j 
  pi = 3.14159
;-------------
; PARAMETER:
;-------------
; 1) AREA
;    l0-McIlwain Par; r0,r1-radial bounds in RE; re-RE
  re=6400.0
  l0=8 & r0=1.016 & r1=1.15
  dr=(r1-r0)/float(nr-1) & r=dr*r+r0 & rkm=(r-1.0)*re 
  drm=dr*re*1000.  ;(gridseparation in m)

; 2) Dipole field strength
;    b0-dipole field in nT; the-dipole latitude from north; 
;    lb-field line length for l0=const in RE;
  the = 180.0/pi*asin(sqrt(r/l0))
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

; 3) Number density (cm^(-3) r>1.05 !!!; m-effective ion mass;
  n0=10.0^5 & z0=200.0 & nmsp=5.0 & nbl=1.0
;  n = 5.0*n0/( exp(-2.173*(rkm-z0)/100.0)+exp(0.522*(rkm-z0)/100.0) )$
  n = 5.0*n0/( exp(-(rkm-z0)/30.0)+exp((rkm-z0)/300.0) )$
      + nmsp/(r-1.0)^1.5 + nbl/cosh(0.5*(r-10.0))
;Lysak:  
;  nl = 0.5*n0*exp( -10.0*(r-1.05) ) + nmsp/(r-1.0)^1.5
  z0=700.0 
  m = 0.0*m+1.0+7.5*( 1.0 - tanh((rkm-z0)/500.0) )
;  m - mass (atomic mass units) 

; 4) Electron Temperature in degrees Kelvin
  z0=150.0
  temp= 1500.0/( exp(-(rkm-z0)/30.0)+exp(-(rkm-z0)/1500.0) + 0.001)
  tev=temp/8600   ; temperature in eV

; 4a) Energetic (>2 eV) Electrons 
      u00=sqrt(2./tev)
      hprofev = 1.0 - errorf(u00) + 2./sqrt(pi)*u00*exp(-u00^2)
      nev = n*hprofev & nev(where(nev lt 0.000000001)) = 0.000000001
;      number of electrons >2 eV

; 5) Alfven velocity
;    vab0-Alfven vel for b0 in km/s; 
    vab0 = 21.8*b0/sqrt(n*m)

; 6) Magnetic shear and field aligned current
;    db-magn shear in nT;
;    db1-magn. shear at 1; br0,br1-magn field at bounds;
;    dcs-current sheet width in km; dcs1-curr sheet width in km at 1;
;    js-sheet current in A/m; j-current density in A/m^2
  db0=100 & dcs0=1.0
  br0=29500.0*(4-3*r0/l0)/r0^3
  br1=29500.0*(4-3*r1/l0)/r1^3
  db = db0*sqrt(b0/br0)
  dcs = dcs0*sqrt(br0/b0)
  js = 0.796/10^3*db
  j = js/1000.0/dcs
;   dx-footpoint displacement in RE;
  dx = db/sqrt(br0*b0)*lb

; 7) Alfven velocity and electric field and Alfven travel time
;    vash-Alfven vel for db in km/s; 
;    tash-Alfven time for db amd dcs in s; 
;    eash, ec-conv electric field for vash in mV/m;
  vash = 21.8*db/sqrt(n*m)
  tash = dcs/vash
  eash = vash*db/1000.0
  ec = vash*b0/1000.0

; 8) Neutral Density

  nn0=10.0^10 & zn0=140.0 
  nn = 0.5*nn0*( exp(-(rkm-z0)/5.0)+exp(-(rkm-z0)/80.0) )+1.0
  mn = 8.0+3.5*( 1.0 - tanh((rkm-z0)/50.0) )

; 9) Collision frequencies
; nei=electron-ion,  nin=ion-neutral, nen=electron-neutral

  nei=55.*n/temp^1.5
;  nei=4.4*10.0^(-5)*n/tev^1.5
  nen=10.0^(-7)*nn*tev      ;for O
  nin=5.0*10.0^(-10)*nn          ;rough guess for an average
  ntot=nei+nen


;* --------------- not used from here ------------*
;10) Normalized collision frequencies
; noei=nei*tash, 
; note noin gives plasma neutral friction 
;    compared to electromagnetic forces
  noei=nei*tash
  noen=nen*tash
  noin=nin*tash

;11) Electron Inertia term compared to Electromagnetic terms

  lambda=(5.3/sqrt(n)/dcs)^2

;12) Normalised resitivities (Inverse=magnetic Reynolds Number)

  etaei=noei*lambda
  etaen=noen*lambda
  etain=noin*lambda
; total:
  eta=etaei+etaen+etain

;13) Properties
; E in micro V/m

  taudiff=tash/eta
  taurek=tash/sqrt(eta)
  ediff=eta*eash*1000.0
  erek=sqrt(eta)*eash*1000.0

;* --------------- not used up to here ------------*


    pos1=[0.2,0.78,0.7,0.96]
    pos2=[0.2,0.55,0.7,0.73]
    pos3=[0.2,0.32,0.7,0.50]
    pos4=[0.2,0.09,0.7,0.27]

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
    !P.POSITION=pos1
    amax = max([n,nev])
    amin = 1.0
    plot_io, rkm, n,$
        title='Electron Density [cm!U-3!N]',$
        font=3, yrange=[amin, amax]
    oplot, rkm, nev, line=2

    !P.POSITION=pos2
    amax = max(temp)
    amin = min(temp)
    plot_io, rkm, temp,$
        title='Electron Temperature [K]',$
        font=3, yrange=[amin,amax]
;    oplot, rkm, templ, line=2

    !P.POSITION=pos3
    amax = max(nn)
    amin = min(nn)
    plot_io, rkm, nn,$
        title='Neutral Density [cm!U-3!N]',$
        font=3, yrange=[amin,amax]

    !P.POSITION=pos4
    amax = max(nn*nev)
    amin = amax/10.^4
;    amin = min(nn*nev)
    plot_io, rkm, nn*nev,$
        title='N_E(>2eV) * N_N',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]

  endif

  ntot = nei+nen
  tcol = 1/ntot
  mfp  = tcol*420.*sqrt(tev)

  print, 'plot 2. page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=2  
    !P.MULTI=[0,0,4]

    !P.POSITION=pos1
    amax = max([nei,nen,ntot])
    amin = min([nei,nen,ntot])
    plot_io, rkm, nei,$
        title='Electron Collision Frequencies [s!U-1!N]',$
        font=3, yrange=[amin,amax],line=1
    oplot, rkm, nen, line=2
    oplot, rkm, ntot, line=0

    !P.POSITION=pos2
    amax = max(tcol)
    amin = min(tcol)
    plot_io, rkm, tcol,$
        title='Electron Collision Time [s]',$
        font=3, yrange=[amin,amax]

    !P.POSITION=pos3
    amax = max(mfp)
    amin = min(mfp)
    plot_io, rkm, mfp,$
        title='Mean Free Path [km]',$
        font=3, yrange=[amin,amax]

    !P.POSITION=pos4
    amax = max(tev)
    amin = min(tev)
    plot_io, rkm, tev,$
        title='Electron Energy [eV]',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]

  endif









   time=0. & delt=4.  
;& efpe=rkm
;  current density measured in micro Amp/m^2
   j0=50.*0.25*(1.+tanh((rkm-220.)/20.))*(1.+tanh((rkm-300.)/200.))

   printent=''

   for i=0,10 do begin
     time=time+delt
     print,'time=',time

;  electric field (ef,efpe)  measured in micro V/m (determined from drift)
;  current density measured in micro Amp/m^2
;  time step delt (in s)

     
     ef=71.0*j0*ntot/n
     pe=n*temp
     efpe=86./n/2./drm*( shift(pe,1)-shift(pe,-1) )
     efpe(0) = 86./n(0)/drm*( pe(1)-pe(0) )
     
     print, 'write ent to file? (input y or n)'
     read, printent
     if printent eq 'y' then begin
        nament='ent'+string(i,'(i1.1)')
        get_lun, unit
        openw, unit, nament
        printf, unit, 'time=',time-delt, '   No of gridpoints=',nr
        printf, unit, 'files: altitude (km), e field (micro V),',$ 
                  'number density (cm**(-3), electron temperature (K)'
        printf, unit, rkm
        printf, unit, ef-efpe
        printf, unit, n
        printf, unit, temp
        close, unit
        FREE_LUN, unit
     endif

     efpe(nr-1) = 86./n(nr-1)/drm*( pe(nr-1)-pe(nr-2) )
;   ef=5.
     vdrift=1.76*10.0^5*ef*tcol
     dele=10.0^(-6)*vdrift*ef*delt
     tev=tev+dele

     nei=4.4*10.0^(-5)*n/tev^1.5
     nen=10.0^(-7)*nn*tev      ;for O
     ntot = nei+nen
     tcol = 1/ntot
     mfp  = tcol*420.*sqrt(tev)
  
     u00=sqrt(2./tev)
     hprofev = 1.0 - errorf(u00) + 2./sqrt(pi)*u00*exp(-u00^2)
     nev = n*hprofev & nev(where(nev lt 10.^(-6))) = 10.^(-6)
     temp=tev*8600.



    print, 'plot 0. page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2  
      !P.MULTI=[0,0,4]
      !P.POSITION=pos1
      amax = max(j0)
      amin = 0.01
      plot_io, rkm, j0,$
        title='Current Density [10^(-6)]',$
        font=3, yrange=[amin, amax]
      oplot, rkm, ef*j0, line=2

      !P.POSITION=pos2
      amax = max(vdrift)
      amin = 0.01
      plot_io, rkm, vdrift,$
          title='Drift Velocity',$
        font=3, yrange=[amin,amax]

      !P.POSITION=pos3
      amax = max(ef)
      amin = 0.01
      plot_io, rkm, ef,$
        title='Electric Field (10^-6) and E.J',$
        font=3, yrange=[amin,amax]
      oplot, rkm, efpe, line=2

      !P.POSITION=pos4
      amax = max(dele)
      amin = 0.001
      plot_io, rkm, dele,$
        title='Energy Gain',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]

    endif


    print, 'plot 1. page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2  
      !P.MULTI=[0,0,4]
      !P.POSITION=pos1
      amax = max([n,nev])
      amin = 1.0
      plot_io, rkm, n,$
        title='Electron Density [cm!U-3!N]',$
        font=3, yrange=[amin, amax]
      oplot, rkm, nev, line=2

      !P.POSITION=pos2
      amax = max(temp)
      amin = min(temp)
      plot_io, rkm, temp,$
        title='Electron Temperature [K]',$
        font=3, yrange=[amin,amax]
;      oplot, rkm, templ, line=2

      !P.POSITION=pos3
      amax = max(nn)
      amin = min(nn)
      plot_io, rkm, nn,$
        title='Neutral Density [cm!U-3!N]',$
        font=3, yrange=[amin,amax]

      !P.POSITION=pos4
      amax = max(nn*nev)
;      amin = min(nn*nev)
      amin = amax/10.^4
      plot_io, rkm, nn*nev,$
        title='N_E(>2eV) * N_N',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]

    endif


    ntot = nei+nen
    tcol = 1/ntot
    mfp  = tcol*420.*sqrt(tev)

    print, 'plot 2. page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2  
      !P.MULTI=[0,0,4]

      !P.POSITION=pos1
      amax = max([nei,nen,ntot])
      amin = min([nei,nen,ntot])
      plot_io, rkm, nei,$
        title='Electron Collision Frequencies [s!U-1!N]',$
        font=3, yrange=[amin,amax],line=1
      oplot, rkm, nen, line=2
      oplot, rkm, ntot, line=0

      !P.POSITION=pos2
      amax = max(tcol)
      amin = min(tcol)
      plot_io, rkm, tcol,$
        title='Electron Collision Time [s]',$
        font=3, yrange=[amin,amax]
      !P.POSITION=pos3
      amax = max(mfp)
      amin = min(mfp)
      plot_io, rkm, mfp,$
        title='Mean Free Path [km]',$
        font=3, yrange=[amin,amax]
      !P.POSITION=pos4
      amax = max(tev)
      amin = min(tev)
      plot_io, rkm, tev,$
        title='Electron Energy [eV]',$
        xtitle='r / R!BE!N', font=3, yrange=[amin,amax]

    endif

  endfor
 
      print, 'again?'
      read, again

  if withps eq 'y' then device,/close
  set_plot,'x'

  endwhile

end



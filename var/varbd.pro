; PROGRAM to read and plot satellite data
;----------------------------------------
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  nnorm,bnorm,vnorm,pnorm,lnorm,tnorm, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  xs,ys,zs
COMMON ref, bxr,byr,bzr,vxr,vyr,vzr, $
            rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            xsat1,ysat1,zsat1,vxsat1,vysat1,vzsat1, $
            xsat2,ysat2,zsat2,vxsat2,vysat2,vzsat2, $
            index, starttime, phi, xtit, withps
COMMON ref2, bxr2,byr2,bzr2,vxr2,vyr2,vzr2, $
             rhor2,pr2,babr2,ptotr2,pbr2,tempr2,beta2,index2

  nsat = long(60) & ntmax = 4001 & pi = 3.14159265536
  nnorm = 20.0 & bnorm = 10.0 &  lnorm= 200.0
  vnorm = 21.8*bnorm/sqrt(nnorm) & pnorm = 0.01*bnorm^2./8.0/pi
  tnorm = lnorm/vnorm
  startime = 0.0 & time = fltarr(ntmax)
  pi = 3.14159265536 & phi0=0.0 & phi=0.0  & time2d=0.0 
  
  coor = 's' & whatangle='o' & withunits='y' & satchoice='b'
  whatindex='0' & index=2 & conti='y'
  dix2=2 & diy2=0 & index2=index-dix2
  contin='' & again='y' & withps='n' & closeps='n' & psname='ps' 
  countps=0 & wcount=0 & scount=0 & hcount=0 & rcount=0 
  fall = '' & change='' & cas=''
  xtit='time (s)'   &   plotvars='t' & vardat='absent'
  ebasis='b' & ebst='B'
  xangle='' & zangle='' & phix=0.0 & phiz=0.0
  evo1=[1.,0.,0.] & evo2=[0.,1.,0.] & evo3=[0.,0.,1.] & ewo=[1.,1.,1.]

reads:
; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
  print, 'Choose format of sat data set: obtions:'
  print, 'Default choice:           b -> binary simulation data'
  print, '                          2 -> 2D grid'
  print, '                          o -> old formatted simulation data'
  print, '                          u -> Ulysses data'
  print, '                          i -> ISSEE data'
  read, satchoice
  if satchoice eq '' or satchoice eq 'b' then readsat
  if satchoice eq '2' then   read2,time2d
  if satchoice eq 'u' then   readulysses
  if satchoice eq 'i' then   readissee
  if (satchoice ne '') and (satchoice ne 'b') and (satchoice ne 'o') $
    and (satchoice ne 'u') and (satchoice ne 'i') and (satchoice ne '2') $
    then goto, reads
  if (satchoice eq 'u') or (satchoice eq 'i') then  withunits='y'
  if (satchoice eq '') or (satchoice eq 'b') or (satchoice eq 'o') then $
             withunits='n'
  if satchoice eq '2' then xtit='x'
             

; determines: nsat,startime,itot,nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,$
;              rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd,$
;              xsat0,ysat0,zsat0,vxsat,vysat,vzsat,$
;              time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots,xs,ys,zs
; UNCOMMENT FOR DIFFERENT NORMALISATION  
;--------------------------------------
;     nnorm = 15.0                    ; for cm^(-3)
;     bnorm = 20.0                    ; for nT
     lnorm = 600.0                    ; for km
;     vnorm = 21.8*bnorm/sqrt(nnorm)  ; for km/s
;     pnorm = 0.01*bnorm^2.0/8.0/pi   ; for nPa
     tnorm = lnorm/vnorm              ; for s
;     boltz = 1.38*10^(-23)           ; 
     tempnorm = pnorm*10000./1.38/1.16/nnorm  ; for eV
    unitype='p'  &  vanorm=21.8   & vafact=21.8
    bfac=bnorm & vfac=vnorm & lfac=lnorm & nfac=nnorm & pfac=pnorm
    tempfac=tempnorm 
    tfac=tnorm & if satchoice eq '2' then tfac=1.0 
    time2d=tnorm*time2d
    t=tfac*time  &  it1=0 & it2=itot-1 & tmin=t(0) & tmax=t(it2) & nt=itot
    iv1=0 & iv2=itot-1 & nv=itot & tvmin=t(0) & tvmax=t(iv2)
    xr=t & yr=t & zr=t & bxr=t & byr=t & bzr=t & vxr=t & vyr=t & vzr=t
    rhor=t & pr=t & br=t & ptotr=t & tempr=t
; SIMULATION COORDINATES:
; Boundary values:                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd
; Probe coordinates and positions:  xsi,ysi,zsi,vxsi,vysi,vzsi
; Probe data:                       time, bxs, bys, bzs, vxs, vys, vzs,
;                                   rhos, ps, bs, ptots, xs, ys, zs

;Rotation of coordinates to obtain approximate boundary normal coordinates:
;--------------------------------------------------------------------------
; Simulation is always in x,y plane; 
;  in boundary normal coordinates:  x-normal to initial current layer
;                                   z-maximum variance of B, i.e., z is locally 
;                                     northward for antiparallel fields. 
;                                   y-completes coordinate system; in 2D y is  
;                                     the invariant direction for reconnection      
; 1. Rotation into an approximate boundary normal system (GSM)
;       i.e. x sunward y nortward.
;    a: Dayside reconnection: y ->  z
;                             z -> -y
;  tr0=fltarr(3,3) & tr0(0,0)=1. & tr0(1,0)=0. & tr0(2,0)=0. 
;                    tr0(0,1)=0. & tr0(1,1)=0.  & tr0(2,1)=1. 
;                    tr0(0,2)=0. & tr0(1,2)=-1.  & tr0(2,2)=0. 
;
;    a: Flank Kelvin Helmholtz: x_s -> y
;                               y_s -> -x
  tr0=fltarr(3,3) & tr0(0,0)=0. & tr0(1,0)=-1. & tr0(2,0)=0. 
                    tr0(0,1)=1. & tr0(1,1)=0.  & tr0(2,1)=0. 
                    tr0(0,2)=0. & tr0(1,2)=0.  & tr0(2,2)=1. 
  trafo, tr0, vxbd,vybd,vzbd,vxbd0,vybd0,vzbd0
  trafo, tr0, bxbd,bybd,bzbd,bxbd0,bybd0,bzbd0
  trafo, tr0, xsi,ysi,zsi,xsi0,ysi0,zsi0
  trafo, tr0, vxsi,vysi,vzsi,vxsi0,vysi0,vzsi0
  trafo, tr0, vxs, vys, vzs, vxs0, vys0, vzs0
  trafo, tr0, bxs, bys, bzs, bxs0, bys0, bzs0
  
; Open some windows before starting menues:
  window,0,xsize=640,ysize=512,title='Overview Plot'
  window,1,xsize=640,ysize=512,title='Variance Plots'
  window,2,xsize=640,ysize=512,title='Var Data'

angle:
; 2. Rotation into an boundary normal system (GSM) 
;        to compare with satellite data.
;    This is the reference coordinate sytem for the variance analysis
;
  print, 'CHOOSE ROTATION ANGLE: '
  print, 'Present choice: ',phi
  print, 'Options:           o -> original angle:',phi0
  print, '                   m -> phi=atan(bybd(1)/bzbd(1))'
  print, '     integer or real -> new choice of angle (in degrees)'
  print, '                   The above rotate into approximate GSM coordinates'
  print, '                   x -> rotation around x presently by'$
                   +string(phix,'(f5.1)')
  print, '                   z -> rotation around z presently by'$
                   +string(phiz,'(f5.1)')
  print, '             The above rotate first around x then around z axis!'
  print, '              return -> no change'
  read, whatangle
  if (whatangle eq 'o') then phi=phi0
  if (whatangle eq 'm') then phi=180.0/pi*atan(bybd(1)/bzbd(1))
  if (whatangle eq 'x') then begin
    print,'Old x angle is'+string(phix,'(f5.1)')
    print,'    INPUT new angle:'
    read, xangle
    if (xangle ne '') then phix=float(xangle)
  endif
  if (whatangle eq 'z') then begin
    print,'Old z angle is'+string(phiz,'(f5.1)')
    print,'    INPUT new angle:'
    read, zangle
    if (zangle ne '') then phiz=float(zangle)
  endif
  
  if ( (whatangle ne '') and (whatangle ne 'o') $
     and (whatangle ne 'x') and (whatangle ne 'z') $
     and (whatangle ne 'm') )    then     phi=float(whatangle)
  print, 'Chosen angle:  phi',phi

  phir = phi*pi/180.0  &  cphi=cos(phir)  &  sphi=sin(phir)
  phixr = phix*pi/180.0  &  cphix=cos(phixr)  &  sphix=sin(phixr)
  phizr = phiz*pi/180.0  &  cphiz=cos(phizr)  &  sphiz=sin(phizr)
  tr1=fltarr(3,3) & tr2=tr1 & tr3=tr1 & tr4=tr1 & trtr=tr1
  tr1(0,0)=cphi   & tr1(1,0)=0.  & tr1(2,0)=sphi
  tr1(0,1)=0.     & tr1(1,1)=1.  & tr1(2,1)=0. 
  tr1(0,2)=-sphi  & tr1(1,2)=0.  & tr1(2,2)=cphi 
  
  tr2(0,0)=1   & tr2(1,0)=0.      & tr2(2,0)=0
  tr2(0,1)=0.  & tr2(1,1)=cphix   & tr2(2,1)=sphix 
  tr2(0,2)=0.  & tr2(1,2)=-sphix  & tr2(2,2)=cphix 
  
  tr3(0,0)=cphiz   & tr3(1,0)=sphiz  & tr3(2,0)=0.
  tr3(0,1)=-sphiz  & tr3(1,1)=cphiz  & tr3(2,1)=0. 
  tr3(0,2)=0.0     & tr3(1,2)=0.0     & tr3(2,2)=1.
  
  matmul3,tr2,tr1,trtr
  matmul3,tr3,trtr,tr4

  trafo, tr4, vxbd0,vybd0,vzbd0, vxbdr,vybdr,vzbdr
  trafo, tr4, bxbd0,bybd0,bzbd0, bxbdr,bybdr,bzbdr
  trafo, tr4, xsi0,ysi0,zsi0, xsi1,ysi1,zsi1
  trafo, tr4, vxsi0,vysi0,vzsi0, vxsi1,vysi1,vzsi1
  trafo, tr4, vxs0, vys0, vzs0, vxs1, vys1, vzs1
  trafo, tr4, bxs0, bys0, bzs0, bxs1, bys1, bzs1

; declare some variable to record and print boundary values
  bmsp=fltarr(3) & bmsh=bmsp & vmsp=bmsp & vmsh=bmsp 
  bmsp(0)=bfac*bxbdr(0) & bmsp(1)=bfac*bybdr(0) & bmsp(2)=bfac*bzbdr(0) 
  bmsh(0)=bfac*bxbdr(1) & bmsh(1)=bfac*bybdr(1) & bmsh(2)=bfac*bzbdr(1) 
  vmsp(0)=0.0 & vmsp(1)=0.0 & vmsp(2)=0.0
  vmsh(0)=vfac*(vxbdr(1)-vxbdr(0))
  vmsh(1)=vfac*(vybdr(1)-vybdr(0))
  vmsh(2)=vfac*(vzbdr(1)-vzbdr(0))
  rhotmsp=fltarr(2) & rhotmsh=rhotmsp
  rhotmsp(0)=nfac*rhobd(0) & rhotmsp(1)=tempfac*pbd(0)/rhobd(0)
  rhotmsh(0)=nfac*rhobd(1) & rhotmsh(1)=tempfac*pbd(1)/rhobd(1)
  
satindex:
; CHOOSE SATELLITE INDEX
;------------------------
  print,'Normalisation for : '
  print, $
  '  No density ! magn. field!  velocity  !  pressure  !length units! time'
  print, $
  '   cm**(-3)  !     nT     !    km/s    !   nPascal  !    km      !   s'
  print, format='(f10.2,f12.2,f13.2,f13.4,f13.2,f12.4)',$
        nnorm,bnorm,vnorm,pnorm,lnorm,tnorm
  print,'Initial asymptotic values at xmin (1. row, magnetosphere) ',$
        'and xmax (2. row, magnetosheath) for: '
  print, $
  '     rho      p        vx       vy       vz       bx       by       bz'
  print, format='(8f9.3)',$
       rhobd(0),pbd(0),vxbd0(0),vybd0(0),vzbd0(0),bxbd0(0),bybd0(0),bzbd0(0)
  print, format='(8f9.3)',$
       rhobd(1),pbd(1),vxbd0(1),vybd0(1),vzbd0(1),bxbd0(1),bybd0(1),bzbd0(1)
  print,' and for magnetospheric (rotated into GSM) coordinates: '
  print, $
  '     rho      p        vx       vy       vz       bx       by       bz'
  print, format='(8f9.3)',$
       rhobd(0),pbd(0),vxbdr(0),vybdr(0),vzbdr(0),bxbdr(0),bybdr(0),bzbdr(0)
  print, format='(8f9.3)',$
       rhobd(1),pbd(1),vxbdr(1),vybdr(1),vzbdr(1),bxbdr(1),bybdr(1),bzbdr(1)
  print, 'Initial satellite locations and velocity:'
  print, 'sat  !    x    !    y    !    z    !    vx   !    vy   !    vz   '
  for i=0,nsat-1 do $
    print, format='(i3,6f10.2)',$
           i,xsi0(i),ysi0(i),zsi0(i),vxsi0(i),vysi0(i),vzsi0(i)
  print, $
'Satellite velocity is relative to restframe of the magnetosphere (at xmin)!!!'
  print, $
  ' Add vy,vz at xmin to obtain the probe velocity in the simulation frame.'
  print, 'Plasma velocity is recorded in the satellite frame!'
  print, 'Parameters are in simulation coordinates!'
    
cut: 
  if withps eq 'y' then  begin 
    print, 'Postscript device is still open! Close postscript device? <y> or <n>'
    read, closeps
    if closeps eq 'y' then begin
         withps = 'n' & closeps='n' & device,/close & set_plot,'x'
         !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
  endif
  
  print, 'CHOOSE PROBE INDEX: '
  print, 'Present choices:  Probe index ='+string(index,'(i3)')+$
                           '   Rotation angle ='+string(phi,'(f7.1)')
  if unitype eq 's' then print, '        Simulation units' else $
                         print, '        Physical units'
  print, '        Time range for overview plots (t)   :',tmin, tmax
  print, '        Time range for variance analysis (v):',tvmin, tvmax
  print, '        Variance plots use time range:  ', plotvars
  print, '        Variance plots (h) and (r) use basis from:  ', ebst
                         
  print, 'OPTIONS:   integer  -> Probe index'
  print, '            return  -> No changes applied'
  print, '                 a  -> Change rotation angle'
  print, '                 i  -> Show probe indices and parameters again'
  print, '                 j  -> Distance along ix (inward) for second satellite'
  print, '                 k  -> Distance along iy (leading) for second satellite'
  print, '                 n  -> Normalized units'
  print, '                 o  -> Physical units'
  print, '                 p  -> Postscrip output'
  print, '                 t  -> Time range for plots'
  print, '                 u  -> Set time range back to total range'
  print, '                 v  -> Set REFERENCE time range for VARIANCE'
  print, '                 vt -> Variance plots for time range t (so)'
  print, '                 vv -> Variance plots for reference range (v) only'
  print, ' VARIANCE plots:  '
  print, '                 s  -> Standard (predefined) variance plots'
  print, '                 h  -> Hodograms for selected BASIS coord.'
  print, '                 r  -> Regular plots V,B,E for selected BASIS coord.'
  print, '                 w  -> Plot walen relation and print data'
  print, ' Select BASIS for inputs (h) and (r):  '
  print, '                 bo -> original Basis'
  print, '                 bb -> Basis using Magnetic field variance'
  print, '                 be -> Basis using Electric field variance'
  print, '                 beh-> Basis using HT Electric field variance'
  print, '                 bv -> Basis using Velocity variance'
  print, '                 q  -> TERMINATE'
  read, whatindex
  if whatindex eq '' then print,'index=',index,' not altered'
  if whatindex eq 'a' then goto, angle
  if whatindex eq 'i' then goto, satindex
  if whatindex eq 'n' then begin
    unitype='s'
    bfac=1. & vfac=1. & lfac=1. & nfac=1. & pfac=1. & tempfac=1.
    vafact=1. & tfac=1. & t=time & tmin=t(it1) & tmax=t(it2)
  endif
  if whatindex eq 'o' then begin
    unitype='p'
    bfac=bnorm & vfac=vnorm & lfac=lnorm & nfac=nnorm & pfac=pnorm
    tempfac=tempnorm & vafact=vanorm & if satchoice ne '2' then tfac=tnorm  
    t=tfac*time
    tmin=t(it1) & tmax=t(it2)
  endif
  if whatindex eq 'p' then begin
     withps = 'y'
     !P.THICK=2.
     !X.THICK=1.5
     !Y.THICK=1.5
     !P.CHARTHICK=2.
     psname='satp'+string(index,'(i2.2)')+'p'+string(countps,'(i2.2)')
     countps=countps+1 & set_plot,'ps'
  endif
  if whatindex eq 't' then begin
     timerange, time,tfac, tmin,tmax,it1,it2,nt,success
     if success eq 'none' then goto, cut
     if (vardat eq 'present') and (plotvars eq 't') then goto, plotvart
  endif
  if whatindex eq 'u' then begin
     tmin=tfac*time(0) & tmax=tfac*time(itot-1)
  endif
  
  if whatindex eq 'q' then stop
 
  if (vardat ne 'present') and $
    ( (whatindex eq 'vt') or (whatindex eq 'vv') or (whatindex eq 's') or $
      (whatindex eq 'h') or (whatindex eq 'r') or (whatindex eq 'bb') or $
      (whatindex eq 'be') or (whatindex eq 'beh') or (whatindex eq 'bv') or $
      (whatindex eq 'w') )   then begin
      print, 'Determine Variance data first!!!! -> v'
      goto, cut
  endif

  if ( (whatindex ne '') and (whatindex ne 'a') and (whatindex ne 'i') $
     and (whatindex ne 'j') and (whatindex ne 'k') $
     and (whatindex ne 'n') and (whatindex ne 'o') and (whatindex ne 'p') $
     and (whatindex ne 't') and (whatindex ne 'u') and (whatindex ne 'v') $
     and (whatindex ne 'vt') and (whatindex ne 'vv') and (whatindex ne 's') $
     and (whatindex ne 'h') and (whatindex ne 'r') and (whatindex ne 'bb') $
     and (whatindex ne 'be') and (whatindex ne 'beh') and (whatindex ne 'bv') $
     and (whatindex ne 'bo') $
     and (whatindex ne 'w') and (whatindex ne 'q') )    then begin
    index=fix(whatindex)
    print, 'Chosen satellite index: ',index
    if index gt nsat-1 then begin 
       print, 'Probe index is too large -> return to menue!'
    endif  
    if index lt dix2 then begin 
       print, 'Probe index is too small -> return to menue!'
    endif  
    vardat='absent'
  endif
  print,'separation along x is ',dix2,' times ',(ysi0(1)-ysi0(0))
  print,'separation along y is ',diy2,' times ',(time(1)-time(0))
  if whatindex eq 'j' then begin
     print, 'Input dix2=separation along the inward direction'
     read, dix2
  endif
  index2=index-dix2
  if whatindex eq 'k' then begin
     print, 'Input diy2=separation along the magnetopause'
     read, diy2
  endif
  
  
; Determine the actual data set to be analyzed and give it physical 
;     (or normalized) units
; 
  bxr=bfac*bxs1(index,*)  & byr=bfac*bys1(index,*) & bzr=bfac*bzs1(index,*)
  vxr=vfac*vxs1(index,*)  & vyr=vfac*vys1(index,*) & vzr=vfac*vzs1(index,*)
  rhor=nfac*rhos(index,*) & pr=pfac*ps(index,*)    & babr=bfac*bs(index,*)  
  ptotr=pfac*ptots(index,*) & pbr=pfac*bs(index,*)*bs(index,*) 
  tempr=tempfac*ps(index,*)/rhos(index,*) & beta=ps(index,*)/bs(index,*)^2
  t=tfac*time
  
  bxr2=bfac*bxs1(index2,*)  & byr2=bfac*bys1(index2,*) & bzr2=bfac*bzs1(index2,*)
  vxr2=vfac*vxs1(index2,*)  & vyr2=vfac*vys1(index2,*) & vzr2=vfac*vzs1(index2,*)
  rhor2=nfac*rhos(index2,*) & pr2=pfac*ps(index2,*)    & babr2=bfac*bs(index2,*)  
  ptotr2=pfac*ptots(index2,*) & pbr2=pfac*bs(index2,*)*bs(index2,*) 
  tempr2=tempfac*ps(index2,*)/rhos(index2,*) & beta2=ps(index2,*)/bs(index2,*)^2
  bxr2=shift(bxr2,diy2) & byr2=shift(byr2,diy2) & bzr2=shift(bzr2,diy2)
  vxr2=shift(vxr2,diy2) & vyr2=shift(vyr2,diy2) & vzr2=shift(vzr2,diy2)
  rhor2=shift(rhor2,diy2) & pr2=shift(pr2,diy2) & babr2=shift(babr2,diy2)
  ptotr2=shift(ptotr2,diy2) & pbr2=shift(pbr2,diy2) 
  tempr2=shift(tempr2,diy2) & beta2=shift(beta2,diy2) 

; smooth data:
;  bxr=smooth(bxr,3) & byr=smooth(byr,3) & bzr=smooth(bzr,3)
;  vxr=smooth(vxr,3) & vyr=smooth(vyr,3) & vzr=smooth(vzr,3)
;  rhor=smooth(rhor,5) & pr=smooth(pr,3) & babr=smooth(babr,3)
;  ptotr=smooth(ptotr,3) & pbr=smooth(pbr,3) & tempr=smooth(tempr,3)
;  beta=smooth(beta,3) 

  efield, unitype,vxr,vyr,vzr, bxr,byr,bzr, exr,eyr,ezr
; Same for the boundary values
  bmsp(0)=bfac*bxbdr(0) & bmsp(1)=bfac*bybdr(0) & bmsp(2)=bfac*bzbdr(0) 
  bmsh(0)=bfac*bxbdr(1) & bmsh(1)=bfac*bybdr(1) & bmsh(2)=bfac*bzbdr(1) 
  vmsp(0)=0.0 & vmsp(1)=0.0 & vmsp(2)=0.0
  vmsh(0)=vfac*(vxbdr(1)-vxbdr(0))
  vmsh(1)=vfac*(vybdr(1)-vybdr(0))
  vmsh(2)=vfac*(vzbdr(1)-vzbdr(0))
  rhotmsp(0)=nfac*rhobd(0) & rhotmsp(1)=tempfac*pbd(0)/rhobd(0)
  rhotmsh(0)=nfac*rhobd(1) & rhotmsh(1)=tempfac*pbd(1)/rhobd(1)
  if satchoice eq '2' then  $
      strnn=cas+'Cut at y= '+string(ysi0(index),'(f5.1)')$
                 +', phi='+string(phi,'(f5.1)')$
                 +', time='+string(time2d,'(f5.0)')


  if (whatindex eq 'v') then begin
; CARRY OUT THE VARIANCE ANALYSIS
; -------------------------------
    timerange, time,tfac, tvmin,tvmax,iv1,iv2,nv,success
    if success eq 'none' then goto, cut
    vardat='present'
    print, 'Variance Analysis for the Time Range:', tvmin, tvmax
    print, '  Number of data points:', nv
    strnn='!17'+cas+'Probe '+string(index,'(i2.2)')$
        +',    phi='+string(phi,'(f5.1)')     ; info to be passed to some plots
    if satchoice eq '2' then  $
      strnn='!17'+cas+'Cut at y= '+string(ysi0(index),'(f5.1)')$
                 +', phi='+string(phi,'(f5.1)')$
                 +', time='+string(time2d,'(f5.0)')
    print,strnn
    tv=t(iv1:iv2)
; Variance for V, B, and E
    vxv=vxr(iv1:iv2) & vyv=vyr(iv1:iv2) & vzv=vzr(iv1:iv2)
    bxv=bxr(iv1:iv2) & byv=byr(iv1:iv2) & bzv=bzr(iv1:iv2)
    exv=exr(iv1:iv2) & eyv=eyr(iv1:iv2) & ezv=ezr(iv1:iv2)
    varmat,nv,vxv,vyv,vzv,vav,vmat
    varmat,nv,bxv,byv,bzv,bav,bmat
    varmat,nv,exv,eyv,ezv,eav,emat
    eigen, vmat,evv1,evv2,evv3,ewv
    eigen, bmat,evb1,evb2,evb3,ewb
    eigen, emat,eve1,eve2,eve3,ewe
    sorteigenb, evb1,evb2,evb3
    sorteigen, evb1,evb2,evb3,evv1,evv2,evv3
    sorteigen, evb1,evb2,evb3,eve1,eve2,eve3
; Hoffmann Teller velocity and variance for Eht
    htcoor, nv, vxv,vyv,vzv, bxv,byv,bzv, vht
    ehtfield, unitype,vht,bxr,byr,bzr,ehxr,ehyr,ehzr
    ehxv=ehxr(iv1:iv2) & ehyv=ehyr(iv1:iv2) & ehzv=ehzr(iv1:iv2)
    varmat,nv,ehxv,ehyv,ehzv,ehav,ehmat
    eigen, ehmat,eveh1,eveh2,eveh3,eweh
    sorteigen, evb1,evb2,evb3,eveh1,eveh2,eveh3
; Variance for Vht
    vmhtx=vxv-vht(0)  & vmhty=vyv-vht(1)  & vmhtz=vzv-vht(2)
    varmat,nv,vmhtx,vmhty,vmhtz,vmhtav,vmhtmat
    eigen, vmhtmat,evmhtv1,evmhtv2,evmhtv3,ewmhtv
    sorteigen, evb1,evb2,evb3,evmhtv1,evmhtv2,evmhtv3
; Some stuff for plots testing the walen relation and the HT frame
    vmhp=fltarr(nv,3) & vmhp(*,0)=vmhtx & vmhp(*,1)=vmhty  & vmhp(*,2)=vmhtz
    valfv=vmhp 
    valfv(*,0)=bxv/sqrt(rhor(iv1:iv2))
    valfv(*,1)=byv/sqrt(rhor(iv1:iv2))
    valfv(*,2)=bzv/sqrt(rhor(iv1:iv2))
    if unitype eq 'p' then valfv=vafact*valfv
    ewh=vmhp & ewh(*,0)=exv & ewh(*,1)=eyv &  ewh(*,2)=ezv
    ewht=vmhp & ewht(*,0)=ehxv & ewht(*,1)=ehyv &  ewht(*,2)=ehzv
; Testing the walen relation and the HT frame
    if withps eq 'n' then begin 
      wset,1
      walenplot, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),ccoef0,cstd,creg,strnn
      wset,2
      printpl, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             bmsp,bmsh,vmsp,vmsh,rhotmsp,rhotmsh,ccoef0,cstd,creg,strnn
    endif
; Determine actual velocity and field components for variance plots        
    if ebasis eq 'v' then begin
       ebst='V' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewv & endif
    if ebasis eq 'b' then begin
       ebst='B' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewb & endif
    if ebasis eq 'e' then begin
       ebst='E' & eb1=eve1 & eb2=eve2 & eb3=eve3 & eb=ewe & endif
    if ebasis eq 'eh' then begin
       ebst='Eht' & eb1=eveh1 & eb2=eveh2 & eb3=eveh3 & eb=eweh & endif
    if ebasis eq 'o' then begin
       ebst='Orig' & eb1=evo1 & eb2=evo2 & eb3=evo3 & eb=ewo & endif
    if plotvars eq 'v' then goto, plotvarv
    if plotvars eq 't' then goto, plotvart
    goto, cut
  endif
  
  
  if (whatindex eq 'vt') then begin
plotvart:
    plotvars='t'
    np=nt
    vp=fltarr(np,3) & bp=vp & ep=vp & ehp=vp & rhop=fltarr(np)
    tp=t(it1:it2)  & rhop=rhor(it1:it2)
    vp(*,0) =vxr(it1:it2)  & vp(*,1) =vyr(it1:it2)  &  vp(*,2)=vzr(it1:it2)
    bp(*,0) =bxr(it1:it2)  & bp(*,1) =byr(it1:it2)  &  bp(*,2)=bzr(it1:it2)
    ep(*,0) =exr(it1:it2)  & ep(*,1) =eyr(it1:it2)  &  ep(*,2)=ezr(it1:it2)
    ehp(*,0)=ehxr(it1:it2) & ehp(*,1)=ehyr(it1:it2) &  ehp(*,2)=ehzr(it1:it2)
    goto, cut
  endif


  if (whatindex eq 'vv') then begin
plotvarv:
    plotvars='v'
    np=nv
    vp=fltarr(np,3) & bp=vp & ep=vp & ehp=vp & rhop=fltarr(np)
    tp=tv  & rhop=rhor(iv1:iv2)
    vp(*,0)=vxv & vp(*,1)=vyv &  vp(*,2)=vzv
    bp(*,0)=bxv & bp(*,1)=byv &  bp(*,2)=bzv
    ep(*,0)=exv & ep(*,1)=eyv &  ep(*,2)=ezv
    ehp(*,0)=ehxv & ehp(*,1)=ehyv &  ehp(*,2)=ehzv
    goto, cut
  endif


; Determine basis for variance plots
  if (whatindex eq 'bv') then begin
    ebasis='v' & ebst='V' & eb1=evv1 & eb2=evv2 & eb3=evv3 & eb=ewv & goto, cut 
  endif
  if (whatindex eq 'bb') then begin
    ebasis='b' & ebst='B' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewb & goto, cut
  endif
  if (whatindex eq 'be') then begin
    ebasis='e' & ebst='E' & eb1=eve1 & eb2=eve2 & eb3=eve3 & eb=ewe & goto, cut
  endif
  if (whatindex eq 'beh') then begin
    ebasis='eh' & ebst='Eht' & eb1=eveh1 & eb2=eveh2 & eb3=eveh3 & eb=eweh 
    goto, cut
  endif
  
  if (whatindex eq 'bo') then begin
    ebasis='o' & ebst='Orig' & eb1=evo1 & eb2=evo2 & eb3=evo3 & eb=ewo 
    goto, cut
  endif

  if (whatindex eq 'w') then begin
    if withps eq 'y' then begin & device,/close & set_plot,'x' & endif
    wset,1
    walenplot, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),ccoef0,cstd,creg,strnn
    wset,2
    printpl, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             bmsp,bmsh,vmsp,vmsh,rhotmsp,rhotmsh,ccoef0,cstd,creg,strnn
    if withps eq 'y' then begin
      set_plot,'ps' & wcount=wcount+1
      device,filename=psname+'w'+string(wcount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
      walenplot, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),ccoef0,cstd,creg,strnn
      printpl, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             bmsp,bmsh,vmsp,vmsh,rhotmsp,rhotmsh,ccoef0,cstd,creg,strnn
    endif
    goto, cut
  endif
  
   
  itv1=iv1-it1 & itv2=iv2-it1 
; Standard (predetermined) variance plots
  if (whatindex eq 's') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      device,/close & set_plot,'ps' & scount=scount+1
      device,filename=psname+'s'+string(scount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
   !p.multi=[0,2,2,0,0]    
    hodo, np,tp,tvmin,tvmax,itv1,itv2,bp,evb1,evb2,evb3,ewb,$
          'Magnetic Field','B',strnn,0,vht
    hodo, np,tp,tvmin,tvmax,itv1,itv2,vp,evv1,evv2,evv3,ewv,$
          'Velocity','V',strnn,1,vht
    print,'Continue with electric field ?'
    read, conti
    if (conti eq 'y') or (conti eq '') then begin
      !p.multi=[0,2,2,0,0]    
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ep,eve1,eve2,eve3,ewe,$
          'Electric Field','E',strnn,0,vht
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ehp,eveh1,eveh2,eveh3,eweh,$
          'HT Electric Field','Eht',strnn,1,vht
    endif
    print,'Continue with regular plot?'
    read, conti
    if (conti eq 'y') or (conti eq '') then  begin
      !p.multi=[0,1,3,0,0]    
      plobve, np,tp,tvmin,tvmax,bp,vp,ep,eb1,eb2,eb3,eb,ebst,strnn,xtit,vht
    endif
    goto, cut
  endif
  
    
; Hodograms in variance coordinates
  if (whatindex eq 'h') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      device,/close & set_plot,'ps' & hcount=hcount+1
      device,filename=psname+'h'+string(hcount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
   !p.multi=[0,2,2,0,0]    
    hodo, np,tp,tvmin,tvmax,itv1,itv2,bp,eb1,eb2,eb3,eb,$
          'Magnetic Field',ebst,strnn,0,vht
    hodo, np,tp,tvmin,tvmax,itv1,itv2,vp,eb1,eb2,eb3,eb,$
          'Velocity',ebst,strnn,1,vht
    print,'Continue with electric field plots?'
    read, conti
    if (conti eq 'y') or (conti eq '') then begin
      !p.multi=[0,2,2,0,0]    
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ep,eb1,eb2,eb3,eb,$
          'Electric Field',ebst,strnn,0,vht
      hodo, np,tp,tvmin,tvmax,itv1,itv2,ehp,eb1,eb2,eb3,eb,$
          'HT Electric Field',ebst,strnn,1,vht
    endif
    goto, cut
  endif
  
    
; Regular variance plots
  if (whatindex eq 'r') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      device,/close & set_plot,'ps' & rcount=rcount+1
      device,filename=psname+'r'+string(rcount,'(i2.2)')+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
    !p.multi=[0,1,3,0,0]    
    plobve, np,tp,tvmin,tvmax,bp,vp,ep,eb1,eb2,eb3,eb,ebst,strnn,xtit,vht
    goto, cut
  endif
  

; Finally the overview plots:
; for simulation frame
  xsat1=xsi0(index)    & ysat1=ysi0(index)   & zsat1=zsi0(index) 
  vxsat1=vxsi0(index)+vxbd0(1)   & vysat1=vysi0(index)+vybd0(1) 
  vzsat1=vzsi0(index)+vzbd0(1) 
; for rotated, magnetospheric frame with physical units
  xsat2=lnorm*xsi1(index) & ysat2=lnorm*ysi1(index) & zsat2=lnorm*zsi1(index) 
  vxsat2=vnorm*vxsi1(index) & vysat2=vnorm*vysi1(index)
  vzsat2=vnorm*vzsi1(index) 

  if withps eq 'n' then wset,0
  if withps eq 'y' then begin
     device,/close & set_plot,'ps' & device,filename=psname+'.ps'
     device,/portrait
     device,/inches,xsize=8.,ysize=10.,scale_factor=1.0,xoffset=0.5,yoffset=0.5
     !P.THICK=2. & !X.THICK=1.5 & !Y.THICK=1.5 & !P.CHARTHICK=2.
  endif
  plt2var, itot,tmin,tmax,time2d,satchoice,strnn
  goto, cut

end

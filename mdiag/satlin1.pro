; PROGRAM to read and plot satellite data
;----------------------------------------

  nsat = long(60) & nt = 4001 & pi = 3.14159265536
  nnorm = 20.0 & bnorm = 10.0 &  lnorm= 200.0
  vnorm = 21.8*bnorm/sqrt(nnorm) & pnorm = 0.01*bnorm^2./8.0/pi
  tnorm = lnorm/vnorm
  pbd = fltarr(2) & rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
  bxbd = pbd & bybd = pbd & bzbd = pbd
  zeit = 0.0 & startime = 0.0 & time = fltarr(nt)
  pi = 3.14159265536 & phi0=0.0 & phi=0.0 
  alph=0.0 & bet=0.0
  psxsize=8. & psysize=10.
  tsmin= fltarr(20) & tsmax=tsmin & tscrit=25.0 & nscrit=12.0
  ishade='y' & shading='o'
  vzdiff = 0.0 & cvzdiff = '0'

  coor = 's' & whatangle='o' 
  whatindex='0' & index=0 & names=strarr(15) & names=replicate(' ',15)
  contin='' & again='y' & withps='n' & fall = '' & change=''
  xtit='time (s)'

; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
  openr, 8, 'satbin',/F77_UNFORMATTED
  readu, 8,  nsat,startime
  print, 'Number of satellites:',nsat,'     Start time:',startime

  xsat=fltarr(nsat) & ysat=xsat & zsat=xsat
  xsat0=xsat & ysat0=xsat & zsat0=xsat
  vxsat=xsat & vysat=xsat & vzsat=xsat
  vxs=xsat & vys=xsat & vzs=xsat & bxs=xsat & bys=xsat & bzs=xsat
  rhos=xsat &  ps=xsat &  bs=xsat 
  xc=fltarr(nsat,nt) & yc=xc & zc=xc 
  bxc=xc & byc=xc & bzc=xc & vxc=xc & vyc=xc & vzc=xc
  rhoc=xc & pc=xc & bc=xc & ptotc=xc

  readu, 8, nnorm,bnorm,vnorm,pnorm,lnorm,tnorm
  readu, 8, rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd
  readu, 8, xsat0,ysat0,zsat0,vxsat,vysat,vzsat
  i=0
  while not eof(8) do begin
     readu, 8, zeit,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,xsat,ysat,zsat
     time(i)=zeit   & bxc(*,i)=bxs & byc(*,i)=bys & bzc(*,i)=bzs
     vxc(*,i)=vxs   & vyc(*,i)=vys & vzc(*,i)=vzs
     rhoc(*,i)=rhos & pc(*,i)=ps   & bc(*,i)=bs & ptotc(*,i)= ps+bs^2
     xc(*,i)=xsat   & yc(*,i)=ysat &  zc(*,i)=zsat
     itot=i
     if i lt nt-1 then i=i+1 else stop, ' to many records '
  endwhile
  print, 'Size of datasets:',itot+1
  close, 8
  
;Rotation of coordinates to obtain approximate boundary normal coordinates:
;--------------------------------------------------------------------------
; simulation is always in x,y plane; in boundary normal coordinates:
;  x-normal to original current layer
;  z-maximum variance of B, i.e., z is locally northward for antiparallel 
;            fields.
;  y- complets coordinate system; in 2D y is the invariant direction for   
;            reconnection
angle:
  print, 'CHOOSE ROTATION ANGLE: '
  print, 'Present choice: ',phi
  print, 'Options:           o -> angle:',0
  print, '                   m -> phi=atan(bybd(1)/bzbd(1))'
  print, '                   b -> change angle to equat plane'
  print, '                   c -> change angle for flaring'
  if whatindex eq 'a' then $
     print, '     integer or real -> new choice of kh angle (in degrees)'
  if whatindex eq 'b' then $
     print, '     integer or real -> new choice of equat angle (in degrees)'
  if whatindex eq 'c' then $
     print, '     integer or real -> new choice of flaring angle (in degrees)'
  print, '              return -> no change'
  read, whatangle
  if (whatangle eq 'o') then phi=phi0
  if (whatangle eq 'm') then phi=180.0/pi*atan(bybd(1)/bzbd(1))
  if ( (whatangle ne '') and (whatangle ne 'o') $
     and (whatangle ne 'm') )   then begin
     if whatindex eq 'a' then phi=float(whatangle)
     if whatindex eq 'b' then alph=float(whatangle)
     if whatindex eq 'c' then bet=float(whatangle)
  endif
  print, 'Chosen angle: ',phi
  if (whatangle eq 'b') then begin
    print, 'Input angle to equat plane:'
    read, alph & endif
  if (whatangle eq 'c') then begin
    print, 'Input angle for flaring:'
    read, bet & endif

signkh:
  if whatindex eq 'r' then begin
    phi=-phi
    vzbd=-vzbd & bxbd=-bxbd & bybd=-bybd
    bxc=-bxc & byc=-byc & vzc=-vzc
    vzsat=-vzsat
  endif
; 1. Change to local boundary system (phi is angle of the k vector
;     with the equatorial plane!!
  phir = phi*pi/180.0  
  cphi=cos(phir) & sphi=sin(phir)
  
  
  vxsatr = vxsat
  vysatr = vysat*cphi - vzsat*sphi  & vzsatr = vysat*sphi + vzsat*cphi
  xsatr  = xsat0
  ysatr  = ysat0*cphi - zsat0*sphi  & zsatr = ysat0*sphi + zsat0*cphi
; 
  vxrot = vxbd
  vyrot = vybd*cphi - vzbd*sphi  & vzrot = vybd*sphi + vzbd*cphi
  bxrot = bxbd
  byrot = bybd*cphi - bzbd*sphi  & bzrot = bybd*sphi + bzbd*cphi

; 1. Change to local boundary system (phi is angle of the k vector
;     with the equatorial plane!?!
  alphr = alph*pi/180.0  
  calp=cos(alphr) & salp=sin(alphr)
  betr = bet*pi/180.0  
  cbet=cos(betr) & sbet=sin(betr)
  if ((alph ne 0.0) or (bet ne 0.0)) then begin

    ; 1. rot to equat plane:::
    vxsau = vxsatr*calp - vzsatr*salp
    vysau = vysatr        & vzsau = vxsatr*salp + vzsatr*calp
    xsau = xsatr*calp - zsatr*salp
    ysau = ysatr          & zsau = xsatr*salp + zsatr*calp

    vxrou = vxrot*calp - vzrot*salp
    vyrou = vyrot         & vzrou = vxrot*salp + vzrot*calp
    bxrou = bxrot*calp - bzrot*salp
    byrou = byrot         & bzrou = bxrot*salp + bzrot*calp

    ; 2. rot for flaring:::
    vxsatr = vxsau*cbet + vysau*sbet
    vysatr = -vxsau*sbet + vysau*cbet   & vzsatr = vzsau
    xsatr  = xsau*cbet + ysau*sbet
    ysatr  = -xsau*sbet + ysau*cbet     & zsatr = zsau
; 
    vxrot = vxrou*cbet + vyrou*sbet
    vyrot = -vxrou*sbet + vyrou*cbet    & vzrot = vzrou
    bxrot = bxrou*cbet + byrou*sbet
    byrot = -bxrou*sbet + byrou*cbet    & bzrot = bzrou
  endif




; CHOOSE SATELLITE INDEX
;------------------------
cut:
  vxout=vxbd & vyout=vybd & vzout=vzbd 
  bxout=bxbd & byout=bybd & bzout=bzbd
  xsato=xsat0 & ysato=ysat0 & zsato=zsat0 
  vxsato=vxsat & vysato=vysat & vzsato=vzsat 
  if (coor eq 'm') then begin
    vxout=vxrot & vyout=vyrot & vzout=vzrot
    byout=byrot & bzout=bzrot 
    ysato=ysatr & zsato=zsatr & vysato=vysatr & vzsato=vzsatr 
  endif

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
       rhobd(0),pbd(0),vxbd(0),vyout(0),vzout(0),bxbd(0),byout(0),bzout(0)
  print, format='(8f9.3)',$
       rhobd(1),pbd(1),vxbd(1),vyout(1),vzout(1),bxbd(1),byout(1),bzout(1)
  print, 'Initial satellite locations and velocity:'
  print, 'sat  !    x    !    y    !    z    !    vx   !    vy   !    vz   '
  for i=0,nsat-1 do $
    print, format='(i3,6f10.2)',$
           i,xsat0(i),ysato(i),zsato(i),vxsat(i),vysato(i),vzsato(i)
  print, $
'Satellite velocity is relative to restframe of the magnetosphere (at xmin)!!!'
  print, $
  ' Add vy,vz at xmin to obtain the probe velocity in the simulation frame.'
  print, 'Plasma velocity is recorded in the satellite frame!'
  if (coor eq 'm') then  $
    print, 'Parameters are in rotated (magnetospheric) coordinates!' $
  else $
    print, 'Parameters are in simulation coordinates!'
    
  print, 'CHOOSE PROBE INDEX: '
  print, 'Present choice: P-index =',index,$
                            '  Rotation angle(kh:phi)  =',phi
  print, '                              Rotation angle (equat:alpha)  =',alph
  print, '                             Rotation angle (flaring:beta)  =',bet
  print, 'Options:   integer -> probe index'
  print, '            return -> no changes applied'
  print, '                 a -> change rotation angle phi (kh plane)'
  print, '                 b -> change angle to equat plane'
  print, '                 c -> change angle for flaring'
  print, '                 i -> indicate sheath-like intervals'
  print, '                 m -> print data in rotated coord'
  print, '                 v -> subtract vx component from vectors'
  print, '                 s -> print data in simulation coord'
  print, '                 r -> change sign of kh vector'
  print, '                 p -> postscrip output'
  print, '                 q -> terminate'
  read, whatindex
  if whatindex eq '' then print,'index=',index,' not altered'
  if (whatindex eq 'a') or (whatindex eq 'b') $
                        or (whatindex eq 'c') then goto, angle
  if whatindex eq 'i' then begin
     print, 'Shading options:'
     print, '  Present choice for nscrit = ',nscrit
     print, '  Options:    floating number-> new value for nscrit '
     print, '              o              -> shading on'
     print, '              f              -> shading off'
     read, shading
     if (shading eq 'o') then ishade='y'
     if (shading eq 'f') then ishade='n'
     if (shading ne 'f') and (shading ne 'o') $
          and (shading ne '') then nscrit=float(shading)
  endif
  if whatindex eq 'v' then begin
     print, ' Present choice for vz = ',vzdiff
     print, ' Input vz value'
     read, cvzdiff
     if cvzdiff ne '' then vzdiff=float(cvzdiff)
  endif
  if (whatindex eq 'm') or (whatindex eq 's') then begin
     coor = whatindex
     goto, cut
  endif
  if whatindex eq 'p' then begin
       withps = 'y'
       !P.THICK=2.
       !X.THICK=1.5
       !Y.THICK=1.5
       !P.CHARTHICK=3.
       set_plot,'ps'
       device,filename='satp'+string(index,'(i2.2)')+'.ps'
       device,/portrait
       device,/inches,xsize=psxsize,scale_factor=1.0,xoffset=0.5
       device,/inches,ysize=psysize,scale_factor=1.0,yoffset=0.5
  endif
  if whatindex eq 'r' then goto, signkh
  if whatindex eq 'q' then stop
  if ( (whatindex ne '') and (whatindex ne 'a') and (whatindex ne 'm') $
     and (whatindex ne 's') and (whatindex ne 'p') and (whatindex ne 'r') $
     and (whatindex ne 'q') and (whatindex ne 'i') $
     and (whatindex ne 'v') )    then begin
    index=fix(whatindex)
    print, 'Chosen satellite index: ',index
  endif

  t=fltarr(itot+1) & t=time(0:itot)
  x1=t & y1=t & z1=t & bx1=t & by1=t & bz1=t & vx1=t & vy1=t & vz1=t
  rho1=t & p1=t & b1=t & ptot1=t & temp1=t
; NOTE: the actual path through the simulation domain is given by 
; xc,yc,zc, however, for periodic systems this can lead to exit and entry
; into the simulation domain.
; Anyhow, of interest is really only the trajectory relative to 
; the magnetospheric frame.
; This is:
  x1=xsat0(index)*(t-startime) 
  bx1=bxc(index,0:itot) & by2=byc(index,0:itot) & bz2=bzc(index,0:itot)
  vx1=vxc(index,0:itot) & vy2=vyc(index,0:itot) & vz2=vzc(index,0:itot)
  rho1=rhoc(index,0:itot) & p1=pc(index,0:itot) & b1=bc(index,0:itot)  
  ptot1=ptotc(index,0:itot) & temp1=p1/rho1 & pb1=b1^2  & beta=p1/b1^2
  
; in rotated coordinates:
  y1=ysatr(index)*(t-startime) & z1=zsatr(index)*(t-startime)  
  vy1 = vy2*cphi - vz2*sphi & vz1 = vy2*sphi + vz2*cphi
  by1 = by2*cphi - bz2*sphi & bz1 = by2*sphi + bz2*cphi
  babs = sqrt(bx1*bx1+by1*by1+bz1*bz1) 

 print,'salp,calp:',salp, calp
  x2 = x1*calp - z1*salp     & y2=y1    & z2 = x1*salp + z1*calp
  vx2 = vx1*calp - vz1*salp  & vy2=vy1  & vz2 = vx1*salp + vz1*calp
  bx2 = bx1*calp - bz1*salp  & by2=by1  & bz2 = bx1*salp + bz1*calp

 print,'sbet,cbet:',sbet, cbet
  x1 = x2*cbet + y2*sbet   & y1 = -x2*sbet + y2*cbet  & z1 = z2
  vx1 = vx2*cbet + vy2*sbet   & vy1 = -vx2*sbet + vy2*cbet  & vz1 = vz2
  bx1 = bx2*cbet + by2*sbet   & by1 = -bx2*sbet + by2*cbet  & bz1 = bz2


; UNCOMMENT FOR DIFFERENT NORMALISATION  
;--------------------------------------
;     nnorm = 15.0
;     bnorm = 20.0
;     lnorm = 600.0
;     vnorm = 21.8*bnorm/sqrt(nnorm)
;     pnorm = 0.01*bnorm^2.0/8.0/pi
     tnorm = lnorm/vnorm
;     boltz = 1.38*10^(-23)
     tempnorm = pnorm*10000./1.38/1.16/nnorm
  t=tnorm*time(0:itot)
  bx1=bnorm*bx1 & by1=bnorm*by1 & bz1=bnorm*bz1
  vx1=vnorm*vx1 & vy1=vnorm*vy1 & vz1=vnorm*vz1
  rho1=nnorm*rho1 & p1=pnorm*p1 & pb1=pnorm*pb1 & ptot1=pnorm*ptot1
  babs = bnorm*babs & temp1=tempnorm*temp1
; for simulation frame
  xsat1=xsat0(index)    & ysat1=ysat0(index)   & zsat1=zsat0(index) 
  vxsat1=vxsat(index)  & vysat1=vysat(index)+vybd(0)
  vzsat1=vzsat(index)+vzbd(0) 
; for rotated, magnetospheric frame with physical units
  xsat2=lnorm*xsat0(index) & ysat2=lnorm*ysatr(index) 
  zsat2=lnorm*zsatr(index) 
  vxsat2=vnorm*vxsat(index) & vysat2=vnorm*vysatr(index)
  vzsat2=vnorm*vzsatr(index) 


    dpx=0.63  & dpy=0.17
    xa=0.07 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.8 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3
    
    ;max = 520  
    max = t(itot)  
    min = 300  ; min = t(0)
    i=0 & while t(i) lt min do i=i+1 & istart=i 
    i=itot & while t(i) gt max do i=i-1 & iend=i 
    del = max-min

   if withps ne 'y' then window,0,xsize=680,ysize=850,title='Satellite Data'


       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,1,5,0,0]
       !P.CHARSIZE=2.5
       !P.FONT=-1
       !X.TICKS=0
       !Y.TICKS=0
       !X.TICKlen=0.04
       !Y.TICKlen=0.02
       !X.RANGE=[min,max]
;       !Y.RANGE=[ymin,ymax]


   if ishade eq 'y' then begin

     inds=0
     if (rho1(istart) gt nscrit) then tsmin(inds)=t(istart) 
     for i=istart,iend-1 do begin
       if ( (rho1(i) ge nscrit) $
          and (rho1(i+1) lt nscrit) ) then begin
           tsmax(inds)=t(i) & inds=inds+1 
           print,inds-1,tsmax(inds-1),rho1(i+1)
       endif
       if ( (rho1(i) lt nscrit) $
          and (rho1(i+1) ge nscrit) ) then tsmin(inds)=t(i+1)
     endfor
     if (rho1(iend) gt nscrit) then begin
        tsmax(inds)=t(i) & inds=inds+1 & 
           print,'last:',inds-1,tsmax(inds-1),rho1(iend)
     endif
     indstot=inds-1

;     inds=0
;     if (temp1(istart) lt tscrit) then tsmin(inds)=t(istart) 
;     for i=istart,iend-1 do begin
;       if ( (temp1(i) le tscrit) $
;          and (temp1(i+1) gt tscrit) ) then begin
;           tsmax(inds)=t(i) & inds=inds+1 & endif
;       if ( (temp1(i) gt tscrit) $
;          and (temp1(i+1) le tscrit) ) then tsmin(inds)=t(i+1)
;     endfor
;     if (temp1(iend-1) lt tscrit) then begin
;        tsmax(inds)=t(i) & inds=inds+1 & endif
;     indstot=inds-1
     
     colpol=240
     if withps ne 'y' then colpol=120 
     for inds=0,indstot do begin
       xaa=(tsmin(inds)-min)*(xe-xa)/(max-min)+xa
       xee=(tsmax(inds)-min)*(xe-xa)/(max-min)+xa
       xs=[xaa,xee,xee,xaa]
       ys=[ylo4,ylo4,yup1,yup1]
       POLYFILL, xs, ys, COLOR = colpol, /norm
     endfor
   endif

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([rho1,babs]) &   print, 'B max: ',bmax
        bmin=min([rho1,babs]) &   print, 'B min: ',bmin
        bmin=0.0
;        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, rho1, $
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=9,xtickname=names,/noerase
	oplot, t, babs, line=1
;	ytick_get=vv

        tempmax=max(temp1) & print, 'Temp, max: ',tempmax
        temprange = 1000.   & tempscale = bmax/temprange
	axis,yaxis=1,yrange=[0,temprange],ystyle=1
	oplot, t, tempscale*temp1, line=2
        xt0=min-0.1*del  &   yt0=bmin+0.9*delb    
        yt0a=bmin+0.68*delb 
        xt1=max+0.12*del  &   yt1=bmin+0.9*delb    
        xt2=max+0.09*del  &   yt2=bmin+0.68*delb 
        yt3=bmin+0.44*delb  & yt4=bmin+0.20*delb 
        xt2a=max+0.12*del   & yt2a=bmin+0.57*delb 
        yt3a=bmin+0.33*delb & yt4a=bmin+0.09*delb
        xyouts, xt0, yt0, 'B',charsize=1.8
        xyouts, xt0, yt0a, 'N',charsize=1.8
        xyouts, xt1, yt1,'T',charsize=1.8
        xyouts, xt2, yt2,'Dens. (cm!U-3!N)',charsize=1.5
        xyouts, xt2a, yt2a,'N ___',charsize=1.5
        xyouts, xt2, yt3, 'Magn.F. (nT)',charsize=1.5
        xyouts, xt2a, yt3a, 'B .....',charsize=1.5
        xyouts, xt2, yt4, 'Temp. (eV)',charsize=1.5
        xyouts, xt2a, yt4a, 'T _ _',charsize=1.5

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([p1,pb1,ptot1]) & print, 'p max: ',bmax
        bmin=min([p1,pb1,ptot1]) & print, 'p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        bmin=0.0
        delb=bmax
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, ptot1,$
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=1,xtickname=names,/noerase
	oplot, t, p1, line=1
	oplot, t, pb1, line=2
        xt1=max+0.02*del   &  yt1=bmin+0.73*delb    
        xt1a=max+0.04*del  &  yt1a=bmin+0.60*delb    
        xt2=max+0.04*del   &  yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Pressure',charsize=1.5
        xyouts, xt1a, yt1a,'(nPascal)',charsize=1.5
        xyouts, xt2, yt2,'P!Dtot!N ___',charsize=1.5
        xyouts, xt2, yt3, 'P!Dth!N ......',charsize=1.5
        xyouts, xt2, yt4, 'P!DB!N _ _',charsize=1.5
        
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([vx1,vy1,vz1]) &   print, 'V max: ',bmax
        bmin=min([vx1,vy1,vz1]) &   print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.0*delb & bmin=bmin-0.1*delb & delb=bmax-bmin
; vx(sat) = vy(sim)
	plot, t, vy1, $
	   yrange=[bmin,bmax], $
	   xstyle=1,ystyle=1,xtickname=names, line=2,/noerase
; vy(sat) = vz(sim)
	oplot, t, vz1, line=1
; vz(sat) = vx(sim)
	oplot, t, vx1, line=0
        yt1=bmin+0.88*delb    
        yt1a=bmin+0.75*delb    
        yt2=bmin+0.60*delb    
        yt3=bmin+0.45*delb    
        yt4=bmin+0.30*delb    
        xyouts, xt1, yt1,'Velocity',charsize=1.5
        xyouts, xt1a, yt1a,'(km s!U-1!N)',charsize=1.5
        xyouts, xt2, yt2,'V!Dx!N _ _',charsize=1.5      ;line=2
        xyouts, xt2, yt3, 'V!Dy!N ......',charsize=1.5  ;line=1
        xyouts, xt2, yt4, 'V!Dz!N ___',charsize=1.5     ;line=0
  

; x(sim) =>  z(sat) ; vx(sim) =>  vz(sat) 
; y(sim) =>  x(sat) ; vy(sim) =>  vx(sat)
; z(sim) =>  y(sat) ; vz(sim) =>  vy(sat)
    dxrange=!x.range(1)-!x.range(0)
    dyrange=1.1*delb
;    print,'dxrange,dyrange:',dxrange,dyrange
;    print,'dpx,dpy:',dpx,dpy
;    print,'psxsize,psysize:',psxsize,psysize
    vscale=0.5

    oy=bmin+0.1*delb
    for i=istart,iend do begin
     ox=t(i) 
;     dx=-vysat=-vxsim    dy=vxsat=-vysim
     dx=vx1(i)*vscale*dxrange/dyrange/dpx*dpy/psxsize*psysize
     dy=(vy1(i)+vzdiff)*vscale
     plots,[ox,ox+dx],[oy,oy+dy]
    endfor
    ox=t(iend)+0.08*dxrange
    oy=bmin-0.13*delb
    dx=400.*vscale*dxrange/dyrange*dpy/dpx*psysize/psxsize
    dy=400.*vscale
    plots,[ox,ox+dx],[oy,oy]
    plots,[ox,ox],[oy,oy+dy]
    yv1=bmin+0.10*delb
    yv2=bmin-0.07*delb
    xyouts, xt2, yv1,'V!Dx!N  400km/s',charsize=1.2
    xyouts, xt2a, yv2, 'V!Dz!N',charsize=1.3


	!P.POSITION=[xa,ylo4,xe,yup4]
        bmax=max([bx1,by1,bz1]) &   print, 'B max: ',bmax
        bmin=min([bx1,by1,bz1]) &   print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.1*delb & bmin=bmin-0.0*delb & delb=bmax-bmin
; bx(sat) = by(sim)
	plot, t, by1, $
	   yrange=[bmin,bmax], $
	   xstyle=1,ystyle=1,xtitle=xtit, line=1,/noerase
; by(sat) = bz(sim)
	oplot, t, bz1, line=2
; bz(sat) = bx(sim)
	oplot, t, bx1, line=0
        yt1=bmin+0.73*delb   
        yt1a=bmin+0.60*delb   
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Magn. Field',charsize=1.5
        xyouts, xt1a, yt1a,'(nT)',charsize=1.5
        xyouts, xt2, yt2,'B!Dx!N _ _',charsize=1.5      ;line=2
        xyouts, xt2, yt3, 'B!Dy!N ......',charsize=1.5  ;line=1
        xyouts, xt2, yt4, 'B!Dz!N ___',charsize=1.5     ;line=0
        
     xyouts, 0.05, 0.2, 'Probe index: '+string(index,'(i3)'), $
       charsize=1.2, /norm
     xyouts, 0.25, 0.2,'Rotation angle for y,z comp.:'+string(phi,'(i3)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.17, $
       'Initial probe location and vel. (simulation frame., normalized):',$
       charsize=1.2, /norm
     xyouts, 0.2, 0.145, 'x = '+string(-ysat1,'(f7.1)')$
                     +'   y = '+string(xsat1,'(f7.1)')$
                     +'   z = '+string(zsat1,'(f7.1)'),$
       charsize=1.2, /norm
     xyouts, 0.2, 0.12, 'V!Dx!N = '+string(-vysat1,'(f7.3)')$
                    +'   V!Dy!N = '+string(vxsat1,'(f7.3)')$
                    +'   V!Dz!N = '+string(vzsat1,'(f7.3)'),$
       charsize=1.2, /norm
       
     xyouts, 0.05, 0.09, $
       'Initial probe location and vel. (MSP frame, in km and km/s):',$
       charsize=1.2, /norm
     xyouts, 0.2, 0.065, 'x = '+string(-ysat2,'(i5)')$
                     +'   y = '+string(xsat2,'(i5)')$
                     +'   z = '+string(zsat2,'(i5)'),$
       charsize=1.2, /norm
     xyouts, 0.2, 0.04, 'V!Dx!N = '+string(-vysat2,'(i4)')$
                    +'   V!Dy!N = '+string(vxsat2,'(i4)')$
                    +'   V!Dz!N = '+string(vzsat2,'(i4)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.01, $
      'DATA is plotted in the probe rest frame in rotated (MSP) coord.',$
       charsize=1.2, /norm
        

     if withps eq 'y' then begin
       device,/close  &  set_plot,'x' & withps='n' 
     endif
     !P.THICK=1.
     !X.THICK=1.
     !Y.THICK=1.
     !P.CHARTHICK=2.

     goto, cut


end

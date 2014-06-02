; PROGRAM to read and plot satellite data

  nsat = long(60) & nt = 4001 & pi = 3.14159265536
  nnorm = 20.0 & bnorm = 10.0 &  lnorm= 200.0
  vnorm = 21.8*bnorm/sqrt(nnorm) & pnorm = 0.01*bnorm^2./8.0/pi
  tnorm = lnorm/vnorm
  pbd = fltarr(2) & rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
  bxbd = pbd & bybd = pbd & bzbd = pbd
  zeit = 0.0 & startime = 0.0 & time = fltarr(nt)
  pi = 3.14159265536 & phi0=00.0 & phi=00.0
  
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
     time(i)=zeit & bxc(*,i)=bxs & byc(*,i)=bys & bzc(*,i)=bzs
     vxc(*,i)=vxs & vyc(*,i)=vys & vzc(*,i)=vzs
     rhoc(*,i)=rhos & pc(*,i)=ps & bc(*,i)=bs & ptotc(*,i)= ps+bs^2
     xc(*,i)=xsat &  yc(*,i)=ysat &  zc(*,i)=zsat
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
  print, 'Options:           o -> original angle:',phi0
  print, '                   m -> phi=atan(bybd(1)/bzbd(1))'
  print, '     integer or real -> new choice of angle (in degrees)'
  print, '              return -> no change'
  read, whatangle
  if (whatangle eq 'o') then phi=phi0
  if (whatangle eq 'm') then phi=180.0/pi*atan(bybd(1)/bzbd(1))
  if ( (whatangle ne '') and (whatangle ne 'o') $
     and (whatangle ne 'm') )    then     phi=float(whatangle)
  print, 'Chosen angle: ',phi
  phir = phi*pi/180.0  
  cphi=cos(phir) & sphi=sin(phir)
  vyrot = vybd*cphi - vzbd*sphi  & vzrot = vybd*sphi + vzbd*cphi
  byrot = bybd*cphi - bzbd*sphi  & bzrot = bybd*sphi + bzbd*cphi
  
  vysatr = vysat*cphi - vzsat*sphi  & vzsatr = vysat*sphi + vzsat*cphi
  ysatr  = ysat0*cphi - zsat0*sphi  & zsatr = ysat0*sphi + zsat0*cphi
  
  
; CHOOSE SATELLITE INDEX
;------------------------
cut:
  vyout=vybd & vzout=vzbd & byout=bybd & bzout=bzbd
  ysato=ysat0 & zsato=zsat0 & vysato=vysat & vzsato=vzsat 
  if (coor eq 'm') then begin
    vyout=vyrot & vzout=vzrot & byout=byrot & bzout=bzrot 
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
    
  print, 'CHOOSE SATELLITE INDEX: '
  print, 'Present choice:  Probe index =',index,'   Rotation angle =',phi
  print, 'Options:   integer -> satellite index'
  print, '            return -> no changes applied'
  print, '                 a -> change rotation angle'
  print, '                 m -> print data in rotated coord'
  print, '                 s -> print data in simulation coord'
  print, '                 p -> postscrip output'
  print, '                 q -> terminate'
  read, whatindex
  if whatindex eq '' then print,'index=',index,' not altered'
  if whatindex eq 'a' then goto, angle
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
       device,filename='satwp'+string(index,'(i2.2)')+'.ps'
       device,/portrait
       device,/inches,xsize=4.5,scale_factor=1.0,xoffset=0.5
       device,/inches,ysize=6.,scale_factor=1.0,yoffset=0.5
  endif
  if whatindex eq 'q' then stop
  if ( (whatindex ne '') and (whatindex ne 'a') and (whatindex ne 'm') $
     and (whatindex ne 's') and (whatindex ne 'p') $
     and (whatindex ne 'q') )    then begin
    index=fix(whatindex)
    print, 'Chosen satellite index: ',index
  endif
  

  t=fltarr(itot+1) & t=time(0:itot)
  x1=t & y1=t & z1=t & bx1=t & by1=t & bz1=t & vx1=t & vy1=t & vz1=t
  rho1=t & p1=t & b1=t & ptot1=t & temp1=t
; NOTE: the actual path through the simulation domain is given by 
; xc,yc,zc, however, for periodic systems this can lead to exit and entry
; into the simulation domain.
; Anyhow, of interest is really onle the trajectory relative to 
; the magnetospheric frame.
; This is:
  x1=xsat0(index)*(t-startime) 
  bx1=bxc(index,0:itot) & by2=byc(index,0:itot) & bz2=bzc(index,0:itot)
  vx1=vxc(index,0:itot) & vy2=vyc(index,0:itot) & vz2=vzc(index,0:itot)
  rho1=rhoc(index,0:itot) & p1=pc(index,0:itot) & b1=bc(index,0:itot)  
  ptot1=ptotc(index,0:itot) & temp1=p1/rho1 & pb1=b1^2  & beta=p1/b1^2
  t=tnorm*time(0:itot)
; in rotated coordinates:
  y1=ysatr(index)*(t-startime) & z1=zsatr(index)*(t-startime)  
  vy1 = vy2*cphi - vz2*sphi & vz1 = vy2*sphi + vz2*cphi
  by1 = by2*cphi - bz2*sphi & bz1 = by2*sphi + bz2*cphi

; UNCOMMENT FOR DIFFERENT NORMALISATION  
;--------------------------------------
;     nnorm = 15.0
;     bnorm = 20.0
;     lnorm = 250.0
;     vnorm = 21.8*bnorm/sqrt(nnorm)
;     pnorm = 0.01*bnorm^2.0/8.0/pi
;     tnorm = lnorm/vnorm
  bx1=bnorm*bx1 & by1=bnorm*by1 & bz1=bnorm*bz1
  vx1=vnorm*vx1 & vy1=vnorm*vy1 & vz1=vnorm*vz1
  rho1=nnorm*rho1 & p1=pnorm*p1 & pb1=pnorm*pb1 & ptot1=pnorm*ptot1
; for simulation frame
  xsat1=xsat0(index)    & ysat1=ysat0(index)   & zsat1=zsat0(index) 
  vxsat1=vxsat(index)  & vysat1=vysat(index)+vybd(1)
  vzsat1=vzsat(index)+vzbd(1) 
; for rotated, magnetospheric frame with physical units
  xsat2=lnorm*xsat0(index) & ysat2=lnorm*ysatr(index) & zsat2=lnorm*zsatr(index) 
  vxsat2=vnorm*vxsat(index) & vysat2=vnorm*vysatr(index)
  vzsat2=vnorm*vzsatr(index) 


    dpx=0.8  & dpy=0.17
    xa=0.07 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.8 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3
    
       max = 300  
       min = 50  
;       max = t(itot)  
;       min = 50  ; min = t(0)
       del = max-min
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,1,4,0,0]
       !P.CHARSIZE=1.5
       !X.TICKS=0
       !Y.TICKS=0
       !X.TICKlen=0.04
       !Y.TICKlen=0.02
       !X.RANGE=[min,max]
;       !Y.RANGE=[ymin,ymax]

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max(rho1) & print, 'rho, max: ',bmax
        bmin=min(rho1) & print, 'rho, min: ',bmin
        bmin=0.0
;        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, rho1, $
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=9,xtickname=names
;	ytick_get=vv
        betamax=max(beta) & print, 'beta, max: ',betamax
        betarange = 9.0   & betascale = bmax/betarange
	axis,yaxis=1,yrange=[0,betarange],ystyle=1
	oplot, t, betascale*beta, line=2
        xt0=min-0.085*del  &   yt0=bmin+0.92*delb    
        xt1=max+0.04*del  &   yt1=bmin+0.89*delb    
        xt2=max+0.05*del  &   yt2=bmin+0.63*delb    
        xt2a=max+0.12*del  &   yt2a=bmin+0.51*delb    
        xt2b=max+0.1*del  &   yt2b=bmin+0.40*delb    
        yt3=bmin+0.25*delb  &   yt3a=bmin+0.13*delb    
        xyouts, xt0, yt0, '!7q!X',charsize= 0.90
        ;, /norm, alignment=0.5
        xyouts, xt2, yt1,'!7b!X',charsize= 0.90
        xyouts, xt2, yt2,'Density',charsize=0.9
        xyouts, xt2a, yt2a,'(cm!U-3!N)',charsize=0.9
        xyouts, xt2b, yt2b,'!7q!X ___',charsize=0.9
        xyouts, xt2, yt3, 'Plasma Beta',charsize=0.9
        xyouts, xt2b, yt3a, '!7b!X _ _',charsize=0.9

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([p1,pb1,ptot1]) & print, 'p max: ',bmax
        bmin=min([p1,pb1,ptot1]) & print, 'p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        bmin=0.0 & delb=bmax
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, ptot1,$
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=1,xtickname=names
	oplot, t, p1, line=1
	oplot, t, pb1, line=2
        xt1=max+0.02*del   &  yt1=bmin+0.73*delb    
        xt1a=max+0.04*del  &  yt1a=bmin+0.60*delb    
        xt2=max+0.04*del   &  yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Pressure',charsize=0.9
        xyouts, xt1a, yt1a,'(nPascal)',charsize=0.9
        xyouts, xt2, yt2,'P!Dtot!N ___',charsize=0.9
        xyouts, xt2, yt3, 'P!Dth!N ......',charsize=0.9
        xyouts, xt2, yt4, 'P!DB!N _ _',charsize=0.9
        
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([vx1,vy1,vz1]) &   print, 'V max: ',bmax
        bmin=min([vx1,vy1,vz1]) &   print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=bmax-bmin
	plot, t, vx1, $
	   yrange=[bmin,bmax], $
	   xstyle=1,ystyle=1,xtickname=names
	oplot, t, vy1, line=1
	oplot, t, vz1, line=2
        yt1=bmin+0.73*delb    
        yt1a=bmin+0.60*delb    
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Velocity',charsize=0.9
        xyouts, xt1a, yt1a,'(km s!U-1!N)',charsize=0.9
        xyouts, xt2, yt2,'V!Dx!N ___',charsize=0.9
        xyouts, xt2, yt3, 'V!Dy!N ......',charsize=0.9
        xyouts, xt2, yt4, 'V!Dz!N _ _',charsize=0.9
        
	!P.POSITION=[xa,ylo4,xe,yup4]
        bmax=max([bx1,by1,bz1]) &   print, 'B max: ',bmax
        bmin=min([bx1,by1,bz1]) &   print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=bmax-bmin
	plot, t, bx1, $
	   yrange=[bmin,bmax], $
	   xstyle=1,ystyle=1,xtitle=xtit 
	oplot, t, by1, line=1
	oplot, t, bz1, line=2
        yt1=bmin+0.73*delb   
        yt1a=bmin+0.60*delb   
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Magn. Field',charsize=0.9
        xyouts, xt1a, yt1a,'(nT)',charsize=0.9
        xyouts, xt2, yt2,'B!Dx!N ___',charsize=0.9
        xyouts, xt2, yt3, 'B!Dy!N ......',charsize=0.9
        xyouts, xt2, yt4, 'B!Dz!N _ _',charsize=0.9
        
     xyouts, 0.05, 0.2, 'Probe index: '+string(index,'(i3)'),charsize= 0.9,/norm
     xyouts, 0.35, 0.2,'Rotation angle for y,z comp.:'+string(phi,'(i3)'),$
       charsize=0.9, /norm
     xyouts, 0.05, 0.17, $
       'Initial probe location and vel. (simulation frame., normalized):',$
       charsize=0.9, /norm
     xyouts, 0.2, 0.145, 'x = '+string(xsat1,'(f7.1)')$
                     +'   y = '+string(ysat1,'(f7.1)')$
                     +'   z = '+string(zsat1,'(f7.1)'),$
       charsize=0.9, /norm
     xyouts, 0.2, 0.12, 'V!Dx!N = '+string(vxsat1,'(f7.3)')$
                    +'   V!Dy!N = '+string(vysat1,'(f7.3)')$
                    +'   V!Dz!N = '+string(vzsat1,'(f7.3)'),$
       charsize=0.9, /norm
       
     xyouts, 0.05, 0.09, $
       'Initial probe location and vel. (MSP frame, in km and km/s):',$
       charsize=0.9, /norm
     xyouts, 0.2, 0.065, 'x = '+string(xsat2,'(i5)')$
                     +'   y = '+string(ysat2,'(i5)')$
                     +'   z = '+string(zsat2,'(i5)'),$
       charsize=0.9, /norm
     xyouts, 0.2, 0.04, 'V!Dx!N = '+string(vxsat2,'(i4)')$
                    +'   V!Dy!N = '+string(vysat2,'(i4)')$
                    +'   V!Dz!N = '+string(vzsat2,'(i4)'),$
       charsize=0.9, /norm
     xyouts, 0.05, 0.01, $
      'DATA is plotted in the probe rest frame in rotated (MSP) coord.',$
       charsize=0.9, /norm
    

     if withps eq 'y' then begin
       device,/close  &  set_plot,'x' & withps='n' 
     endif
     !P.THICK=1.
     !X.THICK=1.
     !Y.THICK=1.
     !P.CHARTHICK=2.

   goto, cut


end

; PROGRAM to read and plot satellite data

  nsat = long(60) & nt = 4001 & pi = 3.14159265536
  nnorm = 20.0 & bnorm = 10.0 &  lnorm= 200.0
  vnorm = 21.8*bnorm/sqrt(nnorm) & pnorm = 0.01*bnorm^2./8.0/pi
  tnorm = lnorm/vnorm
  pbd = fltarr(2) & rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
  bxbd = pbd & bybd = pbd & bzbd = pbd
  zeit = 0.0 & startime = 0.0 & time = fltarr(nt)
  
  whatindex='0' & index=0 & names=strarr(15) & names=replicate(' ',15)
  contin='' & again='y' & withps='n' & fall = '' & change=''
  xtit='time'

; READ INPUT DATA FOR NSAT SATELLITES
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
;  z-maximum variance of B, i.e., z is locally northward for antiparallel       ;            fields.
;  y- complets coordinate system; in 2D y is the invariant direction for         ;            reconnection
  pi = 3.14159265536 & phi=90.0
  phir = phi*pi/180.0 & cphi=cos(phir) & sphi=sin(phir)
  vyrot = vybd*cphi - vzbd*sphi  & vzrot = vybd*sphi + vzbd*cphi
  byrot = bybd*cphi - bzbd*sphi  & bzrot = bybd*sphi + bzbd*cphi
  vybd = vyrot & vzbd = vzrot
  bybd = byrot & bzbd = bzrot
  vysatr = vysat*cphi - vzsat*sphi  & vzsatr = vysat*sphi + vzsat*cphi
  ysatr  = ysat0*cphi - zsat0*sphi  & zsatr = ysat0*sphi + zsat0*cphi
  vysat = vysatr  &  vzsat = vzsatr
  ysat0 = ysatr   &  zsat0 = zsatr
  

; CHOOSE SATELLITE INDEX
cut:
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
       rhobd(0),pbd(0),vxbd(0),vybd(0),vzbd(0),bxbd(0),bybd(0),bzbd(0)
  print, format='(8f9.3)',$
       rhobd(1),pbd(1),vxbd(1),vybd(1),vzbd(1),bxbd(1),bybd(1),bzbd(1)
  print, 'Initial satellite locations and velocity:'
  print, 'sat  !    x    !    y    !    z    !    vx   !    vy   !    vz   '
  for i=0,nsat-1 do $
    print, format='(i3,6f10.2)',$
           i,xsat0(i),ysat0(i),zsat0(i),vxsat(i),vysat(i),vzsat(i)
  print, $
  'Satellite velocity is relative to restframe of the magnetosphere (at xmin)!!!'
  print, 'Plasma velocity is recorded in the satellite frame!'


  print, 'CHOOSE SATELLITE INDEX: '
  print, 'Present choice:',index
  print, 'Options:   integer -> satellite index'
  print, '            return -> no changes applied'
  print, '                 q -> terminate'
  read, whatindex
  if whatindex eq 'q' then stop
  if whatindex ne '' then begin
    index=fix(whatindex)
    print, 'Chosen satellite index: ',index
  endif
  if whatindex eq '' then print,'choice=',index,' not altered'


  t=fltarr(itot+1) & t=time(0:itot)
  x1=t & y1=t & z1=t & bx1=t & by1=t & bz1=t & vx1=t & vy1=t & vz1=t
  rho1=t & p1=t & b1=t & ptot1=t & temp1=t
  max = time(itot)  & min = time(0)  & del = max-min
  x1=xc(index,0:itot) & y1=yc(index,0:itot) & z1=zc(index,0:itot)
  bx1=bxc(index,0:itot) & by1=byc(index,0:itot) & bz1=bzc(index,0:itot)
  vx1=vxc(index,0:itot) & vy1=vyc(index,0:itot) & vz1=vzc(index,0:itot)
  rho1=rhoc(index,0:itot) & p1=pc(index,0:itot) & b1=bc(index,0:itot)  
  ptot1=ptotc(index,0:itot) & temp1=p1/rho1
  
; UNCOMMENT FOR DIFFERENT NORMALISATION  
;     nnorm = 15.0
;     bnorm = 20.0
;     lnorm = 200.0
;     vnorm = 21.8*bnorm/sqrt(nnorm)
;     pnorm = 0.01*bnorm^2.0/8.0/pi
;     tnorm = lnorm/vnorm
  xsat1=x1(0)    & ysat1=y1(0)    & zsat1=z1(0) 
  xsat2=x1(itot) & ysat2=y1(itot) & zsat2=z1(itot) 
  vxsat1=vxsat     & vysat1=vysat     & vzsat1=vzsat 
  vxsat2=(vxsat+vxbd(0)) 
  vysat2=(vysat+vybd(0)) 
  vzsat2=(vzsat+vzbd(0)) 

  
    dpx=0.68 & dpy=0.17
    xa=0.07 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    
    ylo1=0.79 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3
    
    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
       !P.THICK=2.
       !X.THICK=1.5
       !Y.THICK=1.5
       !P.CHARTHICK=3.
        set_plot,'ps'
        device,filename='sat'+string(index,'(i2.2)')+'.ps'
        device,/portrait
        device,/inches,xsize=6.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=7.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
     endif

       max = t(itot) & min = t(0)  & del = max-min
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,1,4,0,0]
       !P.CHARSIZE=2.5
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
        beta=p1/b1^2
        betamax=max(beta) & print, 'beta, max: ',betamax
        betarange = 9.0   & betascale = bmax/betarange
	axis,yaxis=1,yrange=[0,betarange],ystyle=1
	oplot, t, betascale*beta, line=2
        xt0=min-0.08*del  &   yt0=bmin+0.89*delb    
        xt1=max+0.04*del  &   yt1=bmin+0.89*delb    
        xt2=max+0.05*del  &   yt2=bmin+0.50*delb    
        xt2a=max+0.1*del  &   yt2a=bmin+0.40*delb    
        yt3=bmin+0.25*delb  &   yt3a=bmin+0.13*delb    
        xyouts, xt0, yt0, '!7q!X',charsize=1.8
        ;, /norm, alignment=0.5
        xyouts, xt1, yt1,'!7b!X',charsize=1.8
        xyouts, xt2, yt2,'Density',charsize=1.5
        xyouts, xt2a, yt2a,'!7q!X ___',charsize=1.5
        xyouts, xt2, yt3, 'Plasma Beta',charsize=1.5
        xyouts, xt2a, yt3a, '!7b!X _ _',charsize=1.5

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([p1,b1^2,ptot1]) & print, 'p max: ',bmax
        bmin=min([p1,b1^2,ptot1]) & print, 'p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, ptot1,$
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=1,xtickname=names
	oplot, t, p1, line=1
	oplot, t, b1^2, line=2
        xt1=max+0.02*del   &  yt1=bmin+0.63*delb    
        xt2=max+0.04*del   &  yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Pressure',charsize=1.5
        xyouts, xt2, yt2,'P!Dtot!N ___',charsize=1.5
        xyouts, xt2, yt3, 'P!Dth!N ......',charsize=1.5
        xyouts, xt2, yt4, 'P!DB!N _ _',charsize=1.5
        
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
        yt1=bmin+0.63*delb    
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Velocity',charsize=1.5
        xyouts, xt2, yt2,'V!Dx!N ___',charsize=1.5
        xyouts, xt2, yt3, 'V!Dy!N ......',charsize=1.5
        xyouts, xt2, yt4, 'V!Dz!N _ _',charsize=1.5
        
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
        yt1=bmin+0.63*delb   
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Magn. Field',charsize=1.5
        xyouts, xt2, yt2,'B!Dx!N ___',charsize=1.5
        xyouts, xt2, yt3, 'B!Dy!N ......',charsize=1.5
        xyouts, xt2, yt4, 'B!Dz!N _ _',charsize=1.5

     xyouts, 0.05, 0.2, 'SAT index: '+string(index,'(i3)'), charsize=1.2, /norm
     xyouts, 0.05, 0.17,'Rotationangle for y,z components:'+string(phi,'(i3)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.14, 'Initial location (in simulation frame):',$
       charsize=1.2, /norm
     xyouts, 0.3, 0.12, 'x = '+string(xsat1,'(f6.2)')$
                    +'   y = '+string(ysat1,'(f6.2)')$
                    +'   z = '+string(zsat1,'(f6.2)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.10, 'Final location:',$
       charsize=1.2, /norm
     xyouts, 0.3, 0.10, 'x = '+string(xsat2,'(f6.2)')$
                    +'   y = '+string(ysat2,'(f6.2)')$
                    +'   z = '+string(zsat2,'(f6.2)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.07, 'Velocity (km/s)',$
       charsize=1.2, /norm
     xyouts, 0.07, 0.05, 'Relative to mspheric frame:',$
       charsize=1.2, /norm
     xyouts, 0.4, 0.05, 'V!Dx!N = '+string(vxsat1(index),'(f6.3)')$
                    +'   V!Dy!N = '+string(vysat1(index),'(f6.3)')$
                    +'   V!Dz!N = '+string(vzsat1(index),'(f6.3)'),$
       charsize=1.2, /norm
     xyouts, 0.07, 0.03, 'In simulation frame:',$
       charsize=1.2, /norm
     xyouts, 0.4, 0.03, 'V!Dx!N = '+string(vxsat2(index),'(f6.3)')$
                    +'   V!Dy!N = '+string(vysat2(index),'(f6.3)')$
                    +'   V!Dz!N = '+string(vzsat2(index),'(f6.3)'),$
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

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
  xmax =   12. & ymax = 1100.
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
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx & tp=bx & t0=bx
   rhono=bx & rhono2=bx & rhonn2=bx & tea=bx & tna=bx & tia=bx
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   p=bx & pp=bx & pe=bx & pn=bx & p1=bx & pn1=bx
   gradp=bx & gradpe=bx & gradpex=bx   
   nuei=bx & nuen=bx & nuin=bx & res=bx & ioniz=bx
   ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx 
   exs=bx & eys=bx & ezs=bx & angle=bx
   ex_dpe=bx & ey_dpe=bx
   sigmap=bx & sigmah=bx & s0=bx & jp=bx & jh=bx
   sigmapi=bx & sigmape=bx & sigmahi=bx & sigmahe=bx
   sigma0=bx & sigma0i=bx & sigma0e=bx
   f1=bx & f2=bx & f3=bx & f4=bx 
   jxs=fltarr(nx,/NOZERO) & jzs=jxs & jps=jxs & jhs=jxs
   sigmaps=jxs & sigmahs=jxs & exs=jxs & ezs=jxs

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin ;,ioniz
   readu, 8,  jx,jy,jz
;   readu, 8,  ischritt
;   readu, 8,  xmin
;   aequi=fltarr(2,/NOZERO)
;   readu, 8,  aequi
;   readu, 8,  isat
;   s1=fltarr(7,/NOZERO) & s2=fltarr(3,/NOZERO)
;   readu, 8,  ff,s1,s1,s2,s2,s2,s2,s1,s1,s1,ss,s1,s1,s1,mo,tt,echar,je
   close, 8
;   print, 'echar: ',echar, 'je: ',je
;   print, 'f107: ',ff,'sangle: ',ss,'mo: ',mo,' t0: ',tt

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
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhon1,un1,nu12s,nu12a,ioniz
   close, 8
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon

   pn=2*un^(5.0/3.0) & pn1=2*un1^(5.0/3.0)
   rhont=rhono+rhono2/2+rhonn2*4/7
   meff = rhon1/rhont
   tna = pn1/rhont & tia=tna & tea=p1/rho1-tia

     y = h0 + y/rescal
     dy = dy*rescal
     by = by/rescal
     vy = vy/rescal
     vey = vey/rescal
     vny = vny/rescal
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
  
    xa=0.25 & xe=0.65 & dpy=0.34
    ylo1=0.55 & yup1=ylo1+dpy
    ylo2=0.10 & yup2=ylo2+dpy
;    ylo3=0.05 & yup3=ylo3+dpy
   
   ytit='z'
  
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
   
;   nuen = nuen/ta_time
;   nuei = nuei/ta_time
;   nuin = nuin/ta_time

   lambdam = ta_time*(30/5.64)*sqrt(1./16./1836.2/dens0)
   ex_dpe=-gradpex*t_units*1.38/1.6e-2/rho/e_units
   ey_dpe=-gradpe*t_units*1.38/1.6e-2/rho/e_units
   ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx +ex_dpe
   ey=nuen*vy*lambdam-vez*bx+vex*bz+res*jy +ey_dpe
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
   sigma0e = s0*w_ce/(nuen+nuei)
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

;--------------------------------------------------------------------------
; Calculate high integrated conductivities
;--------------------------------------------------------------------------
   for j=0,nx-1 do begin
      sigmaps(j) = 0.0
      sigmahs(j) = 0.0
   endfor
   for j=0,nx-1 do begin
      for k=0,70 do begin
        sigmaps(j) = sigmaps(j) $
                    +(sigmap(j,k)+sigmap(j,k+1))/2.0*(y(k+1)-y(k))*l_units
        sigmahs(j) = sigmahs(j) $
                    +(sigmah(j,k)+sigmah(j,k+1))/2.0*(y(k+1)-y(k))*l_units 
      endfor
   endfor
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
; Calculate high integrated current
;--------------------------------------------------------------------------
   for j=0,nx-1 do begin
      jxs(j) = 0.0
      jzs(j) = 0.0
   endfor
   for j=0,nx-1 do begin
      for k=0,31 do begin
        jxs(j) = jxs(j) $
                +(jx(j,k)+jx(j,k+1))/2.0*(y(k+1)-y(k))*l_units*j_units
        jzs(j) = jzs(j) $
                +(jz(j,k)+jz(j,k+1))/2.0*(y(k+1)-y(k))*l_units*j_units
      endfor
   endfor
   exs=ex(*,5) & ezs=ez(*,5)
   jps= (sigmaps*exs-sigmahs*ezs)*e_units
   jhs= (sigmahs*exs+sigmaps*ezs)*e_units
;--------------------------------------------------------------------------
     while (again eq 'y') or (again eq '') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
;        device,/times,/bold,font_index=3
       endif
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,0,2,0,0]
       !P.CHARSIZE=1.0
       !P.FONT=3
       !X.TICKS=6
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       
        !X.RANGE=[xmin,xmax]
;        !X.RANGE=[2,4]

       
  print, 'plot first page, hight integrated current?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;-----------------------------------------------------------------------
; plot first page ( Rhon, Pn, Rho, P, T)
;-----------------------------------------------------------------------

	!P.POSITION=[xa,ylo1,xe,yup1]
;        bmax=max([jxs,jps]) & print, 'jxs,jps max: ',bmax
;        bmin=min([jxs,jps]) & print, 'jxs,jps min: ',bmin
	bmax = 150.0
	bmin = -150.0
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=xmax+0.015*(xmax-xmin)
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/24. 
        ypos2=bmin+delb/72. 
        ypos3=bmin+delb/216. 
        ypos4=bmin+delb/648. 
	plot, x, jxs, title='Height integrated Pedersen current (mA/m)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, x, jps, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
;	xyouts,xpos,ypos4,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
;        bmax=max([-jzs,-jhs]) & print, 'jys,jhs max: ',bmax
;        bmin=min([-jzs,-jhs]) & print, 'Rho,P min: ',bmin
	bmax = 200.0
	bmin = -200.0
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=xmax+0.015*(xmax-xmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, x, -jzs,title='Height integrated Hall current (mA/m)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, x, -jhs, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
  endif
  
  print, 'plot second page, hight integrated conductivities?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;-----------------------------------------------------------------------
; plot first page ( Rhon, Pn, Rho, P, T)
;-----------------------------------------------------------------------

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([sigmaps]) & print, 'jxs,jps max: ',bmax
        bmin=min([sigmaps]) & print, 'jxs,jps min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=xmax+0.015*(xmax-xmin)
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/24. 
        ypos2=bmin+delb/72. 
        ypos3=bmin+delb/216. 
        ypos4=bmin+delb/648. 
	plot, x, sigmaps, title='Height integrated Pedersen conductivity', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
;	xyouts,xpos,ypos4,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([sigmahs]) & print, 'jys,jhs max: ',bmax
        bmin=min([sigmahs]) & print, 'Rho,P min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=xmax+0.015*(xmax-xmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, x, sigmahs,title='Height integrated Hall conductivity', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
  endif

     !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


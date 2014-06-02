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

   openr, 8, 'magtap01',/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   close, 8
   
   ntmax=5
   ix0=25
   h0=100.0

    print, 'Which case?'
    read, run

     while (again eq 'y') or (again eq '') do begin

gridindex:
  print, 'x', 'Coordinates:'
  for i=0,nx-1,2 do print, i,'  ','x =',x(i)
  print, 'Input - i =Grid Index of Chosen Plane(>0 and <',nx-1,')'
  print, 'Options:   integer -> grid index'
  read, ix0 

; READ INPUT DATA OF DIMENSION NX, NY
   openr, 8, 'magtap01',/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
;   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

          f1=fltarr(ny,ntmax)
          f2=f1 & f1a=f1 & f2a=f1 
          f3=f1 & f4=f1 & f5=f1 & f6=f1 & f7=f1 & f8=f1 & f9=f1 & f10=f1
          f11=f1 & f11a=f1 & f12=f1 & f12a=f1 & f13=f1 & f13a=f1
          f14=f1 & f15=f1 & f16=f1 & f17=f1 & f18=f1 & f19=f1 & f20=f1
          f21=f1 & f01=f1 & f02=f1
	  f30=f1
   close, 8

   for it=0,ntmax-1 do begin

; READ INPUT DATA OF DIMENSION NX, NY
	fnumber=string(it*10,form='(i2.2)')
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
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx & ti=bx & t0=bx
   rhono=bx & rhono2=bx & rhonn2=bx & tea=bx & tna=bx & tia=bx
   rhon=bx & un=bx & rhon1=bx & un1=bx & tn=bx 
   p=bx & pp=bx & pe=bx & pn=bx & p1=bx & pn1=bx
   nuei=bx & nuen=bx & nuin=bx & res=bx & ioniz=bx
   ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
   exs=bx & eys=bx & ezs=bx & angle=bx
   sigmap=bx & sigmah=bx & s0=bx & jp=bx & jh=bx
   sigmapi=bx & sigmape=bx & sigmahi=bx & sigmahe=bx
   sigma0=bx & sigma0i=bx & sigma0e=bx

; parameters for red line volume emission
   alp1=bx & gamath=bx & gama1=bx & gama2=bx & gama3=bx
   xo1d=bx & f6300=bx
   rte=bx & rtn=bx & xo=bx & xn2=bx & xo2=bx & xe=bx

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin ;,ioniz
   readu, 8,  jx,jy,jz
   close, 8

   vx=vx/rho &  vz=vz/rho & vy=vy/rho
   vex=vex/rho &  vez=vez/rho & vey=vey/rho
   p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0)
   pe=rho*te & pp=p-pe & t0=p1/rho1/2. & ti=pp/rho

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
   tna = pn1/rhont & tn=pn/rhont & tia=tna & tea=p1/rho1-tia

     y = h0 + y/rescal
     dy = dy*rescal
     by = by/rescal
     vy = vy/rescal
     vey = vey/rescal
     vny = vny/rescal
      gradp=(shift(p,0,-1)-shift(p,0,1))
      for j=1,ny-2 do gradp(*,j)=dy(j)*gradp(*,j)

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

   lambdam = ta_time*(30/5.64)*sqrt(1./16./1836.2/dens0)
   ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx
   ey=nuen*vy*lambdam-vez*bx+vex*bz+res*jy
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Calculate Red Line Volume Emission Rate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   rte = te*t_units*1000.0
   rtn = tna*t_units*1000.0
   alp1 = 1.9e-7*sqrt(300./rte)
   gamath =  0.406+0.357e-4*rte $
            -(0.333-0.183e-4*rte)*exp(-1.37e4/rte) $
            -(0.456+0.174e-4*rte)*exp(-2.97e4/rte)
   gamath = gamath*1.07e-10*sqrt(rte)*exp(-2.27e4/rte)
   beta2 = 5.3e-12
   a6300 = 7.1e-3
   a6364 = 2.2e-3
   gama1 = 2.0e-11*exp(107.8/rtn)
   gama2 = 2.9e-11*exp(67.5/rtn)
   gama3 = 8.1e-10*sqrt(rte/300.0)
   gama4 = 8.0e-12
   
   xo = rhono*n_units
   xo2 = rhono2/2*n_units
   xn2 = rhonn2*4.0/7.0*n_units
   xe = rho*n_units
   
   xo1d =  (gamath*xo+0.0*alp1*xe)*xe $
          /(a6300+a6364+gama1*xn2+gama2*xo2+gama3*xe+gama4*xo)
   f6300 = xo1d*(a6300+a6364) 
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

plane='x'
igrid=ix0
        
          plcoo=y(0:ny-1) 

          f1(*,it)=n_units*rhon(igrid,*)
          f2(*,it)=p_units*pn(igrid,*) 
          f3(*,it)=n_units*rho(igrid,*)
          f4(*,it)=p_units*p(igrid,*)
          f5(*,it)=t_units*tn(igrid,*)
          f6(*,it)=t_units*ti(igrid,*)
          f7(*,it)=t_units*te(igrid,*)
          f10(*,it)=e_units*ey(igrid,*)
          f11(*,it)=sigmap(igrid,*)
          f12(*,it)=sigmah(igrid,*)
          f13(*,it)=sigma0(igrid,*)
          f20(*,it)=ioniz(igrid,*)
          f30(*,it)=f6300(igrid,*)

     endfor

;     while (again eq 'y') or (again eq '') do begin

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
       !P.MULTI=[0,0,1,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=-1
       !X.TICKS=5
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       pmin=ymin & pmax=ymax
       
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

	!P.POSITION=[xe1,ylo,xa3,yup]
        bmax=15.0
        bmin=0.0
;        bmax=max([f7]) & print, 'Te max: ',bmax
;        bmin=min([f7]) & print, 'Te min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=pmax-pmin
;        xpos=pmax+0.015*(pmax-pmin)
        xpos=1.1*bmax
        ypos0=pmin+0.45*delb    
        ypos1=pmin+0.4*delb 
        ypos3=pmin+0.26*delb 
	plot, f7(*,0), plcoo, title='Te (1000K)', $
	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',line=0
        for it=1,ntmax-1 do  oplot, f7(*,it), plcoo, line=it

	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(x(igrid),'(f6.2)')

;	!P.POSITION=[xa2,ylo,xe2,yup]
;;        bmax=150.0
;;        bmin=0.0
;        bmax=max([f10]) & print, 'ez max: ',bmax
;        bmin=min([f10]) & print, 'ez min: ',bmin
;        if (ix0 ge 11) and (ix0 le 15) then begin
;          bmax=100.0 
;          bmin=0.0
;        endif
;        if (ix0 ge 0) and (ix0 lt 11) then begin
;          bmax=25.0 
;          bmin=-25.0
;        endif
;        if (ix0 gt 15) and (ix0 lt 35) then begin
;          bmax=25.0 
;          bmin=-25.0
;        endif
;       if ix0 gt 39 then begin
;          bmax=25.0 
;          bmin=-25.0
;        endif
;        if (ix0 ge 35) and (ix0 le 39) then begin
;          bmax=0.0 
;          bmin=-200.0
;        endif
;        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
;        delb=bmax-bmin
;        xpos=pmax+0.015*(pmax-pmin)
;	plot, f10(*,0), plcoo, title='ez (uV/m)', $
;	xrange=[bmin,bmax],xstyle=1,ystyle=1,ytitle='z',xticks=4,line=0
;        for it=1,ntmax-1 do  oplot, f10(*,it), plcoo, line=it

  endif

  print, 'plot the second page? red line emission'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
;--------------------------------------------------------------------------
;  Red line emission
;--------------------------------------------------------------------------  
	!P.POSITION=[xe1,ylo,xa3,yup]
        bmax=1.e5
        bmin=1.0e1
;        bmax=max([f30]) & print, 'Ray max: ',bmax
;        bmin=min([f30]) & print, 'Ray min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=pmax-pmin
        xpos=2*bmax
        ypos0=pmin+0.45*delb    
        ypos1=pmin+0.4*delb 
        ypos3=pmin+0.26*delb 
	plot_oi, f30(*,0), plcoo, title='Red line emission', $
	xrange=[bmin,bmax],xstyle=16,ystyle=1,$
	ytitle='z',xticks=4,$
	line=0,xtype=1
        for it=1,ntmax-1 do  oplot, f30(*,it), plcoo, line=it
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(x(igrid),'(f6.2)')

  endif
  

     !P.FONT=-1

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


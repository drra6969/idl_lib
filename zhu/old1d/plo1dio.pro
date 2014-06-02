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
  xmin =  -12. & ymin = 0.
  xmax =   12. & ymax = 200.
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
   vnx=bx & vny=bx & vnz=bx & meff=bx & nu12=bx & nu12a=bx
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx & tp=bx & t0=bx
   nuei=bx & nuen=bx & nuin=bx
   rhono=bx & rhono2=bx & rhonn2=bx & tea=bx & tna=bx & tia=bx
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   res=bx & nu12s=bx 
   p=bx & pp=bx & pe=bx & pn=bx & p1=bx & pn1=bx & jx=bx & jy=bx & jz=bx
   ex=bx & ey=bx & ez=bx 
   f1=bx & f2=bx & f3=bx & f4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
;   readu, 8,  rescal,b0,length0,lengthy0,dens0
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin
   readu, 8,  jx,jy,jz
;   print, 'rescal = ',rescal
   close, 8
;   h0 = 150.
   vx=vx/rho &  vz=vz/rho & vy=vy/rho
   vex=vex/rho &  vez=vez/rho & vey=vey/rho
   p=2*u^(5.0/3.0) & p1=2*u1^(5.0/3.0)
   pe=rho*te & pp=p-pe & t0=p1/rho1/2.

;      y = h0 + y/rescal
      dy = dy*rescal


;   bz = smooth(bz,11)
;   bz = smooth(bz,11)

   name='magnap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
;  print, x
;  print, y
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhon1,un1,nu12s
   close, 8
;   for iy=0, ny-1 do print, 'iy,rhon,rhon1:',iy, rhon(11,iy),rhon1(11,iy)
   vnx=vnx/rhon &  vnz=vnz/rhon & vny=vny/rhon
   pn=2*un^(5.0/3.0) & pn1=2*un1^(5.0/3.0)
;   tna=pn1/rhon1 & tia=tna & tea=p1/rho1-tia

   rhont=rhono+rhono2/2+rhonn2*4/7
   meff = rhon1/rhont
   tna = pn1/rhont & tia=tna & tea=p1/rho1-tia

     y = h0 + y/rescal
     dy = dy*rescal
     vy = vy/rescal
     vey = vey/rescal
     vny = vny/rescal
     by = by/rescal

;    for iv = 2, ny-2, 2 do bz(*,iv)=(bz(*,iv-1)+bz(*,iv+1))/2
;    for iv = 2, ny-2, 2 do vz(*,iv)=(vz(*,iv-1)+vz(*,iv+1))/2

   
      f1 = shift(bz,0,-1)-shift(bz,0,1)
      for j=1,ny-2 do f3(*,j)=dy(j)*f1(*,j)
      jx = f3
      f2 = shift(bz,-1,0)-shift(bz,1,0)
      for i=1,nx-2 do f4(i,*)=dx(i)*f2(i,*)
      jy = -f4

      jx([0,nx-1],*)=0.0 & jy([0,nx-1],*)=0.0 
      jx(*,[0,ny-1])=0.0 & jy(*,[0,ny-1])=0.0 
      gradp=(shift(p,0,-1)-shift(p,0,1))
      for j=1,ny-2 do gradp(*,j)=dy(j)*gradp(*,j)
      ex=-vey*bz+vez*by
;      ey=-vez*bx+vex*bz+res*jy
      ey=res*jy
      ez=-vex*by+vey*bx
   
;   for iv=0, ny-1 do vy(*,iv)=vy(*,iv)/rho(*,iv)-1.5*tanh(x)
;          print, 'igrid: ',21,'   vy: ', vy(21,*)

     
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
  
    xa=0.1 & xe=0.8 & dpy=0.24
    ylo1=0.65 & yup1=ylo1+dpy
    ylo2=0.35 & yup2=ylo2+dpy
    ylo3=0.05 & yup3=ylo3+dpy
   
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

   temp_units = 100./8/3.1415/1.38*2.
   n_units=dens0
   ta_time=0.40974
   v_units=1220.29/500.
;   vv_units=862./.5
   b_units=100.
   p_units=n_units*1.38*1.e-7*temp_units*100.
   curr_units = 1000./4./3.1415
   e_units = 1.6*1e-4*1e6
;   res_units = 10./4./3.1415/1.6
   res_units = e_units/curr_units


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
;    printf, 8, p(11,*)/rho(11,*)*temp_units
;    close, 8

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
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
       !X.TICKS=5
       !Y.TICKS=6
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       
        if (plane eq 'x') then begin
          plcoo=y(0:ny-1) & f1=y & f2=y & f1a=y & f2a=y 
          f3=y & f3a=y & f4=y & f5=y & f6=y & f7=y & f8=y & f9=y & f10=y
          f11=y & f11a=y & f12=y & f12a=y & fpi=y & fpe=y & f13=y & f13a=y
          f14=y & f15=y & f16=y & f14a=y & f15a=y & f16a=y 
          f17=y & f18=y & f19=y & f20=y & f21=y & f22=y & f23=y & f23a=y
	  f01=y & f02=y & f03=y & f04=y & f05=y & f06=y & f30=y
	  fvtex=y & fvtey=y & fvtez=y
	  f01=curr_units*jx(igrid,*) & f02=curr_units*jy(igrid,*)
	  f03=curr_units*jz(igrid,*) 
	  f04=e_units*ex(igrid,*) & f05=e_units*ey(igrid,*)
	  f06=e_units*ez(igrid,*)
          f1=n_units*rhont(igrid,*) & f2=p_units*pn(igrid,*) & 
          f3=temp_units*pn(igrid,*)/rhon(igrid,*)*meff(igrid,*)
          f3a=temp_units*tna(igrid,*)
          f1a=n_units*(rhon(igrid,*)-rhon1(igrid,*)) & 
          f2a=p_units*(pn(igrid,*)-pn1(igrid,*)) 
          f4=v_units*vnx(igrid,*) & f5=v_units*vny(igrid,*) & 
          f6=v_units*vnz(igrid,*)
          f7=res_units*res(igrid,*) & f8=nu12s(igrid,*) 
          f11=n_units*rho(igrid,*) & 
          f11a=n_units*(rho(igrid,*)-rho1(igrid,*))
          f12=p_units*p(igrid,*) & f12a=p_units*(p(igrid,*)-p1(igrid,*))
          fpi=p_units*pp(igrid,*) & fpe=p_units*pe(igrid,*)
          f13=temp_units*pp(igrid,*)/rho(igrid,*)
          f13a=temp_units*tia(igrid,*)
          f23= temp_units*te(igrid,*)
          f23a=temp_units*tea(igrid,*)
          f14=v_units*vx(igrid,*) & f15=v_units*vy(igrid,*) & 
          f16=v_units*vz(igrid,*) & f14a=v_units*vex(igrid,*) & 
          f15a=v_units*vey(igrid,*) & f16a=v_units*vez(igrid,*)
          f17=b_units*bx(igrid,*) & f18=b_units*(by(igrid,*)+500.)
          f19=b_units*bz(igrid,*)
          f20=gradp(igrid,*)
          f30=bx(igrid,*)*jz(igrid,*)-jx(igrid,*)*bz(igrid,*)
          f21=temp_units*(pn(igrid,*)/rhont(igrid,*)-pp(igrid,*)/rho(igrid,*))
          f22=res(igrid,*)*jy(igrid,*)*jy(igrid,*)/rho(igrid,*)
          fvtex=vex(igrid,*)*te(igrid,*)^1.5
          fvtey=vey(igrid,*)*te(igrid,*)^1.5
          fvtez=vez(igrid,*)*te(igrid,*)^1.5
        endif
        if (plane eq 'y') then begin
          plcoo=x & f1=x & f2=x & f1a=x & f2a=x 
          f3=x & f3a=x & f4=x & f5=x & f6=x & f7=x & f8=x & f9=x & f10=x
          f11=x & f11a=x & f12=x & f12a=y & fpi=x & fpe=x & f13=x & f13a=x
          f14=x & f15=x & f16=x & f14a=x & f15a=x & f16a=x
          f17=x & f18=x & f19=x & f20=x & f21=x & f22=x & f23=x & f23a=x
	  f01=x & f02=x & f03=x & f04=x & f05=x & f06=x & f30=x
	  fvtex=x & fvtey=x & fvtez=x
	  f01=curr_units*jx(*,igrid) & f02=curr_units*jy(*,igrid)
	  f03=curr_units*jz(*,igrid)
       	  f04=e_units*ex(*,igrid) & f05=e_units*ey(*,igrid)
	  f06=e_units*ez(*,igrid)
          f1=n_units*rhont(*,igrid) & f2=p_units*pn(*,igrid) & 
          f3=temp_units*pn(*,igrid)/rhon(*,igrid)*meff(*,igrid)
          f3a=temp_units*tna(*,igrid)
          f1a=n_units*(rhon(*,igrid)-rhon1(*,igrid)) & 
          f2a=p_units*(pn(*,igrid)-pn1(*,igrid)) 
          f4=v_units*vnx(*,igrid) & f5=v_units*vny(*,igrid) & 
          f6=v_units*vnz(*,igrid)
          f7=res_units*res(*,igrid) & f8=nu12s(*,igrid) 
          f11=n_units*rho(*,igrid) & 
          f11a=n_units*(rho(*,igrid)-rho1(*,igrid))
          f12=p_units*p(*,igrid) & f12a=p_units*(p(*,igrid)-p1(*,igrid))
          fpi=p_units*pp(*,igrid) & fpe=p_units*pe(*,igrid)
          f13=temp_units*pp(*,igrid)/rho(*,igrid) & 
          f13a=temp_units*tia(*,igrid)
          f14=v_units*vx(*,igrid) & f15=v_units*vy(*,igrid) & 
          f16=v_units*vz(*,igrid)
          f14a=v_units*vex(*,igrid) & f15a=v_units*vey(*,igrid) & 
          f16a=v_units*vez(*,igrid)
          f17=b_units*bx(*,igrid) & f18=b_units*(by(*,igrid)+500.) 
          f19=b_units*bz(*,igrid)
          f20=gradp(*,igrid)
          f30=bx(*,igrid)*jz(*,igrid)-jx(*,igrid)*bz(*,igrid)
          f21=temp_units*(pn(*,igrid)/rhont(*,igrid)-pp(*,igrid)/rho(*,igrid))
          f22=res(*,igrid)*jy(*,igrid)*jy(*,igrid)/rho(*,igrid)
          f23=temp_units*te(*,igrid)
          f23a=temp_units*tea(*,igrid)
          fvtex=vex(*,igrid)*te(*,igrid)^1.5
          fvtey=vey(*,igrid)*te(*,igrid)^1.5
          fvtez=vez(*,igrid)*te(*,igrid)^1.5
        endif
        !X.RANGE=[pmin,pmax]
;        !X.RANGE=[2,4]

       
  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;-----------------------------------------------------------------------
; plot first page ( Rhon, Pn, Rho, P, T)
;-----------------------------------------------------------------------

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f1,f2]) & print, 'rhon,pn max: ',bmax
        bmin=min([f1,f2]) & print, 'rhon,pn min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
;        bmin = 180
;        bmin = .001
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/24. 
        ypos2=bmin+delb/72. 
        ypos3=bmin+delb/216. 
        ypos4=bmin+delb/648. 
	plot_io, plcoo, f1, title='Nn (1/cm^3), Pn (nPa)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f2, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos4,plane+' ='+string(coord(igrid),'(f6.2)')
;	xyouts,xpos,ypos2,run
;	xyouts,xpos,ypos3,'Max='
;	xyouts,xpos,ypos4,'  '+string(fmax,'(f7.4)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f11,f12]) & print, 'Rho,P max: ',bmax
        bmin=min([f11,f12]) & print, 'Rho,P min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f11,title='N (1/cm^3), P (nPa)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f12*3000., line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f3,f13,f23]) & print, 'T max: ',bmax
        bmin=min([f3,f13,f23]) & print, 'T min: ',bmin
;        bmax=max([f3,f13]) & print, 'T max: ',bmax
;        bmin=min([f3,f13]) & print, 'T min: ',bmin
;        bmax=max([f13]) & print, 'T max: ',bmax
;        bmin=min([f13]) & print, 'T min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f3,title=' Tn, Ti, Te (1000K)',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f13, line=1
	oplot, plcoo, f23, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
  print, 'plot 2. page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin
  
;-----------------------------------------------------------------------
;    plot page 2 ( Vn, V, Ve)
;-----------------------------------------------------------------------

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f4,f5,f6]) & print, 'Vn max: ',bmax
        bmin=min([f4,f5,f6]) & print, 'Vn min: ',bmin
;        bmax=max([f4,f5]) & print, 'Vn max: ',bmax
;        bmin=min([f4,f5]) & print, 'Vn min: ',bmin
;        bmax=max([f5]) & print, 'Vn max: ',bmax
;        bmin=min([f5]) & print, 'Vn min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f4,title=' Vn Components (Km/s)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f5, line=1
	oplot, plcoo, f6, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f14,f15,f16]) & print, 'V max: ',bmax
        bmin=min([f14,f15,f16]) & print, 'V min: ',bmin
;        bmax=max([f14]) & print, 'V max: ',bmax
;        bmin=min([f14]) & print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f14,title=' V Components (Km/s)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f15, line=1
	oplot, plcoo, f16, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
	
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f14a,f15a,f16a]) & print, 'Ve max: ',bmax
        bmin=min([f14a,f15a,f16a]) & print, 'Ve min: ',bmin
;        bmax=max([f14a]) & print, 'Ve max: ',bmax
;        bmin=min([f14a]) & print, 'Ve min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f14a,title=' Ve Components (Km/s)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f15a, line=1
	oplot, plcoo, f16a, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

;        bmax=max([f20]) & print, 'gradp max: ',bmax
;        bmin=min([f20]) & print, 'gradp min: ',bmin
;        delb=bmax-bmin
;        if bmin eq bmax then bmax=bmin+1.0
;        delb=bmax-bmin
;        xpos=pmax+0.015*(pmax-pmin)
;        ypos0=bmin+0.9*delb    
;        ypos1=bmin+0.82*delb 
;        ypos2=bmin+0.64*delb 
;        ypos3=bmin+0.56*delb 
;        ypos4=bmin+0.3*delb 
;	plot, plcoo, f20,title='Pressure Gradient',xtitle=xtit, $
;	yrange=[0.0,bmax],xstyle=1,ystyle=1
;	oplot, plcoo, f18, line=1
;	oplot, plcoo, f19, line=2
;	xyouts,xpos,ypos0,'time:'
;	xyouts,xpos,ypos1,' '+string(time,'(f6.3)')
;	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
	
  endif
  
  print, 'plot 3. page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;-----------------------------------------------------------------------
;    plot page 3 ( B, Res, Nu12s )
;-----------------------------------------------------------------------

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f17,f18,f19]) & print, 'B max: ',bmax
        bmin=min([f17,f18,f19]) & print, 'B min: ',bmin
;        bmax=max([f17,f18]) & print, 'B max: ',bmax
;        bmin=min([f17,f18]) & print, 'B min: ',bmin
;        bmax=max([f17]) & print, 'B max: ',bmax
;        bmin=min([f17]) & print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f17,title='B Components (nT)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f18, line=1
;	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f19, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f7]) & print, 'Res max: ',bmax
        bmin=min([f7]) & print, 'Res min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/16. 
        ypos2=bmin+delb/32. 
        ypos3=bmin+delb/64. 
        ypos4=bmin+delb/128. 
	plot_io, plcoo, f7,title='Res (Ohm m)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
	
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f8]) & print, 'Nu12s max: ',bmax
        bmin=min([f8]) & print, 'Nu12s min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
;        bmax=10000.0 & bmin=1850.0
        ypos0=bmin+delb/8.  
        ypos1=bmin+delb/24. 
        ypos2=bmin+delb/72. 
        ypos3=bmin+delb/216. 
        ypos4=bmin+delb/648. 
	plot_io, plcoo, f8,title='Nu12s',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif

  print, 'plot 4. page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

;-----------------------------------------------------------------------
;    plot page 4 ( Del Rho, Del P, Del Rhon, Del Pn, Del T )
;-----------------------------------------------------------------------

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f11a,f12a]) & print, 'Del Rho,P max: ',bmax
;        bmax=200.
        bmin=min([f11a,f12a]) & print, 'Del Rho,P min: ',bmin
;        bmin=min([f11a]) & print, 'Del Rho min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f11a,title='Del Rho, P', $
;	plot, plcoo, f11a,title='Del Rho', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	oplot, plcoo, (f12a-0.00934)*8000., line=1
	oplot, plcoo, f12a, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')
	
	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f1a,f2a]) & print, 'del Rhon,Pn max: ',bmax
        bmin=min([f1a,f2a]) & print, 'del Rhon,Pn min: ',bmin
;        bmax=max([f12a]) & print, 'del P max: ',bmax
;        bmin=min([f12a]) & print, 'del P min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f1a, title=' Del Rhon,pn', $
;        plot, plcoo, f12a, title=' Del P', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f2a, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f3-f3a,f13-f13a,f23-f23a]) & print, 'Del Tp, Tn max: ',bmax
        bmin=min([f3-f3a,f13-f13a,f23-f23a]) & print, 'Del Tp, Tn min: ',bmin
;        bmax=max([f3-f3a,f13-f13a]) & print, 'Del Tp, Tn max: ',bmax
;        bmin=min([f3-f3a,f13-f13a]) & print, 'Del Tp, Tn min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f3-f3a,title='Del Tp, Tn',xtitle=xtit, $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f13-f13a, line=1
	oplot, plcoo, f23-f23a, line=2
;	oplot, plcoo, f18, line=1
;	oplot, plcoo, f19, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

;	!P.POSITION=[xa,ylo3,xe,yup3]
;        bmax=max([f21]) & print, 'Tn-Tp max: ',bmax
;        bmin=min([f21]) & print, 'Tn-Tp min: ',bmin
;        delb=bmax-bmin
;        if bmin eq bmax then bmax=bmin+1.0
;        delb=bmax-bmin
;        xpos=pmax+0.015*(pmax-pmin)
;        ypos0=bmin+delb/8.  
;        ypos1=bmin+delb/24. 
;        ypos2=bmin+delb/72. 
;        ypos3=bmin+delb/216. 
;        ypos4=bmin+delb/648. 
;	plot, plcoo, f21,title='Tn-Tp',xtitle=xtit, $
;	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	xyouts,xpos,ypos0,'time:'
;	xyouts,xpos,ypos1,' '+string(time,'(f6.3)')
;	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif

print, 'plot page 5, jy,ey,res'

;-----------------------------------------------------------------------
; plot  page 5 ( J, E, )
;-----------------------------------------------------------------------

  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f01]) & print, 'J max: ',bmax
        bmin=min([f01]) & print, 'Jx min: ',bmin
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f01, title=' Jx (uA/m^2) ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos4,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f03]) & print, 'J max: ',bmax
        bmin=min([f03]) & print, 'Jy min: ',bmin
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f03, title=' Jy (uA/m^2) ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos4,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f04]) & print, 'Ohm max: ',bmax
        bmin=min([f04]) & print, 'Ohm min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f04,title=' Ex (uV/m)', $
;	plot, plcoo, f22, title=' Res*J^2/Rho ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	oplot, plcoo, f05, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(time*ta_time,'(f6.3)')+ ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

;	!P.POSITION=[xa,ylo3,xe,yup3]
;        bmax=max([f7]) & print, 'Res max: ',bmax
;        bmin=min([f7]) & print, 'Res min: ',bmin
;        delb=bmax-bmin
;        if bmin eq bmax then bmax=bmin+1.0
;        delb=bmax-bmin
;        xpos=pmax+0.015*(pmax-pmin)
;;        bmax=10000.0 & bmin=1850.0
;        ypos0=bmin+delb/8.  
;        ypos1=bmin+delb/16. 
;        ypos2=bmin+delb/32. 
;        ypos3=bmin+delb/64. 
;        ypos4=bmin+delb/128. 
;	plot_io, plcoo, f7,title='Res (Ohm m)', $
;	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	xyouts,xpos,ypos0,'time:'
;	xyouts,xpos,ypos1,' '+string(time,'(f6.3)')
;	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')


  endif
print, 'plot page 6, rho,ey,ve'

;-----------------------------------------------------------------------
; plot  page 6 ( Rho, E, Ve, )
;-----------------------------------------------------------------------

  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f11]) & print, 'Rho max: ',bmax
        bmin=min([f11]) & print, 'Rho min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f11,title=' Rho (1/cm^3) ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, f12, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([f04]) & print, 'E max: ',bmax
        bmin=min([f04]) & print, 'E min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
;	plot, plcoo, f04,title=' E (uV/m) ', $
	plot, plcoo, f04,title=' Ex (uV/m) ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	oplot, plcoo, f05, line=1
;	oplot, plcoo, f06, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([f14a]) & print, 'Ve max: ',bmax
        bmin=min([f14a]) & print, 'Ve min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
;	plot, plcoo, f14a,title=' Ve Components (Km/s)', $
	plot, plcoo, f14a,title=' Vex Components (Km/s)', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	oplot, plcoo, f15a, line=1
;	oplot, plcoo, f16a, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

  endif
  
print, 'plot page 7, gradp, Ve*He, Pi & Pe'

;-----------------------------------------------------------------------
; plot  page 7 ( gradP, ve*he , Pi & Pe)
;-----------------------------------------------------------------------

  read, contin
  if (contin eq '' or contin eq 'y') then begin

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([f20]) & print, 'gradP max: ',bmax
;        bmax=5.
        bmin=min([f20]) & print, 'gradP min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, f20,title=' gradP ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
;	oplot, plcoo, f12, line=1
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([fvtex,fvtey,fvtez]) & print, 'Ve*He max: ',bmax
        bmin=min([fvtex,fvtey,fvtez]) & print, 'Vey*He min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
;	plot, plcoo, f04,title=' JxB ', $
	plot, plcoo, fvtex,title=' Ve*He ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, fvtey, line=1
	oplot, plcoo, fvtez, line=2
	xyouts,xpos,ypos0,'time:'
	xyouts,xpos,ypos1,' '+string(ta_time*time,'(f6.3)') + ' s'
	xyouts,xpos,ypos3,plane+' ='+string(coord(igrid),'(f6.2)')

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([fpi,fpe]) & print, 'Pi, Pe max: ',bmax
        bmin=min([fpi,fpe]) & print, 'Pi, Pe min: ',bmin
;        bmax=max([fpe/rho]) & print, 'Pi, Pe max: ',bmax
;        bmin=min([fpe/rho]) & print, 'Pi, Pe min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        xpos=pmax+0.015*(pmax-pmin)
        ypos0=bmin+0.9*delb    
        ypos1=bmin+0.82*delb 
        ypos2=bmin+0.64*delb 
        ypos3=bmin+0.56*delb 
        ypos4=bmin+0.3*delb 
	plot, plcoo, fpi,title=' Pi & Pe ', $
	yrange=[bmin,bmax],xstyle=1,ystyle=1
	oplot, plcoo, fpe, line=1
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


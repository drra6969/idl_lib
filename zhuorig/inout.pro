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
  xmin =  -1. & ymin = 0.
  xmax =   1. & ymax = 1200.27958
;--------------------
   time=0.0 & fnumber='1'
   name='' & contin='' & again='y' & withps='n' & run=''
   nx=long(303) & ny=long(303) & nyy=long(303)

   ntmax=40
   ix0=37
   tarray = fltarr(ntmax)


; READ INPUT DATA OF DIMENSION NX, NY
   openr, 8, 'magtap01',/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

          f1=fltarr(ny,ntmax)
          f2=f1 & f1a=f1 & f2a=f1 
          f3=f1 & f4=f1 & f5=f1 & f6=f1 & f7=f1 & f8=f1 & f9=f1 & f10=f1
          f11=f1 & f11a=f1 & f12=f1 & f12a=f1 & f13=f1 & f13a=f1
          f14=f1 & f15=f1 & f16=f1 & f17=f1 & f18=f1 & f19=f1 & f20=f1 & f21=f1
	  f01=f1 & f02=f1 
   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & 
   vx=bx & vy=bx & vz=bx & vnx=bx & vny=bx & vnz=bx
   nuei=bx & nuen=bx & nuin=bx 
   vex=bx & vey=bx & vez=bx   & te=bx  & lengthy0=0.0
   rho=bx & rho1=bx & u=bx & u1=bx
   pe=bx & gradpe=bx & gradpex=bx

   rhono=bx & rhono2=bx & rhonn2=bx 
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   res=bx & nu12s=bx 
   p=bx & pn=bx & p1=bx & pn1=bx & jx=bx & jy=bx & jz=bx
   ex=bx & ey=bx & ez=bx 
   w1=bx & w2=bx & w3=bx & w4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin
   readu, 8,  jx,jy,jz
   close, 8

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
   print,'Dimension and time: ',ny,ta_time*tarray
   print,'vertical grid:',l_units*y
   print,'IX0, and X(IX0)', ix0, x(ix0)

   lambdam = ta_time*(30.0/5.64)*sqrt(1.0/16.0/1836.2/dens0)

   for it=0,ntmax-1 do begin

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
;   read, fnumber	
	fnumber=string(it+1,form='(i2.2)')
;   name='magtap'+fnumber
	name='magtap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 & dx=g1
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 & dy=h1
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & 
   vx=bx & vy=bx & vz=bx & vnx=bx & vny=bx & vnz=bx
   vex=bx & vey=bx & vez=bx   & te=bx  & lengthy0=0.0
   rho=bx & rho1=bx & u=bx & u1=bx
   pe=bx & gradpe=bx & gradpex=bx & fpe=bx & ex_dpe=bx & ey_dpe=bx
   rhono=bx & rhono2=bx & rhonn2=bx 
   rhon=bx & un=bx & rhon1=bx & un1=bx 
   res=bx & nu12s=bx 
   p=bx & pn=bx & p1=bx & pn1=bx & jx=bx & jy=bx & jz=bx
   ex=bx & ey=bx & ez=bx 
   w1=bx & w2=bx & w3=bx & w4=bx 

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin
   readu, 8,  jx,jy,jz
   close, 8
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
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhon1,un1,nu12s
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

      fpe=shift(pe,0,-1)-shift(pe,0,1)
      for i=1,ny-2 do gradpe(*,i)=dy(i)*fpe(*,i)
      for i=0,nx-1 do gradpe(i,0)=0.00001 
      for i=0,nx-1 do gradpe(i,ny-1)=0.00001
      fpe=shift(pe,-1,0)-shift(pe,1,0)
      for i=1,nx-2 do gradpex(i,*)=dx(i)*fpe(i,*)
      for i=0,ny-1 do gradpex(0,i)=0.00001
      for i=0,ny-1 do gradpex(nx-1,i)=0.00001
      
      ex_dpe = -t_units/e_units*1.38/0.016/rho*gradpex
      ey_dpe = -t_units/e_units*1.38/0.016/rho*gradpe

;      ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx+ex_dpe
;      ey=nuen*vy*lambdam-vez*bx+vex*bz+res*jy+ey_dpe
;      ez=nuen*vz*lambdam-vex*by+vey*bx+res*jz
      ex=nuen*vx*lambdam-vey*bz+vez*by+res*jx+ex_dpe
      ey=nuen*vy*lambdam+res*jy
      ez=nuen*vz*lambdam-vex*by+vey*bx+res*jz

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
  
    xa=0.1 & xe=0.8 & dpy=0.24
    ylo1=0.65 & yup1=ylo1+dpy
    ylo2=0.35 & yup2=ylo2+dpy
    ylo3=0.05 & yup3=ylo3+dpy
   
   ytit='z'
  
;  print, 'What unit?'
;  print, 'Options: s: plot in simulation unit'
;  print, '         p: plot in physical unit'
;  print, '    return -> no changes applied'
;  print, 'Present Choice: ', unit
;  read, whatunit
;  if (whatunit eq 's') or (whatunit eq 'p') then unit=whatunit
;  print, 'current unit= ', unit
     
;    print, 'Which case?'
;    read, run


plane='x'
igrid=ix0
        
          plcoo=y(0:ny-1) 
          tarray(it) = time 
	  f01(*,it)=jy(igrid,*) & f02(*,it)=ey(igrid,*)
          f1(*,it)=rhon(igrid,*) & f2(*,it)=pn(igrid,*) 
          f3(*,it)=pn(igrid,*)/rhont(igrid,*)
          f1a(*,it)=rhon(igrid,*)-rhon1(igrid,*) 
          f2a(*,it)=pn(igrid,*)-pn1(igrid,*) 
          f4(*,it)=vnx(igrid,*) & f5(*,it)=vny(igrid,*)
          f6(*,it)=vnz(igrid,*)
          f7(*,it)=res(igrid,*) & f8(*,it)=nu12s(igrid,*) 
          f11(*,it)=rho(igrid,*) & f11a(*,it)=rho(igrid,*)-rho1(igrid,*)
          f12(*,it)=p(igrid,*) & f12a(*,it)=p(igrid,*)-p1(igrid,*)
          f13(*,it)=te(igrid,*)
          f13a(*,it)=p1(igrid,*)/rho1(igrid,*)
          f14(*,it)=vx(igrid,*) & f15(*,it)=vy(igrid,*) & f16(*,it)=vz(igrid,*)
          f17(*,it)=bx(igrid,*) & f18(*,it)=by(igrid,*) & f19(*,it)=bz(igrid,*)
          f20(*,it)=gradp(igrid,*)
          f21(*,it)=pn(igrid,*)/rhont(igrid,*)-p(igrid,*)/rho(igrid,*)

     endfor

   larr = l_units*y
   tarr = ta_time*tarray
   earr = e_units*f02
   rhoarr = n_units*f11
   temparr = t_units*f13

 save,file='/why1/zhu/io2d/ray/curr_down.dat', $
           /xdr,ny,larr,tarr, earr, rhoarr, temparr
;save,file='/why1/zhu/io2d/ray/currion2_down.dat',/xdr,ny,larr,tarr, rhoarr, temparr
end


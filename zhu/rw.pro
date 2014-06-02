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

;----PARAMETER-------
  xmin = 3. & ymin =200.
  xmax = 9. & ymax = 800
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
   ex=bx & ey=bx & ez=bx & jx=bx & jy=bx & jz=bx
   sigmap=bx & sigmah=bx & s0=bx & jb=bx
   rho=bx & rho1=bx & u=bx & u1=bx & te=bx
   rhono=bx & rhono2=bx & rhonn2=bx 
   nuei=bx & nuen=bx & nuin=bx

   bsq=bx & p=bx & p1=bx
   f1=bx & f2=bx & f3=bx & f4=bx
   a=fltarr(nx,ny) 
   vnx=bx & vny=bx & vnz=bx
   rhon=bx & rhona=bx & rhont=bx & meff=bx & mo=bx
   un=bx & una=bx 
   res=bx & nu12s=bx & pn=bx & pna=bx  
   pe=bx & gpx=bx & pp=bx
   byx=bx & bxy=bx 
   
   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  bx,by,bz
   readu, 8,  vx,vy,vz,vex,vey,vez
   readu, 8,  rho,rho1,u,u1,te,res
   readu, 8,  rescal,b0,length0,lengthy0,dens0,h0
   readu, 8,  nuei,nuen,nuin
   readu, 8,  jx,jy,jz
   close, 8   
name='magnap'+fnumber
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nyy,time
   print, 'dimension nx=',nx,'     ny=',ny,'     nyy=',nyy

   readu, 8,  x,dx,g2,g3,g3,g3,g3, y,dy,h2,h3,h3,h3,h3
   readu, 8,  vnx,vny,vnz
   readu, 8,  rhono,rhono2,rhonn2,rhon,un,rhona,una,nu12s
   close, 8



save,  filename = 'f2d'+fnumber, nx,ny,nyy,time, x,dx,g2,y,dy,h2, bx,by,bz, vx,vy,vz,vex,vey,vez, rho,rho1,u,u1,te,res, rescal,b0,length0,lengthy0,dens0,h0, nuei,nuen,nuin, jx,jy,jz, vnx,vny,vnz, rhono,rhono2,rhonn2,rhon,un,rhona,una,nu12s



end


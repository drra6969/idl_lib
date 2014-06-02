PRO read2,tim
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  nnorm,bnorm,vnorm,pnorm,lnorm,tnorm, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  xs,ys,zs

; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
  pbd = fltarr(2) & rhobd = pbd & vxbd = pbd & vybd = pbd & vzbd = pbd
  bxbd = pbd & bybd = pbd & bzbd = pbd
  zeit = 0.0 
  
   tim=0.0 & fnumber=1
   nx=long(503) & ny=long(503)
   nxf = 101   &   nyf = 101
;----PARAMETER-------
  xmin = -20. & ymin = -20.0
  xmax =  20. & ymax =  20.0

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+string(fnumber,'(i2.2)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,tim
   print, 'dimension nx=',nx,'     ny=',ny

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)
   g1=fltarr(nx,/NOZERO) & g2=g1 & g3=g1 
   h1=fltarr(ny,/NOZERO) & h2=h1 & h3=h1 
   bx=fltarr(nx,ny,/NOZERO) & by=bx & bz=bx & vx=bx & vy=bx & vz=bx
   rho=bx & u=bx & res=bx & p=bx 
   nyf = ny-2
   
   readu, 8,  x,g1,g2,g3,g3,g3,g3, y,h1,h2,h3,h3,h3,h3
  print, 'after read x'
   if (xmin lt x(1)) then begin
     print, 'warning! xmin:',xmin,' out of bounds:',x(1),' Reset!'
     xmin=x(1)
   endif  
   if (xmax gt x(nx-2)) then begin
     print, 'warning! xmax:',xmax,' out of bounds:',x(nx-2),' Reset!'
     xmax=x(nx-2)
   endif  
;  print, y
   ymin=y(1) & ymax=y(ny-2) & nyf=ny-2 & ioxf=fltarr(nxf) & ioyf=fltarr(nyf)
   f1=fltarr(nxf,nyf) & f2=f1 & f3=f1
   readu, 8,  bx,by,bz
    print, 'after read b'
;    bx=-bx & by=-by
   readu, 8,  vx,vy,vz
    print, 'after read v'
;    vz=-vz
   readu, 8,  rho,u,res
    print, 'after read rho'
   close, 8
   vx=vx/rho &  vz=vz/rho & vy=vy/rho & p=2*u^(5.0/3.0)
   
   rhobd(0)=total(rho(1,*))/ny & rhobd(1)=total(rho(nx-2,*))/ny
   pbd(0)=total(p(1,*))/ny     & pbd(1)=total(p(nx-2,*))/ny
   bxbd(0)=total(bx(1,*))/ny   & bxbd(1)=total(bx(nx-2,*))/ny
   bybd(0)=total(by(1,*))/ny   & bybd(1)=total(by(nx-2,*))/ny
   bzbd(0)=total(bz(1,*))/ny   & bzbd(1)=total(bz(nx-2,*))/ny
   vxbd(0)=total(vx(1,*))/ny   & vxbd(1)=total(vx(nx-2,*))/ny
   vybd(0)=total(vy(1,*))/ny   & vybd(1)=total(vy(nx-2,*))/ny
   vzbd(0)=total(vz(1,*))/ny   & vzbd(1)=total(vz(nx-2,*))/ny
   vy=vy-vybd(0) & vybd(0)=0.0 & vybd(1)=vybd(1)-vybd(0)
   vz=vz-vzbd(0) & vzbd(0)=0.0 & vzbd(1)=vzbd(1)-vzbd(0)
   
   rho=reverse(rho,2) & p=reverse(p,2) 
   bx=reverse(bx,2) & by=reverse(by,2)  & bz=reverse(bz,2) 
   vx=reverse(vx,2) & vy=reverse(vy,2)  & vz=reverse(vz,2) 
   
; GRID FOR INTERPOLATION
   xf=findgen(nxf) & yf=findgen(nyf)
   dxf=(xmax-xmin)/float(nxf-1) & xf=xf*dxf+xmin
   dyf=(ymax-ymin)/float(nyf-1) & yf=yf*dyf+ymin
   in=-1 & k=0
   repeat begin
     in=in+1
     while xf(in) gt x(k+1) do k=k+1
     ioxf(in) = float(k) + (xf(in)-x(k))/(x(k+1)-x(k)) 
   endrep until in eq nxf-1
   in=-1 & k=0
   repeat begin
     in=in+1
     while yf(in) gt y(k+1) do k=k+1
     ioyf(in) = float(k) + (yf(in)-y(k))/(y(k+1)-y(k))        
   endrep until in eq nyf-1
        
   bx=interpolate(bx,ioxf,ioyf,/grid)
   by=interpolate(by,ioxf,ioyf,/grid)
   bz=interpolate(bz,ioxf,ioyf,/grid)
   vx=interpolate(vx,ioxf,ioyf,/grid)
   vy=interpolate(vy,ioxf,ioyf,/grid)
   vz=interpolate(vz,ioxf,ioyf,/grid)
   rho=interpolate(rho,ioxf,ioyf,/grid)
   p=interpolate(p,ioxf,ioyf,/grid)
   x=xf & y=yf
       
  nsat=nxf & itot=2*nyf-1 & startime=y(0)
  
  xsi=fltarr(nsat) & ysi=xsi & zsi=xsi 
  xi=xsi & yi=xsi & zi=xsi
  vxsi=xsi & vysi=xsi & vzsi=xsi
  xs=fltarr(nsat,itot) & ys=xs & zs=xs 
  bxs=xs & bys=xs & bzs=xs & vxs=xs & vys=xs & vzs=xs
  rhos=xs & ps=xs & bs=xs & ptots=xs
  time=fltarr(itot) 
  
  time(0:nyf-1)=y(0:nyf-1) & time(nyf:2*nyf-2)=y(nyf-1)-y(0)+y(1:nyf-1)
;  print,time
  bxs(*,0:nyf-1)=bx(*,0:nyf-1) & bxs(*,nyf:2*nyf-2)=bx(*,1:nyf-1)
  bys(*,0:nyf-1)=by(*,0:nyf-1) & bys(*,nyf:2*nyf-2)=by(*,1:nyf-1)
  bzs(*,0:nyf-1)=bz(*,0:nyf-1) & bzs(*,nyf:2*nyf-2)=bz(*,1:nyf-1)
  vxs(*,0:nyf-1)=vx(*,0:nyf-1) & vxs(*,nyf:2*nyf-2)=vx(*,1:nyf-1)
  vys(*,0:nyf-1)=vy(*,0:nyf-1) & vys(*,nyf:2*nyf-2)=vy(*,1:nyf-1)
  vzs(*,0:nyf-1)=vz(*,0:nyf-1) & vzs(*,nyf:2*nyf-2)=vz(*,1:nyf-1)
  rhos(*,0:nyf-1)=rho(*,0:nyf-1) & rhos(*,nyf:2*nyf-2)=rho(*,1:nyf-1)
  ps(*,0:nyf-1)=p(*,0:nyf-1) & ps(*,nyf:2*nyf-2)=p(*,1:nyf-1)
  bs=sqrt(bxs*bxs+bys*bys+bzs*bzs)
  ptots=ps+bs^2
  
   xsi=x & ysi(0:nsat-1)=y(0) & zsi(0:nsat-1)=0.0
   nnorm = 11.0                    ; for cm^(-3)
   bnorm = 16.0                    ; for nT
   lnorm = 600.0                   ; for km
   vnorm = 21.8*bnorm/sqrt(nnorm)  ; for km/s
   pnorm = 0.01*bnorm^2.0/8.0/pi   ; for nPa
   tnorm = lnorm/vnorm             ; for s

return
end

  

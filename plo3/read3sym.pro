PRO read3sym, dx,dxh,dy,dyh,dz,dzh, bx,by,bz,sx,sy,sz,rho,u,res

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time

  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  name='magtap'+string(fnumber,'(i2.2)')
;  name='magbin'+string(fnumber,'(i2.2)')
  openr, 8, name,/F77_UNFORMATTED
  readu, 8, nx, ny, nz, time
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    print, 'time=',time
    x=fltarr(nx,/NOZERO) & dx=x & dxh=x
    y=fltarr(ny,/NOZERO) & dy=y & dyh=y
    nztot=2*nz-3       &      nzl=nz-1
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx 

  readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin:in=', z(1), '  zmax:in=', z(nz-2)

    zup=z(1:nzl) & zlo=-z(2:nzl) & zlo=reverse(zlo)
    z=fltarr(nztot,/NOZERO) 
    z(0:nzl-2)=zlo(*) & z(nzl-1:nztot-1)=zup(*)
    print, z

    zup=dz(1:nzl) & zlo=dz(2:nzl) & zlo=reverse(zlo)
    dz=z
    dz(0:nzl-2)=zlo(*) & dz(nzl-1:nztot-1)=zup(*)
    print, dz


  print, 'after read x'
;  print, y
   readu, 8,  bx,by,bz
  print, 'after read b'
   readu, 8,  sx,sy,sz
  print, 'after read v'
   readu, 8,  rho,u,res
  print, 'after read rho'
   close, 8

  ttt=fltarr(ny,nzl-1,/NOZERO)
  fup=bx(*,*,1:nzl) & flo=-bx(*,*,2:nzl) & print, size(fup) & print,size(flo)
;  print,flo(80,*,0)
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt &  endfor
;  print,'revers:',flo(80,*,nzl-2)
  bx=fltarr(nx,ny,nztot,/NOZERO)
  bx(*,*,0:nzl-2)=flo   &  bx(*,*,nzl-1:nztot-1)=fup

  fup=by(*,*,1:nzl) & flo=by(*,*,2:nzl) 
;  print,'by:',flo(80,*,0)
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  by=bx & by(*,*,0:nzl-2)=flo   &  by(*,*,nzl-1:nztot-1)=fup
;  print,'revers:',flo(80,*,nzl-2)

  fup=bz(*,*,1:nzl) & flo=bz(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  bz=bx & bz(*,*,0:nzl-2)=flo   &  bz(*,*,nzl-1:nztot-1)=fup

  fup=sx(*,*,1:nzl) & flo=sx(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  sx=bx & sx(*,*,0:nzl-2)=flo   &  sx(*,*,nzl-1:nztot-1)=fup

  fup=sy(*,*,1:nzl) & flo=-sy(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  sy=bx & sy(*,*,0:nzl-2)=flo   &  sy(*,*,nzl-1:nztot-1)=fup

  fup=sz(*,*,1:nzl) & flo=-sz(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  sz=bx & sz(*,*,0:nzl-2)=flo   &  sz(*,*,nzl-1:nztot-1)=fup

  fup=rho(*,*,1:nzl) & flo=rho(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  rho=bx & rho(*,*,0:nzl-2)=flo   &  rho(*,*,nzl-1:nztot-1)=fup

  fup=u(*,*,1:nzl) & flo=u(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  u=bx & u(*,*,0:nzl-2)=flo   &  u(*,*,nzl-1:nztot-1)=fup

  fup=res(*,*,1:nzl) & flo=res(*,*,2:nzl) 
  for i = 0, nx-1 do begin
   ttt(*,*)=flo(i,*,*) & ttt=rotate(ttt,2) & flo(i,*,*)=ttt & endfor
  res=bx & res(*,*,0:nzl-2)=flo   &  res(*,*,nzl-1:nztot-1)=fup

  nz=nztot

return
end


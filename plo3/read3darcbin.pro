PRO read3darcbin, dx,dxh,dy,dyh,dz,dzh, bx,by,bz,sx,sy,sz,rho,u,res

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
  nzz=long(126) 
  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  nome='pagtap'+string(fnumber,'(i1.1)')
  print, '1',nome
;  nome='magbin'+string(fnumber,'(i1.1)')
  openr, 8, nome,/F77_UNFORMATTED
  RESTORE, 'temp1'
  print, '2',nome
  temp1=binary_template(nome,template=temp1)
  print, '3',nome
  nome_BINARY=READ_BINARY(nome, TEMPLATE=temp1)
   nx=nome_BINARY.nx
   ny=nome_BINARY.ny
   nz=nome_BINARY.nz
   time=nome_BINARY.time
   nzz=nome_BINARY.nzz
  print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
  print, 'time=',time
   restore, 'temp3'
   print, '4a',nome
   temp3=binary_template(nome,template=temp3)
   print, '4',nome
   nome_BINARY=READ_BINARY(nome, TEMPLATE=temp3)
   print, '5',nome
    print,'nx',nx,'ny',ny
    xvec=fltarr(nx,/NOZERO) & dxvec=xvec 
    yvec=fltarr(ny,/NOZERO) & dyvec=yvec
    x=fltarr(nx,/NOZERO) & dx=x & dxh=xvec
    y=fltarr(ny,/NOZERO) & dy=y & dyh=yvec
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z  & u1=z & u2=z & u3=z
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx 

   xvec=nome_BINARY.xvec
   dxvec=nome_BINARY.dxvec
   dxh=nome_BINARY.dxh
   yvec=nome_BINARY.yvec
   dyvec=nome_BINARY.dyvec
   dyh=nome_BINARY.dyh
   z=nome_BINARY.z
   dz=nome_BINARY.dz
   dzh=nome_BINARY.dzh
  print, 'after read x'
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    y=yvec(0:ny-1) & dy=dyvec(0:ny-1)
;    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
   bx=nome_BINARY.bx
   by=nome_BINARY.by
   bz=nome_BINARY.bz
  print, 'after read b'
   sx=nome_BINARY.sx
   sy=nome_BINARY.sy
   sz=nome_BINARY.sz 
  print, 'after read v'
   rho=nome_BINARY.rho
   u=nome_BINARY.u
   res=nome_BINARY.res
   prof=nome_BINARY.prof
  print, 'after read rho'
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    y=yvec(0:ny-1) & dy=dyvec(0:ny-1)

;    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
   close, 8

return
end


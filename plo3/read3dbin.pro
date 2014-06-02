PRO read3dbin, dx,dxh,dy,dyh,dz,dzh, bx,by,bz,sx,sy,sz,rho,u,res

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
  nzz=long(126) 
  print, 'Input filenumber'
  read, fnumber
  print, 'Which case?'
  read, run
  name='pagtap'+string(fnumber,'(i1.1)')
;  name='magbin'+string(fnumber,'(i1.1)')
  openr, 8, name,/F77_UNFORMATTED
  RESTORE, 'temp1'
  temp1=binary_template(name,template=temp1)
  name_BINARY=READ_BINARY(name, TEMPLATE=temp1)
   nx=name_BINARY.nx
   ny=name_BINARY.ny
   nz=name_BINARY.nz
   time=name_BINARY.time
   nzz=name_BINARY.nzz
  print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
  print, 'time=',time
   restore, 'temp2'
   temp2=binary_template(name,template=temp2)
   name_BINARY=READ_BINARY(name, TEMPLATE=temp2)
    nv=nx*ny  & xvec=fltarr(nv,/NOZERO) & dxvec=xvec & yvec=xvec & dyvec=xvec
    x=fltarr(nx,/NOZERO) & dx=x & dxh=xvec
    y=fltarr(ny,/NOZERO) & dy=y & dyh=yvec
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z  & u1=z & u2=z & u3=z
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx 

   xvec=name_BINARY.xvec
   dxvec=name_BINARY.dxvec
   dxh=name_BINARY.dxh
   yvec=name_BINARY.yvec
   dyvec=name_BINARY.dyvec
   dyh=name_BINARY.dyh
   z=name_BINARY.z
   dz=name_BINARY.dz
   dzh=name_BINARY.dzh
  print, 'after read x'
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
   bx=name_BINARY.bx
   by=name_BINARY.by
   bz=name_BINARY.bz
  print, 'after read b'
   sx=name_BINARY.sx
   sy=name_BINARY.sy
   sz=name_BINARY.sz 
  print, 'after read v'
   rho=name_BINARY.rho
   u=name_BINARY.u
   res=name_BINARY.res
   prof=name_BINARY.prof
  print, 'after read rho'
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)
   close, 8

return
end


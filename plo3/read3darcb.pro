PRO read3darcb, dx,dxh,dy,dyh,dz,dzh, bx,by,bz,sx,sy,sz,rho,u,res

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
  openr, 8, name,/F77_UNFORMATTED,/swap_endian
  readu, 8, nx, ny, nz, time,nzz
    print, 'dimension nx=',nx,'   ny=',ny,'   nz=',nz
    print, 'time=',time
    nv=nx*ny  & xvec=fltarr(nv,/NOZERO) & dxvec=xvec & yvec=xvec & dyvec=xvec
    x=fltarr(nx,/NOZERO) & dx=x & dxh=xvec
    y=fltarr(ny,/NOZERO) & dy=y & dyh=yvec
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z  & u1=z & u2=z & u3=z
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx 

  readu, 8,  xvec,dxvec,dxh,dxh,dxh,dxh,dxh,yvec,dyvec,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    x=xvec(0:nx-1) & dx=dxvec(0:nx-1)
    for i=0,ny-1 do y(i)=yvec(i*nx)  &   for i=0,ny-1 do dy(i)=dyvec(i*nx) 
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)

  print, 'after read x'
;  print, y
   readu, 8,  bx,by,bz
  print, 'after read b'
   readu, 8,  sx,sy,sz
  print, 'after read v'
   readu, 8,  rho,u,res
  print, 'after read rho'
   close, 8

return
end


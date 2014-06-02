PRO read3dh, dx,dxh,dy,dyh,dz,dzh, bx,by,bz,sx,sy,sz,rho,u,res,jx,jy,jz

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
    z=fltarr(nz,/NOZERO) & dz=z & dzh=z 
    bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx 
    sx=bx & sy=bx & sz=bx & rho=bx & u=bx & res=bx
    jx=bx & jy=bx & jz=bx 

  readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh,y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)

  print, 'after read x'
;  print, y
   readu, 8,  bx,by,bz
  print, 'after read b'
   readu, 8,  sx,sy,sz
  print, 'after read v'
   readu, 8,  jx,jy,jz
  print, 'after read J'
   readu, 8,  rho,u,res
  print, 'after read rho'
   close, 8

return
end

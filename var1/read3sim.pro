PRO read3sim,tim
COMMON procommon, nsat,startime,itot,pi,ntmax, $
                  rhobd,pbd,vxbd,vybd,vzbd,bxbd,bybd,bzbd, $
                  xsi,ysi,zsi,vxsi,vysi,vzsi, $
                  time,bxs,bys,bzs,vxs,vys,vzs,rhos,ps,bs,ptots, $
                  jxs,jys,jzs,xs,ys,zs,cutalong
COMMON simdat3, nx,ny,nz,x,y,z,dx,dy,dz,bx,by,bz,vx,vy,vz,rho,u,res,p,$
                jx,jy,jz,xmin,xmax,ymin,ymax,zmin,zmax,linealong,icut

; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
   nx=long(503) & ny=long(503) & nz=long(503)
;----PARAMETER-------
  xmin = -20. & ymin = -20.0 & zmin = -20.0
  xmax =  20. & ymax =  20.0 & zmax =  20.0

; READ INPUT DATA OF DIMENSION NX, NY
   print, 'Input filenumber'
   read, fnumber
   name='magtap'+string(fnumber,'(i2.2)')
   openr, 8, name,/F77_UNFORMATTED
   readu, 8,  nx,ny,nz,tim
   print, 'dimension nx=',nx,'     ny=',ny,'     nz=',nz

   x=fltarr(nx,/NOZERO) & y=fltarr(ny,/NOZERO)& z=fltarr(nz,/NOZERO)
   dx=x & dxh=x & dy=y & dyh=y & dz=z & dzh=z 
   bx=fltarr(nx,ny,nz,/NOZERO) & by=bx & bz=bx & vx=bx & vy=bx & vz=bx
   rho=bx & u=bx & res=bx & p=bx 
   f1=bx & f2=by & f3=bz & f4=bz & jx=bx & jy=bx & jz=bx

   readu, 8,  x,dx,dxh,dxh,dxh,dxh,dxh, y,dy,dyh,dyh,dyh,dyh,dyh,$
             z,dz,dzh,dzh,dzh,dzh,dzh
   print, 'after read x'
   xmin=x(1) & xmax=x(nx-2) & ymin=y(1) & ymax=y(ny-2) 
   zmin=z(1) & zmax=z(nz-2)
;  print, y

    print, 'xmin=', xmin, '  xmax=', xmax
    print, 'ymin=', ymin, '  ymax=', ymax
    print, 'zmin=', zmin, '  zmax=', zmax
   
   readu, 8,  bx,by,bz
    print, 'after read b'
   readu, 8,  vx,vy,vz
    print, 'after read v'
   readu, 8,  rho,u,res
    print, 'after read rho'
   close, 8
   vx=vx/rho &  vz=vz/rho & vy=vy/rho & p=2*u^(5.0/3.0)

      f1 = shift(bz,0,-1,0)-shift(bz,0,1,0)
      for j=1,ny-2 do f3(*,j,*)=dy(j)*f1(*,j,*)
      f2 = shift(by,0,0,-1)-shift(by,0,0,1)
      for k=1,nz-2 do f4(*,*,k)=dz(k)*f2(*,*,k)
      jx = f3-f4
      f1 = shift(bx,0,0,-1)-shift(bx,0,0,1)
      for k=1,nz-2 do f3(*,*,k)=dz(k)*f1(*,*,k)
      f2 = shift(bz,-1,0,0)-shift(bz,1,0,0)
      for i=1,nx-2 do f4(i,*,*)=dx(i)*f2(i,*,*)
      jy = f3-f4
      f1 = shift(by,-1,0,0)-shift(by,1,0,0)
      for i=1,nx-2 do f3(i,*,*)=dx(i)*f1(i,*,*)
      f2 = shift(bx,0,-1,0)-shift(bx,0,1,0)
      for j=1,ny-2 do f4(*,j,*)=dy(j)*f2(*,j,*)
      jz = f3-f4
      jx([0,nx-1],*,*) = 0.
      jy([0,nx-1],*,*) = 0.
      jz([0,nx-1],*,*) = 0.


return
end

  

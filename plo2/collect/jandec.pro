PRO jandec, nx,ny,g1,h1,bx,by,bz,jx,jy,jz

;--- compute current density and electric field---
   f1=bx & f2=bx & f3=bx & f4=bx

   f1=shift(by,-1,0)-shift(by,1,0)
   f2=shift(bx,0,-1)-shift(bx,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   jz=f1-f2 & jz(0,*)=0. & jz(nx-1,*)=0. & jz(*,0)=0. & jz(*,ny-1)=0.

   f1 = shift(bz,0,-1)-shift(bz,0,1)
   for j=1,nx-2 do f3(j,*)=h1*f1(j,*)
   jx = f3
   f2 = shift(bz,-1,0)-shift(bz,1,0)
   for i=1,ny-2 do f4(*,i)=g1*f2(*,i)
   jy = -f4
   jx([0,nx-1],*)=0.0 & jy([0,nx-1],*)=0.0 
   jx(*,[0,ny-1])=0.0 & jy(*,[0,ny-1])=0.0 
  
return
end


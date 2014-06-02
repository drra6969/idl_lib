PRO jandeh1, nx,ny,g1,h1,bx,by,bz,vex,vey,res,jx,jy,jz,ez

;--- compute current density and electric field---
   f1=shift(by,-1,0)-shift(by,1,0)
   f2=shift(bx,0,-1)-shift(bx,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   jz=f1-f2 & jz(0,*)=jz(2,*) & jz(nx-1,*)=0. & jz(*,0)=0. & jz(*,ny-1)=0.
   ez=-(vex*by-vey*bx) + res*jz


   f1=shift(bz,0,-1)-shift(bz,0,1)
   for i=0,nx-1 do f1(i,*) = h1*f1(i,*)
   jx=f1 & jx(0,*)=-jx(2,*)   & jx(nx-1,*)=0.0  
           jx(*,0)=jx(*,ny-3) & jx(*,ny-1)=jx(*,2)

   f1=shift(bz,-1,0)-shift(bz,1,0)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   jy=-f1  & jy(0,*)=-jy(2,*)   & jy(nx-1,*)=0.0  
             jy(*,0)=jy(*,ny-3) & jy(*,ny-1)=jy(*,2)
   
  
return
end


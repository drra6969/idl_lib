PRO forces, nx,ny,g1,h1,bx,by,bz,jx,jy,jz,rho,p,fpx,fpy,fjx,fjy,fjz

;--- compute pressure grad forces---

   f1=shift(p,-1,0)-shift(p,1,0)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   fpx=-0.5*f1/rho  & fpx(0,*)=0.0  & fpx(nx-1,*)=0.0  
             fpx(*,0)=0.0  & fpx(*,ny-1)=0.0

   f1=shift(p,0,-1)-shift(p,0,1)
   for i=0,nx-1 do f1(i,*) = h1*f1(i,*)
   fpy=-0.5*f1/rho  & fpy(0,*)=0.0  & fpy(nx-1,*)=0.0  
             fpy(*,0)=0.0  & fpy(*,ny-1)=0.0

   fjx=jy*bz-jz*by  & fjy=jz*bx-jx*bz  & fjz=jx*by-jy*bx
   fjx=fjx/rho      & fjy=fjy/rho      & fjz=fjz/rho

   
  
return
end


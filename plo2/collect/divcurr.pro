PRO divcurr, nx,ny,g1,h1,jx,jy,divj

;--- compute divergence of current density---
   f1=shift(jx,-1,0)-shift(jx,1,0)
   f2=shift(jy,0,-1)-shift(jy,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   divj=(f1+f2) & divj(0,*)=divj(2,*) & divj(nx-1,*)=0. 
                      divj(*,0)=0.        & divj(*,ny-1)=0.
  
   
  
return
end

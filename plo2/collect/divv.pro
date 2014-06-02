PRO divv, nx,ny,g1,h1,rho,bx,by,vx,vy,vex,vey,vez,divi,dive,divni,divne,bdivvy

;--- compute current density and electric field---
   f1=shift(vx,-1,0)-shift(vx,1,0)
   f2=shift(vy,0,-1)-shift(vy,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   divi=(f1+f2) & divi(0,*)=divi(2,*) & divi(nx-1,*)=0. 
                      divi(*,0)=0.        & divi(*,ny-1)=0.
   
   f1=shift(vex,-1,0)-shift(vex,1,0)
   f2=shift(vey,0,-1)-shift(vey,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   dive=(f1+f2) & dive(0,*)=dive(2,*) & dive(nx-1,*)=0. 
                      dive(*,0)=0.        & dive(*,ny-1)=0.

   hx=rho*vx & hy=rho*vy
   f1=shift(hx,-1,0)-shift(hx,1,0)
   f2=shift(hy,0,-1)-shift(hy,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   divni=f1+f2 & divni(0,*)=divni(2,*) & divni(nx-1,*)=0. 
                 divni(*,0)=0.         & divni(*,ny-1)=0.
   
   hx=rho*vex & hy=rho*vey
   f1=shift(hx,-1,0)-shift(hx,1,0)
   f2=shift(hy,0,-1)-shift(hy,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   divne=f1+f2 & divne(0,*)=divne(2,*) & divne(nx-1,*)=0. 
                 divne(*,0)=0.         & divne(*,ny-1)=0.
  
   f1=shift(vez,-1,0)-shift(vez,1,0)
   f2=shift(vez,0,-1)-shift(vez,0,1)
   for j=0,ny-1 do f1(*,j) = g1*f1(*,j)
   for i=0,nx-1 do f2(i,*) = h1*f2(i,*)
   bdivvy=bx*f1+by*f2 & bdivvy(0,*)=bdivvy(2,*) & bdivvy(nx-1,*)=0. 
                      bdivvy(*,0)=0.        & bdivvy(*,ny-1)=0.

   
  
return
end


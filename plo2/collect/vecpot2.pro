PRO vecpot2, nx,ny,x,y,bx,by,a,fmin,fmax

;--- compute vectorpotential---
; reference row: k0 (in x)
; reference column: l0 (in y)
    k0=1  & l0=0
 
    a=0.0*bx
; 1. sweep along k (x) first positive then negative for fixed l0
    for k=k0, nx-3, 2 do $
        a(k+2,l0)=a(k,l0)-by(k+1,l0)*(x(k+2)-x(k))
    for k=k0, nx-2, 2 do $
        a(k+1,l0)=a(k,l0)-0.5*(by(k,l0)+by(k+1,l0))*(x(k+1)-x(k))

    for k=k0, 2, -2 do $
        a(k-2,l0)=a(k,l0)-by(k-1,l0)*(x(k-2)-x(k))
    for k=k0, 1, -2 do $
        a(k-1,l0)=a(k,l0)-0.5*(by(k,l0)+by(k-1,l0))*(x(k-1)-x(k))

; 2. sweep along l (y) first positive then negative for all k
    for k=0, nx-1 do begin
     for l=l0, ny-3, 2 do $
        a(k,l+2)=a(k,l)+bx(k,l+1)*(y(l+2)-y(l))  
     for l=l0, 2, -2 do $
        a(k,l-2)=a(k,l)+bx(k,l-1)*(y(l-2)-y(l))  & endfor
    for k=0, nx-1 do begin
     for l=l0, ny-2,2 do $
        a(k,l+1)=a(k,l)+0.5*(bx(k,l)+bx(k,l+1))*(y(l+1)-y(l))
     for l=l0, 1, -2 do $
        a(k,l-1)=a(k,l)+0.5*(bx(k,l)+bx(k,l-1))*(y(l-1)-y(l))  & endfor

    a1=a & a=0.0*bx

; 1. sweep along l (y) first positive then negative for k0
    for l=l0, ny-3, 2 do $
        a(k0,l+2)=a(k0,l)+bx(k0,l+1)*(y(l+2)-y(l))
    for l=l0, ny-2, 2 do $
        a(k0,l+1)=a(k0,l)+0.5*(bx(k0,l)+bx(k0,l+1))*(y(l+1)-y(l))

    for l=l0, 2, -2 do $
        a(k0,l-2)=a(k0,l)+bx(k0,l-1)*(y(l-2)-y(l))
    for l=l0, 1, -2 do $
        a(k0,l-1)=a(k0,l)+0.5*(bx(k0,l)+bx(k0,l-1))*(y(l-1)-y(l))

; 2. sweep along k (x) first positive then negative for all l

    for l=0, ny-1 do begin
     for k=k0, nx-3,2 do $
        a(k+2,l)=a(k,l)-by(k+1,l)*(x(k+2)-x(k))
     for k=k0, 2, -2 do $
        a(k-2,l)=a(k,l)-by(k-1,l)*(x(k-2)-x(k))  & endfor

    for l=0, ny-1 do begin
     for k=k0, nx-2, 2 do $
        a(k+1,l)=a(k,l)-(by(k+1,l)+by(k,l))*(x(k+1)-x(k))
     for k=k0, 1, -2 do $
        a(k-1,l)=a(k,l)-(by(k-1,l)+by(k,l))*(x(k-1)-x(k))  & endfor

    a=0.5*(a+a1)
    co00=a(1,0) & a=a-co00

;    a(*,0)=a(*,ny-3) & a(*,ny-1)=a(*,2) 
    fmax=max(a((nx-1)/2,2:ny-1))
    fmin=min(a((nx-1)/2,2:ny-1))

return
end


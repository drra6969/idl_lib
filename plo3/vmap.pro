PRO vmap, vavg,verr,x0,y0,z0 

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
COMMON    vmap_var, ex,ey,ez

;reference posotion
;k0=fix(nx/2) & l0=fix(ny/2) & m0=fix(nz/2)	;center

k0=fix(((x0-x(0))/(x(nx-1)-x(0)))*(nx-1))
l0=fix(((y0-y(0))/(y(ny-1)-y(0)))*(ny-1))
m0=fix(((z0-z(0))/(z(nz-1)-z(0)))*(nz-1))
;k0=110
;l0=82
;m0=82
print, 'k0=',k0,' l0=',l0,' m0=',m0

print,'creating potential fields...'

;v1 intigrates x -> y -> z
  v1=fltarr(nx,ny,nz)

; 1. sweep along k (x) first positive then negative for fixed l0,m0
  for k=k0, nx-3, 2 do $
    v1(k+2,l0,m0)=v1(k,l0,m0)-ex(k+1,l0,m0)*(x(k+2)-x(k))
  for k=k0, nx-2, 2 do $
    v1(k+1,l0,m0)=v1(k,l0,m0)-0.5*(ex(k,l0,m0)+ex(k+1,l0,m0))*(x(k+1)-x(k))
    
  for k=k0, 2, -2 do $
    v1(k-2,l0,m0)=v1(k,l0,m0)-ex(k-1,l0,m0)*(x(k-2)-x(k))
  for k=k0, 1, -2 do $
    v1(k-1,l0,m0)=v1(k,l0,m0)-0.5*(ex(k,l0,m0)+ex(k-1,l0,m0))*(x(k-1)-x(k))

; 2. sweep along l (y) first positive then negative for all k
  for k=0, nx-1 do begin
    for l=l0, ny-3, 2 do $
      v1(k,l+2,m0)=v1(k,l,m0)-ey(k,l+1,m0)*(y(l+2)-y(l))
    for l=l0, 2, -2 do $
      v1(k,l-2,m0)=v1(k,l,m0)-ey(k,l-1,m0)*(y(l-2)-y(l))  & endfor
  for k=0, nx-1 do begin
    for l=l0, ny-2, 2 do $
      v1(k,l+1,m0)=v1(k,l,m0)-0.5*(ey(k,l,m0)+ey(k,l+1,m0))*(y(l+1)-y(l))
    for l=l0, 1, -2 do $
      v1(k,l-1,m0)=v1(k,l,m0)-0.5*(ey(k,l,m0)+ey(k,l-1,m0))*(y(l-1)-y(l))  & endfor

; 3. sweep along m (z)  first positive then negative for all k and l
  for l=0, ny-1 do begin
    for k=0, nx-1 do begin
      for m=m0, nz-3, 2 do $
        v1(k,l,m+2)=v1(k,l,m)-ez(k,l,m+1)*(z(m+2)-z(m))
      for m=m0, 2, -2 do $
        v1(k,l,m-2)=v1(k,l,m)-ez(k,l,m-1)*(z(m-2)-z(m))  & endfor & endfor
  for l=0, ny-1 do begin
    for k=0, nx-1 do begin
      for m=m0, nz-2, 2 do $
        v1(k,l,m+1)=v1(k,l,m)-0.5*(ez(k,l,m)+ez(k,l,m+1))*(z(m+1)-z(m))
      for m=m0, 1, -2 do $
        v1(k,l,m-1)=v1(k,l,m)-0.5*(ez(k,l,m)+ez(k,l,m-1))*(z(m-1)-z(m))  & endfor & endfor


;v2 intigrates z -> x -> y
  v2=fltarr(nx,ny,nz)

; 1. sweep along m (z) first positive then negative for fixed l0,m0
  for m=m0, nz-3, 2 do $
    v2(k0,l0,m+2)=v2(k0,l0,m)-ez(k0,l0,m+1)*(z(m+2)-z(m))
  for m=m0, nz-2, 2 do $
    v2(k0,l0,m+1)=v2(k0,l0,m)-0.5*(ez(k0,l0,m)+ez(k0,l0,m+1))*(z(m+1)-z(m))
    
  for m=m0, 2, -2 do $
    v2(k0,l0,m-2)=v2(k0,l0,m)-ez(k0,l0,m-1)*(z(m-2)-z(m))
  for m=m0, 1, -2 do $
    v2(k0,l0,m-1)=v2(k0,l0,m)-0.5*(ez(k0,l0,m)+ez(k0,l0,m0-1))*(z(m-1)-z(m))

; 2. sweep along k (x) first positive then negative for all k
  for m=0, nz-1 do begin
    for k=k0, nx-3, 2 do $
      v2(k+2,l0,m)=v2(k,l0,m)-ex(k+1,l0,m)*(x(k+2)-x(k))
    for k=k0, 2, -2 do $
      v2(k-2,l0,m)=v2(k,l0,m)-ex(k-1,l0,m)*(x(k-2)-x(k))  & endfor
  for m=0, nz-1 do begin
    for k=k0, nx-2, 2 do $
      v2(k+1,l0,m)=v2(k,l0,m)-0.5*(ex(k,l0,m)+ex(k+1,l0,m))*(x(k+1)-x(k))
    for k=k0, 1, -2 do $
      v2(k-1,l0,m)=v2(k,l0,m)-0.5*(ex(k,l0,m)+ex(k-1,l0,m))*(x(k-1)-x(k))  & endfor

; 3. sweep along k (y)  first positive then negative for all l and m
  for k=0, nx-1 do begin
    for m=0, nz-1 do begin
      for l=l0, ny-3, 2 do $
        v2(k,l+2,m)=v2(k,l,m)-ey(k,l+1,m)*(y(l+2)-y(l))
      for l=l0, 2, -2 do $
        v2(k,l-2,m)=v2(k,l,m)-ey(k,l-1,m)*(y(l-2)-y(l))  & endfor & endfor
  for k=0, nx-1 do begin
    for m=0, nz-1 do begin
      for l=l0, ny-2, 2 do $
        v2(k,l+1,m)=v2(k,l,m)-0.5*(ey(k,l,m)+ey(k,l+1,m))*(y(l+1)-y(l))
      for l=l0, 1, -2 do $
        v2(k,l-1,m)=v2(k,l,m)-0.5*(ey(k,l,m)+ey(k,l-1,m))*(y(l-1)-y(l))  & endfor & endfor


;v3 intigrates y -> z -> x
  v3=fltarr(nx,ny,nz)

; 1. sweep along l (y) first positive then negative for fixed k0,m0
  for l=l0, ny-3, 2 do $
    v3(k0,l+2,m0)=v3(k0,l,m0)-ey(k0,l+1,m0)*(y(l+2)-y(l))
  for l=l0, ny-2, 2 do $
    v3(k0,l+1,m0)=v3(k0,l,m0)-0.5*(ey(k0,l,m0)+ey(k0,l+1,m0))*(y(l+1)-y(l))
    
  for l=l0, 2, -2 do $
    v3(k0,l-2,m0)=v3(k0,l,m0)-ey(k0,l-1,m0)*(y(l-2)-y(l))
  for l=l0, 1, -2 do $
    v3(k0,l-1,m0)=v3(k0,l,m0)-0.5*(ey(k0,l,m0)+ey(k0,l-1,m0))*(y(l-1)-y(l))

; 2. sweep along m (z) first positive then negative for all m
  for l=0, ny-1 do begin
    for m=m0, nz-3, 2 do $
      v3(k0,l,m+2)=v3(k0,l,m)-ez(k0,l,m+1)*(z(m+2)-z(m))
    for m=m0, 2, -2 do $
      v3(k0,l,m-2)=v3(k0,l,m)-ez(k0,l,m-1)*(z(m-2)-z(m))  & endfor
  for l=0, ny-1 do begin
    for m=m0, nz-2, 2 do $
      v3(k0,l,m+1)=v3(k0,l,m)-0.5*(ez(k0,l,m)+ez(k0,l,m+1))*(z(m+1)-z(m))
    for m=m0, 1, -2 do $
      v3(k0,l,m-1)=v3(k0,l,m)-0.5*(ez(k0,l,m)+ez(k0,l,m-1))*(z(m-1)-z(m))  & endfor

; 3. sweep along k (x)  first positive then negative for all l and m
  for m=0, nz-1 do begin
    for l=0, ny-1 do begin
      for k=k0, nx-3, 2 do $
        v3(k+2,l,m)=v3(k,l,m)-ex(k+1,l,m)*(x(k+2)-x(k))
      for k=k0, 2, -2 do $
        v3(k-2,l,m)=v3(k,l,m)-ex(k-1,l,m)*(x(k-2)-x(k))  & endfor & endfor
  for m=0, nz-1 do begin
    for l=0, ny-1 do begin
      for k=k0, nx-2, 2 do $
        v3(k+1,l,m)=v3(k,l,m)-0.5*(ex(k,l,m)+ex(k+1,l,m))*(x(k+1)-x(k))
      for k=k0, 1, -2 do $
        v3(k-1,l,m)=v3(k,l,m)-0.5*(ex(k,l,m)+ex(k-1,l,m))*(x(k-1)-x(k))  & endfor & endfor


print, 'v1',v1(k0,l0,m0)
print, 'v2',v2(k0,l0,m0)
print, 'v3',v3(k0,l0,m0)

vavg=(v1+v2+v3)/3
;vavg=v3
verr=(abs(v1-v2)+abs(v1-v3)+abs(v2-v3))/3
;vavg=abs(v1-v2)
;verr=abs(v1-v3)

return
end

;x0=fix(((x0-x(0))/(x(nx-1)-x(0)))*(nx-1)) & xf=fix(((xf-x(0))/(x(nx-1)-x(0)))*(nx-1))
;y0=fix(((y0-y(0))/(y(ny-1)-y(0)))*(ny-1)) & yf=fix(((yf-y(0))/(y(ny-1)-y(0)))*(ny-1))
;z0=fix(((z0-z(0))/(z(nz-1)-z(0)))*(nz-1)) & zf=fix(((zf-z(0))/(z(nz-1)-z(0)))*(nz-1))


PRO htcoor, n, vx,vy,vz, bx,by,bz, vht

  m=fltarr(3,3)
  fav=fltarr(3) 
  b2=bx*bx+by*by+bz*bz
  vb=vx*bx+vy*by+vz*bz
  m(0,0) = total(b2-bx*bx)  
  m(1,1) = total(b2-by*by)  
  m(2,2) = total(b2-bz*bz)
  m(1,0) = -total(bx*by) & m(2,0) = -total(bz*bx) &  m(1,2) = -total(by*bz)
  m(0,1) = m(1,0)  &  m(0,2) = m(2,0)  &  m(2,1) = m(1,2) 
  
  fav(0) = total(b2*vx-vb*bx)
  fav(1) = total(b2*vy-vb*by)
  fav(2) = total(b2*vz-vb*bz)
  
  m=m/n & fav=fav/n
  m0=invert(m, status0)
  vht=m0#fav
  if status0 ne 0 then print, 'status of matrix inversion:', status0
;  print, 'deHoffmann-Teller velocity components:', vht

return
end

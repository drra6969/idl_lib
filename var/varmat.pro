PRO varmat, n,  x,y,z, fav, m	   ;Vars corresp to vars used when called

; variance matrix m and average values fav for data sets x,y, and z
  m=fltarr(3,3)
  fav=fltarr(3) 
  fav(0) = total(x) & fav(1) = total(y) & fav(2) = total(z) 
  m(0,0) = total(x*x) & m(1,1) = total(y*y) &  m(2,2) = total(z*z)
  m(1,0) = total(x*y) & m(2,0) = total(z*x) &  m(1,2) = total(y*z)
  m(0,1) = m(1,0) & m(0,2) = m(2,0) &  m(2,1) =  m(1,2) 
  fav=fav/n  &  m=m/n
  m(0,0) = m(0,0) - fav(0)*fav(0)
  m(1,1) = m(1,1) - fav(1)*fav(1)
  m(2,2) = m(2,2) - fav(2)*fav(2)
  m(1,0) = m(1,0) - fav(1)*fav(0)
  m(2,0) = m(2,0) - fav(2)*fav(0)
  m(2,1) = m(2,1) - fav(2)*fav(1)
  m(0,1) = m(1,0)
  m(0,2) = m(2,0)
  m(1,2) = m(2,1)
;  print, 'Average values:'
;  print, fav
;  print, 'Variance matrix:'
;  print, m
  
return
end


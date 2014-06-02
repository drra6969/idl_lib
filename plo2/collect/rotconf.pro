PRO rotconf, by,bz,byrot,bzrot,phi

;--- rotation of y,z plane---
  pi = 3.14159265536 
  phir = phi*pi/180.0
  bzrot = by*sin(phir) + bz*cos(phir)
  byrot = by*cos(phir) - bz*sin(phir)
  
return
end


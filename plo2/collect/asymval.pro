PRO asymval, nx,ny,v,v1,v2,vav

;--- velocities on either side and average velocities---

    v1=0.0   & v2=0.0
    for iv=1, ny-1 do v1=v1+v(1,iv)
    for iv=1, ny-1 do v2=v2+v(nx-2,iv)
    v1=v1/(ny-1) & v2=v2/(ny-1) & vav=(v1+v2)/2.

  
return
end


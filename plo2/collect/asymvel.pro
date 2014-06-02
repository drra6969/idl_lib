PRO asymvel, nx,ny,vy,vy1,vy2,vyav

;--- velocities on either side and average velocities---

    vy1=0.0   & vy2=0.0
    for iv=1, ny-1 do vy1=vy1+vy(1,iv)
    for iv=1, ny-1 do vy2=vy2+vy(nx-2,iv)
    vy1=vy1/(ny-1) & vy2=vy2/(ny-1) & vyav=(vy1+vy2)/2.

  
return
end


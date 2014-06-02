PRO efield, unitype,vx,vy,vz,bx,by,bz,ex,ey,ez

  ex = vz*by - vy*bz
  ey = vx*bz - vz*bx
  ez = vy*bx - vx*by
  if unitype eq 'p' then begin
    c = 0.001             ; to obtain mV/m
    ex = c*ex
    ey = c*ey
    ez = c*ez
  endif

return
end


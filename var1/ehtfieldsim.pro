PRO ehtfieldsim, vht,bx,by,bz,ehx,ehy,ehz,withunits

  ehx = vht(2)*by - vht(1)*bz
  ehy = vht(0)*bz - vht(2)*bx
  ehz = vht(1)*bx - vht(0)*by
  
  if withunits eq 'y' then begin
    c = 0.001             ; to obtain mV/m
    ehx=c*ehx & ehy=c*ehy & ehz=c*ehz &
  endif

return
end


PRO timeprop, nn, time, datestrn, strnn,nint,itstart,itend

  itstart=lonarr(20) & itend=itstart

  CDF_EPOCH, time(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  datestrn = string(yr,'(i4.4)')+'.'+string(mo,'(i2.2)')+'.'+string(dy,'(i2.2)')
  strnn = 'Cluster '+datestrn
  print, strnn
  
; SUBTRACT days, months and years
  for k=0,nn-1 do begin
    CDF_EPOCH, time(k), yr, mo, dy, hr, mn, sc, milli, /BREAK
    time(k)=60.*hr+mn+sc/60.
  endfor

  
; DETERMINE RELATIVELY continuous intervals

  i=0 & j=0
  while i lt nn-1 do begin
    itstart(j)=i
    while (time(i+1)-time(i) lt 2.5) and (i lt nn-2) do i=i+1
    itend(j)=i & j=j+1
    if (itend(j-1)-itstart(j-1) lt 8) then j=j-1
    i=i+1
  endwhile
  nint=j
    
  return
end

PRO hclint, n,time,nint,itstart,itend

  itstart=lonarr(200) & itend=itstart

; SUBTRACT days, months and years
  for k=0,n-1 do begin
    CDF_EPOCH, time(k), yr, mo, dy, hr, mn, sc, milli, /BREAK
    time(k)=60.*hr+mn+sc/60.
  endfor

; DETERMINE RELATIVELY continuous intervals
  critint = 17./60.
  i=0 & j=0  
  while i lt n-1 do begin
    itstart(j)=i
    while (time(i+1)-time(i) lt critint) and (i lt n-2) do i=i+1
    itend(j)=i & j=j+1
    if (itend(j-1)-itstart(j-1) lt 4) then j=j-1
    i=i+1
  endwhile
  nint=j
    
  return
end

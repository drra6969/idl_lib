  pro draw, xli, yli, zli, style

; subroutine to draw a field line which is dashed for x<0
  s=size(xli)
  n=s(2)
  ndraw=0
;  print, 'size=', s, '  n=', n, '  style=', style
  if s(0) gt 1 then begin
  for i=0,n-2 do begin 
   if xli(i) lt 0.0 then $
   plots,xli(i:i+1),yli(i:i+1),zli(i:i+1),linestyle=style,/t3d,/data
   if xli(i) ge 0.0 then begin
     if ndraw ge 3 then $
     plots,xli(i:i+1),yli(i:i+1),zli(i:i+1),linestyle=style,/t3d,/data
     ndraw=ndraw+1
     if ndraw eq 6 then ndraw=0
   endif
  endfor
  endif
end



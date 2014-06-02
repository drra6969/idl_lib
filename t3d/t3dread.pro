  i=8L
  f=fltarr(6)
  openr, 8, 'WSbin',/F77_UNFORMATTED

  for k=0, 7 do begin
    readu, 8, i,f
    print, i,f
  endfor
  
  close,8
  
  end

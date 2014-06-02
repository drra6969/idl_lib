pro setc, n, id, red, green, blue, r, g, b

  r=fltarr(256) & g=r & b=r

  print,'id',id
  for it=0, n-2 do begin
    for i=id(it),id(it+1) do begin
      r(i) = ( i-id(it) ) *( red(it+1)-red(it) )$
                          / float( id(it+1)-id(it) ) + red(it) 
      g(i) = ( i-id(it) ) *( green(it+1)-green(it) )$
                          / float( id(it+1)-id(it) ) + green(it) 
      b(i) = ( i-id(it) ) *( blue(it+1)-blue(it) )$
                          / float( id(it+1)-id(it) ) + blue(it) 
    endfor
  endfor

  r(255)=1. & g(255)=1. & b(255)= 1.
  r(0)=0. & g(0)=0. & b(0)=0. 

return
end

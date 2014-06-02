
for n = 0.1, 0.9, 0.05  do begin
  omegai = sqrt(0.1)
  c0 = n^2.*(n^2.-1.0-omegai^2.)
  c1 = -2.0*n*(1.0-omegai^2.)
  c2 = -(2.0*n^2.0+1.0+omegai^2.)
  c3 = 0.0
  c4 = 1.0

 coeffs = [c0, c1, c2, c3, c4]
 roots = NR_ZROOTS(coeffs)
 PRINT, 'n =', n, ',    roots:  ', roots
endfor

end

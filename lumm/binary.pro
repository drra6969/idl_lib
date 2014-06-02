function binary, i

;+
; NAME:
;        binary
;
; PURPOSE:
;        print out the binary representation of an integer number
; use as 
;
;       print, binary(i)
;       with input i of type byte, integer, or long
;
; return value is of type string
;-

n=size(i)
nn=n_elements(n)
if nn ne 3 then return, 'error: array?'

if n(1) eq 0 or n(1) gt 3 then return, 'error: wrong type'
mm=[0,4,16,32]
m=mm(n(1))

ii=bytarr(m)
format='('+strcompress(m/4)+'(4i1,1x))'
for n=0,m-1 do if i/2L^(m-n-1) then ii(n)=1 else ii(n)=0

return, string(ii, form=format)
end

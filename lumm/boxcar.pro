function boxcar, f, length=length

;+
; NAME:
;      boxcar
;
; Purpose:
;      return boxcar average of the time series
;      output=boxcar(f, [length=n])
;
;      if length n of the boxcar window is not specified, a length 
;      of 3 is assumed.  Array output is the same length as input time
;      series f, where the first and last n/2 points are left unchanged.
;-

if not keyword_set(length) then length=3

length=2*(fix(length)/2) + 1   ; make sure that we use an odd number
n=n_elements(f)
delta=length/2
nstart=delta
nstop=n-1-nstart

out=f

for i=nstart,nstop,1 do out(i)=total(f(i-delta:i+delta))/float(length)

return, out
end

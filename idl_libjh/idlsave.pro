; IDL Version 2.3.2 (ultrix mipsel)
; Journal File for jah1@flare
; Working directory: /home/jah1/idl_lib
; Date: Tue May 25 15:08:01 1993
 
t=fltarr(10)
for i=0,9 do t=i
print, t
;       9
for i=0,9 do t(1)=i
; % Attempt to subscript T with <INT      (       1)> is out of range.
for i=0,9 do t(i)=i
; % Attempt to subscript T with I is out of range.
help, t
t=fltarr(10)
for i=0,9 do t(i)=float(i)
print, t
;      0.00000      1.00000      2.00000      3.00000      4.00000      5.00000
;      6.00000      7.00000      8.00000      9.00000

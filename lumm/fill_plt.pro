pro fill_plt, x, y, width=width, color=color, nofill=nofill, _extra=_extra

;+
; NAME:
;    fill_plt
;
; PURPOSE:
;    make a histogram-like plot with filled bars
;
; use as
;    fill_plt, x, y, width=width, color=color, nofill=nofill
; 
; with
;    width gives the width of each bar, default is the miniumum distance
;          between two data points
;    color species the color for filling the bars
;    /nofill prevents filling and plots just outlines
;    all plot keywords are also accepted
;-

n=n_elements(x)
if not keyword_set(width) then width=min(abs(x(0:n-2)-x(1:n-1)))
if not keyword_set(color) then color=!d.n_colors

plot, x, y, _extra=_extra, /nodata

xx=fltarr(4*n)
yy=fltarr(4*n)
one=indgen(n)*4
two=one+1
three=one+2
four=one+3

xx(one)=x-width/2.
xx(two)=xx(one)
xx(three)=x+width/2.
xx(four)=xx(three)

yy(one)=replicate(0., n)
yy(two)=y
yy(three)=y
yy(four)=yy(one)

if keyword_set(nofill) then oplot, xx, yy $
  else polyfill, xx, yy, color=color

return
end

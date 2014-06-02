pro step, x, y, _extra=_extra, oplot=oplot, layers=layers, left=left

;+
; NAME:
;        step
;
; PURPOSE:
;
;        plot data as a step function (similar to using plot, x, y, psym=10,
;                                      but with more and other options) 
;
; use as:
;        step, x, y, [/oplot,] [/layers,] [/left]
; with
;   /oplot: overplot the previous plot
;   /layers: the function that is plotted makes steps along the y-axis,
;        as if the quantities are given in a layered medium.  The default
;        is steps along the x-axis.
;   /left: by default, the steps of the step function are centered on
;        the provided x-values.  Specifying /left makes the provided
;        x-values the left edge of each step.
;   also accepts all plot keywords
;
; HISTORY:
;
;       Mon Aug 14 10:20:20 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

n=min([n_elements(x), n_elements(y)])
if abs(n_elements(x) - n_elements(y)) gt 1 then begin
    print, 'x and y must have same (+-1) number of elements'
    return
endif

xx=fltarr(2*n)
yy=fltarr(2*n)
even=indgen(n)*2
odd=even+1

if keyword_set(layers) then begin
    if n_elements(x) eq n_elements(y) then begin
        y1=fltarr(n+1)
        y1(1:n-1)=(y(1:n-1)+y(0:n-2))/2.
        y1(0)=y(0)
        y1(n)=y(n-1)
    endif else y1=y
    
    yy(even)=y1(0:n-1)
    yy(odd)=y1(1:n)
    
    xx(even)=x
    xx(odd)=x
endif else begin
    if keyword_set(left) then begin
        xx(even)=x(0:n-1)
        xx(odd)=[x(1:n-1), x(n-1)]
    endif else begin
        if n_elements(x) eq n_elements(y) then begin
            x1=fltarr(n+1)
            x1(1:n-1)=(x(1:n-1)+x(0:n-2))/2.
            x1(0)=x(0)
            x1(n)=x(n-1)
        endif else x1=x
        xx(even)=x1(0:n-1)
        xx(odd)=x1(1:n)
    endelse

    yy(even)=y
    yy(odd)=y
endelse

if keyword_set(oplot) then $
   oplot, xx, yy, _extra=_extra $
else $
   plot, xx, yy, _extra=_extra

return
end

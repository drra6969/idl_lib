function trap, y, x, limits=limits, noextrapol=noextrapol

;+
; NAME:
;     trap
;
; Purpose:
;     perform trapezoidal integration of integral y dx
;
; use as:
;      result = trap(y, x, [limits=limits])
;
;      If keyword limits [lower, upper] is given, it specifies the 
;      integration limits,  if the keyword is omitted, the entire 
;      range is integrated.
;
;      When limits are specified, linear interpolation (or
;      extrapolation) will be used to integrate to exactly the
;      specified limits.
;      If the integration limits do not contain any points (from
;      arrays x, y), linear extrapolation is used to substitute.
;
;      NOTE: array x must be sorted such that x(n-1) lt x(n) lt x(n+1)
;
;      If the keyword /noextrapol is specified, the extrapolation to
;      the limits is prevented (this makes only sense if keyword
;      limits also has values.
;
; HISTORY:
;
;       Thu Oct 19 13:34:02 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

; xx and yy are local arrays used so that x and y are not changed
xx=x
yy=y
sign=1.

if keyword_set(limits) then begin
  n1=where(min(limits) lt xx and xx lt max(limits), find1)
  if find1 gt 0 then begin
                                ; add the integration limits as new
                                ; data points at the ends of the data
                                ; point array.  Find the values at
                                ; these points by inter- or extrapolation
      n0=min(n1)
      n2=max(n1)
      if keyword_set(noextrapol) then xx=[min(limits), x(n0:n2), max(limits)]
      else xx=[min(limits), x(n0:n2), max(limits)]
      yy=[interpol(y, x, xx(0)), y(n0:n2), interpol(y, x, xx(n2-n1+2))]
  endif else begin
                                ; there are no data points inside the
                                ; integration limits 
      xx=[min(limits), max(limits)]
      yy=[interpol(y, x, xx(0)), interpol(y, x, xx(1))]
  endelse
  if limits(0) gt limits(1) then sign=-1.
endif

nn=n_elements(xx)-1
yy=(yy(0:nn-1)+yy(1:nn))*0.5
dx=xx(1:nn)-xx(0:nn-1)

return, sign*total(yy*dx)
end

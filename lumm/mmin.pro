function mmin, array, min_subscript, above=above, max=max

;+
; NAME:
;       mmin
;
; PURPOSE:
;       find the minimum of an array above a given threshold
;
; use as:
;       print, mmin(array[, min_subscript=m][, above=a][, max=amax])
;
; where the keywords have the same meaning as in IDL's "min" function,
; except:
;      above=a:  specify a value, a, to set the lower limit for the 
;                search of the minimum of the array.  The search is
;                done excluding this lower limit.
;
; example:
;      a=findgen(20)*1e-5
;      plot_io, a, yrange=[mmin(a, above=0), max(a)]
;      makes a plot of array a excluding values less than or equal to 0
; 
; HISTORY:
;
;       Thu Aug 17 12:22:14 1995, Dirk Lummerzheim
;       <lumm@loke.gi.alaska.edu>
;
;-

if n_elements(above) le 0 then return, min(array, min_subscript, max=max)

ii=where(array gt above, i0)
if i0 gt 0 then value=min(array(ii), min_subscript, max=max) $
           else return, above
min_subscript = ii(min_subscript)

return, value

end

pro setps, back=back, _extra=_extra
;+
; NAME:
;         setps
;
;
; PURPOSE:
;         set plotting device to PostScript and set paper size to
;         cover the entire page
;
;
; CALLING SEQUENCE:
;         setps
;
; 
; INPUTS:
;         set keyword /back to close the device and set the plotting
;         back to the screen 
;         also takes all device keywords (e.g. /landscape)
;
;
; MODIFICATION HISTORY:
;
;-

if keyword_set(back) then begin
    device, /close
    set_plot, 'x'
endif else begin
    set_plot, 'ps'
    device, yoffset=2.5, ysize=23, _extra=_extra
endelse

return
end

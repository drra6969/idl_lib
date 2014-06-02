function log_label, axis, index, value

;+
; NAME:
;     log_label
;
; Purpose:
;     force exponential style notation on log_axis
;
;     to be used with keyword x[y,z]tickformat in plot routines
;
; example: 
;     plot_oi, x, y, xtickformat='log_label'
;-

exponent=alog10(value)
label=string(exponent, format="('!s10!e',i,'!n')")

return, strcompress(label, /remove_all)
end

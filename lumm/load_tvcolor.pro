pro load_tvcolor
;+
; NAME:
;       load_tvcolor
;
; PURPOSE:
;
; load colortable 22 with black lines instead of red ones, and change
; the saturation such that it looks better on a regular TV (for video
; production) 
;
; use as:
;       load_tvcolor
;
; common block:
;       common my_color, r, g, b
;       with r, g, b, the RGB values (example: pass these to write_gif
;       to get the same color scale on gif files)
;-

common my_color, r, g, b
if not !d.flags/256 then !p.color=!d.n_colors-1  ; for PostScript
black=0
white=255
loadct, 22
gamma_ct, 0.65       ; stretch the colortable a bit - it looks nicer!
tvlct, r, g, b, /get  ; get RGB color table
color_convert, r, g, b, h, l, s, /rgb_hls  ; translate to HLS

n=!p.color
h(n)=0                ; text
l(n)=1.
s(n)=1.
h(0)=250              ; background
l(0)=0.6
s(0)=0.7
s(1:n-1)=s(1:n-1)*.8
tvlct, h, l, s, /hls
color_convert, h, l, s,  r, g, b, /hls_rgb ; translate back to RGB (to
                                           ; save in the common block...

return
end


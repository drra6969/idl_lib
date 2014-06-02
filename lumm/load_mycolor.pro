pro load_mycolor, aurora=aurora, reverse=reverse, video=video
;+
; NAME:
;      load_mycolor
;
; PURPOSE:
;
; load special color tables, and keep it in a common block
;
;    default:
;       load colortable 22 with black lines instead of red ones...
;       behaves just as "loadct, 22", except that it will set the color 
;       used for line drawing to black (instead of red, as 22 has)
;       resets !p.color for postscript devices....
;
; KEYWORDS:
;
;    aurora:
;         selects a colortable similar to "green", "blue", or "red" to
;         simulate aurora (set aurora="g", "b", or "r")
;    reverse:
;         exchange black and white (i.e. make white letters on black
;         background) 
;    video:
;         change saturation to make colortable more suitable for
;         video production (keyword 'reverse' has no effect in this
;         case).
;
; common block:
;         common my_color, r, g, b
;         with r, g, b the RGB values of the new color table
;
; Dirk Lummerzheim (1993)
;-

common my_color, r, g, b
if not !d.flags/256 then !p.color=!d.n_colors-1  ; for PostScript
if keyword_set(reverse) then begin
    black=255
    white=0
endif else begin
    black=0
    white=255
endelse 

if keyword_set(aurora) then begin
    case aurora of
        'b': begin
            loadct, 1
            gamma_ct, 0.5
        end
        'r': begin
            loadct, 3
            gamma_ct, 0.75
        end
        'g': begin
            loadct, 8
            gamma_ct, 0.35
        end
    endcase
    tvlct, r, g, b, /get
endif else begin
    loadct, 22
    gamma_ct, 0.65   ; stretch the colortable a bit - it looks nicer!
    tvlct, r, g, b, /get
    n=!p.color
    r(n)=black       ; set the last entry in the color table to black
    b(n)=black
    g(n)=black
    r(0)=white       ; set the first entry in the color table to white
    b(0)=white
    g(0)=white
    tvlct, r, g, b
endelse

if keyword_set(video) then begin
    tvlct, r, g, b, /get        ; get RGB color table
    color_convert, r, g, b, h, l, s, /rgb_hls ; translate to HLS

    n=!p.color
    h(n)=0                      ; white text
    l(n)=1.
    s(n)=1.
    h(0)=250                    ; blue background
    l(0)=0.6
    s(0)=0.7
    s(1:n-1)=s(1:n-1)*.8
    tvlct, h, l, s, /hls
    color_convert, h, l, s,  r, g, b, /hls_rgb ; translate back to RGB (to
                                ; save in the common block...
endif

return
end


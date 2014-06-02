pro anim_handler, ev
   widget_control, /destroy, ev.top
end


pro flyby, nframes=nframes, theta, lambda, distance, uvi=uvi, vis=vis

; animate a sequence of auroral views
; use reduced resolution to speed things up

if not keyword_set(nframes) then nframes=30

if n_params() eq 0 then begin
    lambda0=-60.
    time=2.*findgen(nframes)/(nframes-1.)-1.
    distance=9.-7.*time^2
    theta=90.*time^2
    lambda=fltarr(nframes)+lambda0
    lambda(nframes/2:nframes-1)=lambda0+180.
    ;yshift=-(findgen(nframes)-nframes/2.)*16./nframes
    yshift=(distance-9)/9.*16
endif else begin
    nframes=n_elements(distance)
    yshift=fltarr(nframes)
endelse

base=widget_base()
animate=cw_animate(base, 256, 256, nframes)
widget_control, base, /real
load_mycolor, /aurora
if keyword_set(uvi) then extra={angle:8., round:1} else $
   if keyword_set(vis) then extra={angle:6.3} else $
                            extra={angle:20.}

for frame=0,nframes-1 do begin
   window, /free, xs=256, ys=256
   a=mk_image(distance(frame), theta(frame), lambda(frame), $
              xdim=64, ydim=64, _extra=extra, yshift=yshift(frame), $
              /nadir, /noise)
   tvscl, rebin(a.patch+a.earth*0.1, 256, 256)
;   tvscl, rebin(a.rr_oval+a.p_edge+0.7*a.patch+a.day*0+a.earth*0.1, 256, 256)
;   tvscl, rebin(a.day, 256, 256)
   window=!d.window
   cw_animate_load, animate, frame=frame, window=window
   wdelete, window
endfor

; now animate the stuff

cw_animate_run, animate
xmanager, "POLAR animation", base, event_handler="anim_handler"

end

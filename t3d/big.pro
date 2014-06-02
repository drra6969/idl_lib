pro anim_handler, ev
   widget_control, /destroy, ev.top
end



   name1='gifout'
   n1frames=30
   n2frames=30
   base=widget_base()
;   xsize=853 & ysize=710
   xsize=711 & ysize=568

   animate=cw_animate(base, xsize, ysize, n1frames+n2frames)
   widget_control, base, /real


   loadct, 3
   for frame=0,n1frames-1 do begin
     read_gif, 'gifout'+string(frame,'(i2.2)'), plottt
     cw_animate_load, animate, frame=frame, image=plottt
   endfor

   for frame=0,n2frames-1 do begin
     read_gif, 'gifout'+string(frame,'(i2.2)'), plottt
     cw_animate_load, animate, frame=frame+n1frames, image=plottt
   endfor


; now animate the stuff

  cw_animate_run, animate
  xmanager, "Magnetopause Animation", base, event_handler="anim_handler"

end

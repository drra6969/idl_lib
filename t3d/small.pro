pro anim_handler, ev
   widget_control, /destroy, ev.top
end



   name1='gifout'
   n1frames=5
;   n1frames=56
   n2frames=5
   base=widget_base()
;   xsize=853 & ysize=710
   xsize=711 & ysize=568

   animate=cw_animate(base, xsize, ysize, n1frames+n2frames+3)
   widget_control, base, /real


   loadct, 3
   read_gif, 'p11', plottt
   cw_animate_load, animate, frame=0, image=plottt
   read_gif, 'p22', plottt
   cw_animate_load, animate, frame=1, image=plottt

   for frame=0,n1frames-1 do begin
     read_gif, 'gifout'+string(frame,'(i2.2)'), plottt
     cw_animate_load, animate, frame=frame+2, image=plottt
   endfor

   read_gif, 'p33', plottt
   cw_animate_load, animate, frame=n1frames+2, image=plottt

   for frame=0,n2frames-1 do begin
     read_gif, 'flux'+string(frame,'(i2.2)')+'.gif', plottt
     cw_animate_load, animate, frame=n1frames+3+frame, image=plottt
   endfor

; now animate the stuff

  cw_animate_run, animate
  xmanager, "Magnetopause Animation", base, event_handler="anim_handler"

end

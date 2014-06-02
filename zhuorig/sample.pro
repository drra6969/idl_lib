   pro sample
      
;    !p.multi=0
   !p.multi=[0,2,4]
;   ps=1
;    set_plot,'ps'
;       device, xoffset=0.5, xsize=3, ysize=2, yoffset=0.5, /inches
;       device, filename='ttt.ps'
   
;   device, retain=2, decomposed=1

; (11) means starting from 0 to 10   
   xt=fltarr(11)
   yt=fltarr(11)
; open, read, & plot should have the same channel (3)
   openr,3, 'sample_test.data'
;read data file
   for i=1,10 do begin
       readf, 3, va1,va2, format='(2f7.2)'
       xt(i)=va1
       yt(i)=va2        
       print, i, xt(i), yt(i)
   endfor
   close,3
   
   plot, xt, yt, xtitle = "Scale", ytitle="Number", $
;   xyouts, 20,1.0e-7,"Time", $   
   xrange=[0,300], yrange=[0,200]
    
   original = sin((findgen(200)/35)^2.5)   
   plot, original, xtitle = "Time", ytitle="Amplitude"
   xyouts, 20,-0.5,"Real Time"
   
   noisy = original + ((randomu(seed,200)-0.5)/2)
   plot, noisy, $   
   xrange=[0,150], $
   yrange=[-0.5,0.5]
   
   plot, original, xtitle = "Time", ytitle="Amplitude", thick=3
   oplot, noisy

;
   !P.FONT=-1  
   smoothed = smooth (noisy,5)   
   plot, smoothed, title="Smoothed data, !7q =1"
   
   !P.FONT=0  
   smoothed = smooth (noisy,5)   
   plot, smoothed, title="Smoothed data"
   
   plot, original, xtitle = "Time", ytitle="Amplitude"
   oplot, noisy, linestyle=1
   oplot, smoothed, linestyle=2
   
   empty
   device,/close
    
   end

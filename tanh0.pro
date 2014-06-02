; START OF PROGRAM
  xmin=-0.7 & xmax=1.
  vs=100.0 & nx=801 & dx=(xmax-xmin)/(nx-1.) & x=findgen(nx)*dx+xmin
;-------------
; PARAMETER:
; vs=vector/scalar speed
;-------------

; Speedup:

  tanh1=tanh(x)
  tanh5=tanh(5.*x)
  tanh20=tanh(20.*x)
  
; loop to replot the result (in case of interactive parameter changes or
;                            generationof postscript files)
    again='y' & withps='n' & contin='y'
    while again eq 'y' do begin

; some settings for postscript
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='tanh.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
      endif

;actually plot something
      !P.REGION=[0.,0.,1.0,1.25]
      print, 'plot page?'
      read, contin
      if (contin eq '' or contin eq 'y') then begin
        !P.CHARSIZE=1  
        !P.MULTI=[0,0,1]
        !P.POSITION=[0.1,0.35,0.9,0.8]
        amax = 1.
        amin = -1.
        plot, x, tanh1,$
            title='',xtitle='x',ytitle='f',$
            font=3, yrange=[amin, amax], ystyle=1, xstyle=1, line=0
        oplot, x, tanh5, line=3
        oplot, x, tanh20, line=2
        plots,[xmin,xmax],[0,0], line=1
        plots,[0,0],[-1,1], line=1
;        plots,0,0
      endif
;doit again? 
      print, 'again?'
      read, again
;close postscript device if not needed
      if withps eq 'y' then device,/close
      set_plot,'x'

    endwhile

end



; START OF PROGRAM

  nx=100 & withps = 'n' & contin = 'y' & again = 'y'

  s=fltarr(nx,/NOZERO) & g=findgen(nx) & gmin=-2. & gmax=2.
  dg=(gmax-gmin)/nx & g=gmin+dg*g
  s = 0.5 - 3./8.*g - 1./8./g

; first page
  !P.REGION=[0.,0.,1.0,1.25]

    while again eq 'y' do begin

       !P.CHARSIZE=2.0
       !P.FONT=3
      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='tear.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
       
  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=1  
    !P.MULTI=[0,0,1]
    !P.POSITION=[0.5,0.08,0.9,0.4]
    amax = 2.
    amin = -2.
    plot, g, s,$
        title='tilde S(g)',$
        xtitle='g', ytitle='S', font=3, yrange=[amin, amax]
  endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end


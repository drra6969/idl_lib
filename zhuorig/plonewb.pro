; MAIN PROGRAM
;   velocity plots for newburger

   name='' & contin='' & again='y' & withps='n' & run='' & fnumber=1
   pi=3.141593

; GRID 
   nx = long(51)  &  ny = long(51) &  niter = long(51)  &  rms=0.1 

   openr, 8, 'newb.bin',/F77_UNFORMATTED
   readu, 8,  nx,ny
   print, 'dimension nx=',nx,'     ny=',ny
   x=fltarr(nx) & y=fltarr(ny)
   uv=double(fltarr(nx,ny)) & vv=uv 
   readu, 8,  x,y
   readu, 8,  uv,vv
   close,8
   

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

        xmax=max(x) & xmin=min(x) & dxpos=(xmax-xmin)
        ymax=max(y) & ymin=min(y) & dypos=(ymax-ymin)
       
        xpos=xmax - 0.1*dxpos
        ypos0=ymin + 0.6*dypos   
        ypos1=ymin + 0.7*dypos   
        ypos2=ymin + 0.8*dypos   
        ypos3=ymin + 0.9*dypos   
        xa1 = 0.10 & xe1 = 0.9
        ylo1 = 0.7 & yup1 = 0.90
        ylo2 = 0.07 & yup2 = 0.47

       
  print, 'plot  page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,5,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
;       !X.TICKS=2
;       !Y.TICKS=8
;       !Y.TICKlen=0.04
;       !X.THICK=2
;       !Y.THICK=2
       !P.THICK=2.
       !X.RANGE=[xmin,xmax]
       !Y.RANGE=[ymin,ymax]

	!P.POSITION=[xa1,ylo1,xe1,yup1]
        velovect, uv, vv, x, y, length=1.5,$
        title=' Velocity'


        
  endif

     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


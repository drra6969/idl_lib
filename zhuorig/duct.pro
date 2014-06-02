; MAIN PROGRAM

;----PARAMETER-------
  xmin = -1. & ymin = -1.
  xmax =  1. & ymax =  1.
;--------------------
   name='' & again='y' & withps='n' & run='' & contin=''
   name1='' & name2='' & name3='' & name4='' 
     while (again eq 'y') or (again eq '') do begin

      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        device,/portrait
;        device,/landscape
;        device,/inches,xsize=10.0,scale_factor=1.0,xoffset=1.0
;        device,/inches,ysize=8.0,scale_factor=1.0,yoffset=1.0
        !P.THICK=2.
       endif
   nx=1l 
   ny=1l 

   openr, 8,  'duct.bin' ,/F77_UNFORMATTED
   readu, 8,  nx
   x=fltarr(nx)
   readu, 8,  x
   readu, 8,  ny
   y=fltarr(ny)
   readu, 8,  y
   wexact=fltarr(nx,ny)
   readu, 8, wexact
   readu, 8, r
   close, 8

       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,0,1,0,0]
       !P.CHARSIZE=1.0
       !P.FONT=2
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
    u = fltarr(ny)
    u = 1.0/2.0*(1.0-y*y)
    plot, y, u, xstyle=1,ystyle=1,xtitle='y',ytitle='w'

   wu = fltarr(ny)
   rab = fltarr(5)
   rms = fltarr(5)

  for it=0, 4 do begin
    fnumber = string(10-it*2,form='(i2.2)')
  
; READ INPUT DATA OF DIMENSION NX, NY
   name='duct'+fnumber+'.bin'
print, 'name is:',name
   openr, 8,  name,/F77_UNFORMATTED
   readu, 8,  nx
print,nx
   readu, 8,  x
   readu, 8,  ny
print,ny
   readu, 8,  y
   readu, 8, wexact
   readu, 8, r
   close, 8
   
   rab(it) = r
   
   for i=0, ny-1 do wu(i)=wexact(5,i)
    sum = 0.0
   for i=0, ny-1 do sum = sum+(wu(i)-u(i))*(wu(i)-u(i))
    rms(it) = sqrt(sum/ny)
   print,'rab = ',rab(it),'   rms = ', sum
    
    oplot,y,wu, line=it+1
   
   endfor
        oplot, [-0.9,-0.7],[0.45,0.45]
   	xyouts,-0.65,0.448, 'u = (1-y^2)/2'
        oplot, [-0.45,-0.25],[0.02,0.02],line=1
	xyouts,-0.2,0.018,'rab = '+string(rab(0),form='(f5.2)') $
	      +', rms = '+string(rms(0),form='(f9.6)')
	oplot, [-0.45,-0.25],[0.04,0.04],line=2
	xyouts,-0.2,0.038,'rab = '+string(rab(1),form='(f5.2)') $
	      +', rms = '+string(rms(1),form='(f9.6)')
	oplot, [-0.45,-0.25],[0.06,0.06],line=3
	xyouts,-0.2,0.058, 'rab = '+string(rab(2),form='(f5.2)') $
	      +', rms = '+string(rms(2),form='(f9.6)')
 	oplot, [-0.45,-0.25],[0.08,0.08],line=4
	xyouts,-0.2,0.078,'rab = '+string(rab(3),form='(f5.2)') $
	      +', rms = '+string(rms(3),form='(f9.6)')
	oplot, [-0.45,-0.25],[0.1,0.1],line=5
	xyouts,-0.2,0.098,'rab = '+string(rab(4),form='(f5.2)') $
	      +', rms = '+string(rms(4),form='(f9.6)')
   
    !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

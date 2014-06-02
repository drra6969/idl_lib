; MAIN PROGRAM

;----PARAMETER-------
  xmin =  0. & ymin = 0.
  xmax =  1. & ymax = 100.
;--------------------
   time=0.0 
   name='' & again='y' & withps='n' & run='' & contin=''

     while (again eq 'y') or (again eq '') do begin

      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/portrait
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        !P.THICK=2.
       endif
   nx=1l 
   nouts = 5
   time=fltarr(5)

; READ INPUT DATA OF DIMENSION NX, NY
   name='fivep06.bin'
   openr, 8,  name,/F77_UNFORMATTED
   readu, 8,  nx
print,nx
   x=fltarr(nx)
   readu, 8,  x
   fout=fltarr(nx,nouts)
   t=0.0 & f=fltarr(nx)
   for it=0, nouts-1 do begin
     readu, 8,  t, f
     time(it)=t & fout(*,it)=f
   endfor
   close, 8
   print, 'dimension nx=',nx
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,0,3,0,0]
       !P.CHARSIZE=1.0
       !P.FONT=2
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2

    !Y.RANGE=[ymin,ymax]

    xa=0.1 & xe=0.8 & dpy=0.24
    ylo1=0.65 & yup1=ylo1+dpy
    ylo2=0.35 & yup2=ylo2+dpy
    ylo3=0.05 & yup3=ylo3+dpy
    

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',$
	title='Five-Point Scheme, s=0.3, nx=6, tmax=6000'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

   name='fivep11.bin'
   openr, 8,  name,/F77_UNFORMATTED
   readu, 8,  nx
   x=fltarr(nx)
   readu, 8,  x
   fout=fltarr(nx,nouts)
   t=0.0 & f=fltarr(nx)
   for it=0, nouts-1 do begin
     readu, 8,  t, f
     time(it)=t & fout(*,it)=f
   endfor
   close, 8
   print, 'dimension nx=',nx

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',$
	title='Five-Point Scheme, s=0.3, nx=11, tmax=6000'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

   name='fivep21.bin'
   openr, 8,  name,/F77_UNFORMATTED
   readu, 8,  nx
   x=fltarr(nx)
   readu, 8,  x
   fout=fltarr(nx,nouts)
   t=0.0 & f=fltarr(nx)
   for it=0, nouts-1 do begin
     readu, 8,  t, f
     time(it)=t & fout(*,it)=f
   endfor
   close, 8
   print, 'dimension nx=',nx

	!P.POSITION=[xa,ylo3,xe,yup3]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',$
	title='Five-Point Scheme, s=0.3, nx=21, tmax=6000'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

    !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

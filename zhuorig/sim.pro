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
   name='sim100.bin'
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
       !P.MULTI=[0,2,3,0,0]
       !P.CHARSIZE=1.0
       !P.FONT=2
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2

    !Y.RANGE=[ymin,ymax]

    xa1=0.05 & xe1=0.4 & xa2=0.5 & xe2=0.85  & dpy=0.24
    ylo1=0.65 & yup1=ylo1+dpy
    ylo2=0.35 & yup2=ylo2+dpy
    ylo3=0.05 & yup3=ylo3+dpy
    
  print, 'plot the first page?'

;-----------------------------------------------------------------------
; plot first page ( Rhon, Pn, Rho, P, T)
;-----------------------------------------------------------------------

	!P.POSITION=[xa1,ylo1,xe1,yup1]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',title='Problem 9 s = 1.0 tmax = 40000'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

   name='sim102.bin'
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

	!P.POSITION=[xa1,ylo2,xe1,yup2]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',title='Problem 9 s = 1.02 tmax = 40800'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

   name='sim104.bin'
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

	!P.POSITION=[xa1,ylo3,xe1,yup3]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',title='Problem 9 s = 1.04 tmax = 20800'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

;   nouts = 4
;   time=fltarr(4)
   name='fivep040.bin'
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

	!P.POSITION=[xa2,ylo1,xe2,yup1]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',$
	title='five-point Scheme s = 0.4 tmax = 32000'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

   name='fivep041.bin'
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

	!P.POSITION=[xa2,ylo2,xe2,yup2]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',$
	title='five-point Scheme s = 0.41 tmax = 32800'
	for it=1,nouts-1 do begin
	  oplot,x,fout(*,it), line=it
	endfor

    name='fivep042.bin'
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

	!P.POSITION=[xa2,ylo3,xe2,yup3]
        bmin = 0.0
        bmax = 100.0
	plot, x, fout(*,0), $ 
	xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=1,ystyle=1, $
	xtitle='x',ytitle='f',$
	title='five-point Scheme s = 0.42 tmax = 16800'
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

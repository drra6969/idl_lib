; MAIN PROGRAM

;----PARAMETER-------
  xmin = -1. & ymin = -1.
  xmax =  1. & ymax =  1.
;--------------------
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
   ny=1l 

; READ INPUT DATA OF DIMENSION NX, NY
   name='hw18.bin'
   openr, 8,  name,/F77_UNFORMATTED
   readu, 8,  nx
print,nx
   x=fltarr(nx)
   readu, 8,  x
   readu, 8,  ny
print,nx
   y=fltarr(ny)
   readu, 8,  y
      wexact=fltarr(nx,ny)
      wg1=fltarr(nx,ny)
      wg2=fltarr(nx,ny)
      wsub1=fltarr(nx,ny)
      wsub2=fltarr(nx,ny)
      wcol1=fltarr(nx,ny)
      wcol2=fltarr(nx,ny)
   readu, 8, wexact
   readu, 8, wg1
   readu, 8, wg2
   readu, 8, wsub1
   readu, 8, wsub2
   readu, 8, wcol1
   readu, 8, wcol2
   close, 8
   print, 'dimension nx=',nx, 'ny=',ny
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,2,3,0,0]
       !P.CHARSIZE=1.0
       !P.FONT=2
       !X.TICKS=4
       !Y.TICKS=4
       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2

;    !Y.RANGE=[ymin,ymax]
;    !X.RANGE=[xmin,xmax]

    xa1=0.05 & xe1=0.4 & xa2=0.5 & xe2=0.85 & dpy=0.24
    ylo1=0.7 & yup1=ylo1+dpy
    ylo2=0.4 & yup2=ylo2+dpy
    ylo3=0.1 & yup3=ylo3+dpy
    
    !P.POSITION=[xa1,ylo1,xe1,yup1]
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],wg1,x,y, $
            TITLE = 'Galerkin method, N=1'
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],/OVERPLOT, $
                                     C_LINESTYLE=2,wexact,x,y
    !P.POSITION=[xa1,ylo2,xe1,yup2]
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],wsub1,x,y, $
            TITLE = 'Subdomain method, N=1'
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],/OVERPLOT, $
                                     C_LINESTYLE=2,wexact,x,y
    !P.POSITION=[xa1,ylo3,xe1,yup3]
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],wcol1,x,y, $
            TITLE = 'Collocation method, N=1'
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],/OVERPLOT, $
                                     C_LINESTYLE=2,wexact,x,y
    plots, [0.7,1.1],[-1.5,-1.5], line=2
    xyouts, 1.2, -1.55, 'Exact solution'
    !P.POSITION=[xa2,ylo1,xe2,yup1]
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],wg2,x,y, $
            TITLE = 'Galerkin method, N=2'
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],/OVERPLOT, $
                                     C_LINESTYLE=2,wexact,x,y
    !P.POSITION=[xa2,ylo2,xe2,yup2]
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],wsub2,x,y, $
            TITLE = 'Subdomain method, N=2'
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],/OVERPLOT, $
                                     C_LINESTYLE=2,wexact,x,y
    !P.POSITION=[xa2,ylo3,xe2,yup3]
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],wcol2,x,y, $
            TITLE = 'Collocation method, N=2'
    contour,level=[0.05,0.15,0.25], C_LABELS=[1,1,1],/OVERPLOT, $
                                     C_LINESTYLE=2,wexact,x,y
    !P.FONT=3

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end

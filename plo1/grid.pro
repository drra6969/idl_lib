; START OF PROGRAM

  pi=3.141593
  nx = 23 
  xmin = 0.  & xmax = 10.
  eps = 0.05 
  
  dx =  (xmax-xmin)/(nx-3)
  eeps = eps/dx & lam = 0.7 & gam = 2.7
;  eeps = 1.3 & eps = eeps*dx
  print, 'eeps:',eeps
  xanf = xmin
  ix0 = 1
  beta = pi/(xmax-xmin)
  kk = 1./(nx-3.)
  a1 = -dx*( 1. - eeps )*24./17./pi/kk 
  a2 =  dx*( 1. - eeps )* 6./17./pi/kk
  a3 = -dx*( 1. - eeps )* 8./51./pi/kk
  a4 =  dx*( 1. - eeps )* 3./68./pi/kk
  b1 = 4./3.*(eps-dx)/dx/beta
  b2 = (dx-eps)/6./dx/beta
  
  print, 'coefs -a- :',a1,a2,a3,a4
  print, 'coefs -b- :',b1,b2
  ix = indgen(nx)
  w  =  dx*(ix-ix0)
  x  =  w + a1*sin(beta*w) + a2*sin(2.*beta*w)$
          + a3*sin(3.*beta*w) + a4*sin(4.*beta*w)
  delx = dx*(1. + a1*beta*cos(beta*w) $
              + 2.*a2*beta*cos(2.*beta*w) $
               + 3.*a3*beta*cos(3.*beta*w) $
                + 4.*a4*beta*cos(4.*beta*w) )
  dddelx = dx  + pi*kk*( a1 + 2.*a2 + 3.*a3 + 4.*a4 )
  print,'eps, eps/dx0:', eps, eeps
  print,'dx(nxhalf), dx(nxhalf)/dx0:',delx((nx+1)/2),delx((nx+1)/2)/dx
  ddelx = -dx*dx*(a1*beta^2*sin(beta*w) $
              + 4.*a2*beta^2*sin(2.*beta*w) $
               + 9.*a3*beta^2*sin(3.*beta*w) $
               + 16.*a4*beta^2*sin(4.*beta*w) )
  ddd = ddelx/delx
  difx = 0.5/dx/(1. + a1*beta*cos(beta*w) $
          + 2.*a2*beta*cos(2.*beta*w) + 3.*a3*beta*cos(3.*beta*w) )
          
  x1  =  w + b1*sin(beta*w) + b2*sin(2.*beta*w)
  delx1 = dx*(1. + b1*beta*cos(beta*w) $
              + 2.*b2*beta*cos(2.*beta*w) )

  ddelx1 = -dx*dx*(b1*beta^2*sin(beta*w) $
              + 4.*b2*beta^2*sin(2.*beta*w) )
  ddd1 = ddelx1/delx1
  

    again='y' & withps='n' & contin='y'
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
; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=1  
    !P.MULTI=[0,0,3]
    !P.POSITION=[0.15,0.05,0.75,0.32]
    amax = max([delx,delx1])
    amin = min([delx,delx1])
    plot, ix, delx,$
        title='Delta x',$
        xtitle='ix', font=3, yrange=[0, amax], xrange=[0, nx]
    oplot, ix, delx1, line=2



    !P.POSITION=[0.15,0.37,0.75,0.64]
    plot, x, delx,$
        title='Delta x',$
        xtitle='x', font=3, yrange=[0, 5], xrange=[xmin, 10]
    oplot, x1, delx1,line=2


    !P.POSITION=[0.15,0.69,0.75,0.96]
    amax = max([ddelx,ddd])
    amin = min([ddelx,ddd])
    plot, ix, ddelx,$
        title='Diff Delta x',$
        xtitle='ix', font=3, yrange=[amin, amax], xrange=[0, nx]
    oplot, ix, ddd, line=1
    oplot, ix, ddelx1, line=2
    oplot, ix, ddd1, line=3

   endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end


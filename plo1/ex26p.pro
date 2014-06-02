; START OF PROGRAM

  nx=100 & withps = 'n' & contin = 'y' & again = 'y'
  pi=3.141593

  theta=findgen(nx) & tmin=0. & tmax=1.
  dtheta=(tmax-tmin)/(nx-1) & theta=tmin+dtheta*(theta+1.)
  arg=pi*theta
;--PARAMETER--
  c=0.1  &  alpha=0.01
  
  gdiss=fltarr(nx,3) & gdisp=fltarr(nx,3)
  
  gdiss(*,0)=sqrt( ( 1.-c*(1-cos(arg(*))) )^2 + c^2*sin(arg(*))^2 )
  gdiss(*,1)=1.
  gdiss(*,2)=sqrt( ( 1.-c^2*(1-cos(arg(*))) )^2 + c^2*sin(arg(*))^2 )
  gdisp(*,0)=1./(c*arg(*))*atan( c*sin(arg(*))/( 1.-c*(1-cos(arg(*))) ) )
  gdisp(*,1)=1./(c*arg(*))*atan( c*sin(arg(*))/sqrt( 1.-c^2*sin(arg(*))^2)  )
  gdisp(*,2)=1./(c*arg(*))*atan( c*sin(arg(*))/( 1.-c^2*(1-cos(arg(*))) ) )

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
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.3,0.65,0.8,0.9]
    !X.RANGE=[0.,1.]
    !X.TICKS=4
    !Y.TICKS=4
    !P.THICK=2  
    amax = 1.
    amin = 0.
    plot, theta, gdiss(*,0)^5,$
        title='Disspation',$
        xtitle='k dx', ytitle='', font=3, yrange=[amin, amax],$
        xstyle=1,ystyle=1,linestyle=0
    oplot, theta, gdiss(*,1)^5,linestyle=1
    oplot, theta, gdiss(*,2)^5,linestyle=2
        
        
    !Y.TICKS=6
    amin=min(gdisp) & amax=max(gdisp)
    print,amin,amax
    amin=-1.5 & amax=1.5
    !P.POSITION=[0.3,0.1,0.8,0.55]
    plot, theta, gdisp(*,0),$
        title='Dispersion',$
        xtitle='k dx', ytitle='', font=3, yrange=[amin, amax],$
        xstyle=1,ystyle=1,linestyle=0
    oplot, theta, gdisp(*,1),linestyle=1
    oplot, theta, gdisp(*,2),linestyle=2
        
  endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end


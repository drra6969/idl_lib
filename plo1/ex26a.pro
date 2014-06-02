; START OF PROGRAM

  nx=100 & withps = 'n' & contin = 'y' & again = 'y'
  pi=3.141593

  theta=findgen(nx) & tmin=0. & tmax=1.
  dtheta=(tmax-tmin)/(nx-1) & theta=tmin+dtheta*(theta+1.)
  arg=pi*theta
;--PARAMETER--
  c=1.45  &  alpha=0.01
  
  gdiss=fltarr(nx,3) & gdisp=fltarr(nx,3)
  gr=gdiss & gi=gdiss & fr=gdiss & fi=gdiss & gabs=gdiss 
  
  delta=0.
  gr(*,0)=1.-2.*delta*(1.-cos(arg(*)))
  gi(*,0)=0.5*c*sin(arg(*))
  gabs(*,0)=   gr(*,0)^2+gi(*,0)^2
  fr(*,0)  = ( gr(*,0)^2-gi(*,0)^2 )/gabs(*,0)
  fi(*,0)  =(2.*gr(*,0) * gi(*,0))/gabs(*,0)

  delta=1./6.
  gr(*,1)=1.-2.*delta*(1.-cos(arg(*)))
  gi(*,1)=0.5*c*sin(arg(*))
  gabs(*,1)=   gr(*,1)^2+gi(*,1)^2
  fr(*,1)  = ( gr(*,1)^2-gi(*,1)^2 )/gabs(*,1)
  fi(*,1)  =(2.*gr(*,1) * gi(*,1))/gabs(*,1)
  
  delta=1./6.+c*c/12.
  gr(*,2)=1.-2.*delta*(1.-cos(arg(*)))
  gi(*,2)=0.5*c*sin(arg(*))
  gabs(*,2)=   gr(*,2)^2+gi(*,2)^2
  fr(*,2)  = ( gr(*,2)^2-gi(*,2)^2 )/gabs(*,2)
  fi(*,2)  =(2.*gr(*,2) * gi(*,2))/gabs(*,2)
  
  
  gdiss(*,0)=sqrt( fr(*,0)^2 + fi(*,0)^2 )
  gdiss(*,1)=sqrt( fr(*,1)^2 + fi(*,1)^2 )
  gdiss(*,2)=sqrt( fr(*,2)^2 + fi(*,2)^2 )
  gdisp(*,0)=1./(c*arg(*))*atan( fi(*,0)/fr(*,0) )
  gdisp(*,1)=1./(c*arg(*))*atan( fi(*,1)/fr(*,1) )
  gdisp(*,2)=1./(c*arg(*))*atan( fi(*,2)/fr(*,2) )

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
    plot, theta, gdiss(*,0),$
        title='Disspation',$
        xtitle='k dx', ytitle='', font=3, yrange=[amin, amax],$
        xstyle=1,ystyle=1,linestyle=0
    oplot, theta, gdiss(*,1),linestyle=1
    oplot, theta, gdiss(*,2),linestyle=2
        
        
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


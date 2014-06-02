; START OF PROGRAM

  nx=100 & withps = 'n' & contin = 'y' & again = 'y'
  pi=3.141593

  theta=findgen(nx) & tmin=0. & tmax=1.
  dtheta=(tmax-tmin)/(nx-1) & theta=tmin+dtheta*(theta+1.)
  arg=pi*theta  & cost=cos(arg) & sint=sin(arg)
  fr=fltarr(nx,4) & fi=fr & gdiss=fr & gdisp=fr & argu=fr
  for i=0,3 do argu(*,i)=arg(*)
  
  
  
;--PARAMETER--
  u=1.0 & alpha=0.5 & dt=0.05 & dx=0.2
  c=u*dt/dx  &  s=alpha*c/u/dx 
  astar=alpha+0.5*u*c*dx  &  sstar=astar*c/u/dx
  
;--LEAPFROG/DUFORT-FRANKEL
  br=fltarr(nx) & bi=br & denom=(2.+4.*s)
  br = (1.+4.*s*cost) & bi = -2.*c*sint
  cr = br^2 - bi^2 - 8.*s*(1.+2.*s) & ci = 2.*bi*br
  amp = sqrt(cr^2 + ci^2) & phi = atan(ci/cr)
  cosp2=cos(phi/2.) & sinp2=sin(phi/2)
  fr(*,0) = (br(*)+sqrt(amp(*))*cosp2(*))/denom
  fi(*,0) = (bi(*)+sqrt(amp(*))*sinp2(*))/denom
;--LAX-WENDROFF
  fr(*,1) = (1.-2.*sstar*(1-cost(*)))
  fi(*,1) = c*sint(*)
;--CRANK-NICHOLSON
  gr1=fltarr(nx,2) & gi1=gr1 & gabs1=gr1 
  gr2=gr1          & gi2=gr1 & gabs2=gr1 

  gr1(*,0) = 1.-s*(1.-cost(*))  &  gi1(*,0) = -0.5*c*sint(*)
  gr2(*,0) = 1.+s*(1.-cost(*))  &  gi2(*,0) =  0.5*c*sint(*)
  gabs2(*,0) =  gr2(*,0)^2+gi2(*,0)^2
  fr(*,2)  = ( gr1(*,0)*gr2(*,0) + gi1(*,0)*gi2(*,0) )/gabs2(*,0)
  fi(*,2)  = (-gr1(*,0)*gi2(*,0) + gr2(*,0)*gi1(*,0) )/gabs2(*,0)
  
  delta=0.25
  gr1(*,1) = 1.-(s+delta)*(1.-cost(*))  &  gi1(*,1) = -0.5*c*sint(*)
  gr2(*,1) = 1.+(s-delta)*(1.-cost(*))  &  gi2(*,1) =  0.5*c*sint(*)
  gabs2(*,1) =  gr2(*,0)^2+gi2(*,0)^2
  fr(*,3)  = ( gr1(*,1)*gr2(*,1) + gi1(*,1)*gi2(*,1) )/gabs2(*,1)
  fi(*,3)  = (-gr1(*,1)*gi2(*,1) + gr2(*,1)*gi1(*,1) )/gabs2(*,1)
  
  gdiss=sqrt( fr^2 + fi^2 )
  gdisp=1./(c*argu)*atan( fi/fr )

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
    amax = 1.25
    amin = 0.25
    plot, theta, gdiss(*,0),$
        title='Disspation',$
        xtitle='k dx', ytitle='', font=3, yrange=[amin, amax],$
        xstyle=1,ystyle=1,linestyle=0
    oplot, theta, gdiss(*,1),linestyle=1
    oplot, theta, gdiss(*,2),linestyle=2
    oplot, theta, gdiss(*,3),linestyle=3
        
        
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
    oplot, theta, gdisp(*,3),linestyle=3
        
  endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end


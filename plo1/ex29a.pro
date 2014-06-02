; START OF PROGRAM

  nx=100 & withps = 'n' & contin = 'y' & again = 'y'
  pi=3.141593

  theta=findgen(nx) & tmin=0. & tmax=1.
  dtheta=(tmax-tmin)/(nx-1) & theta=tmin+dtheta*(theta+1.)
  arg=pi*theta  & cost=cos(arg) & sint=sin(arg)
  fr=fltarr(nx,4) & fi=fr & gdiss=fr & gdisp=fr & argu=fr
  for i=0,3 do argu(*,i)=arg(*)
  
  
  
;--PARAMETER--
  u=1.0 & alpha=0.01 & dt=0.02 & dx=0.2
  c=u*dt/dx  &  s=alpha*c/u/dx  & rcell = u*dx/alpha
  astar=alpha+0.5*u*c*dx  &  sstar=astar*c/u/dx  & k=arg/dx  
  
;--LEAPFROG/DUFORT-FRANKEL
  br=fltarr(nx) & bi=br & denom=(1.+2.*s)
  br = 2.*s*cost  &  bi = c*sint
  cr = br^2 - bi^2 +1.-4.*s*s & ci = 2.*bi*br
  amp = sqrt(cr^2 + ci^2) & phi = atan(ci/cr)
  cosp2=cos(phi/2.) & sinp2=sin(phi/2.)
  fr(*,0) = ( br(*)+sqrt(amp(*))*cosp2(*) )/denom
  fi(*,0) = ( bi(*)+sqrt(amp(*))*sinp2(*) )/denom
;--LAX-WENDROFF
  fr(*,1) = (1.-2.*sstar*(1-cost(*)))
  fi(*,1) = c*sint(*)
;--CRANK-NICHOLSON
  gr1=fltarr(nx,2) & gi1=gr1 
  gr2=gr1          & gi2=gr1 & gabs2=gr1 

  gr1(*,0) = 1.-s*(1.-cost(*))  &  gi1(*,0) = -0.5*c*sint(*)
  gr2(*,0) = 1.+s*(1.-cost(*))  &  gi2(*,0) =  0.5*c*sint(*)
  gabs2(*,0) =  gr2(*,0)^2+gi2(*,0)^2
  fr(*,2)  = ( gr1(*,0)*gr2(*,0) + gi1(*,0)*gi2(*,0) )/gabs2(*,0)
  fi(*,2)  = -(-gr1(*,0)*gi2(*,0) + gr2(*,0)*gi1(*,0) )/gabs2(*,0)
  
;  delta=0.2
  delta=1./6.
;  delta=1./6.+c*c/12. 
  dd1=2.*delta+s  &  dd2=2.*delta-s
  gr1(*,1) = 1.-dd1*(1.-cost(*))  &  gi1(*,1) = -0.5*c*sint(*)
  gr2(*,1) = 1.-dd2*(1.-cost(*))  &  gi2(*,1) =  0.5*c*sint(*)
  gabs2(*,1) =  gr2(*,1)^2+gi2(*,1)^2
  fr(*,3)  = ( gr1(*,1)*gr2(*,1) + gi1(*,1)*gi2(*,1) )/gabs2(*,1)
  fi(*,3)  = -(-gr1(*,1)*gi2(*,1) + gr2(*,1)*gi1(*,1) )/gabs2(*,1)
  
;  gdiss= ( sqrt( fr^2 + fi^2 ) )
  gdiss= ( sqrt( fr^2 + fi^2 ) )*exp(s*argu*argu)
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
    !P.POSITION=[0.2,0.79,0.8,0.98]
    !X.RANGE=[0.,1.]
    !X.TICKS=4
    !Y.TICKS=5
    !P.THICK=2  
    amax = 1.25
    amin = 0.0
    plot, theta, gdiss(*,0)^10, title='Dissipation',$
        xtitle=' ', ytitle='', font=3, yrange=[amin, amax],$
        xstyle=1,ystyle=1,linestyle=0
    oplot, theta, gdiss(*,1)^10,linestyle=1
    oplot, theta, gdiss(*,2)^10,linestyle=2
    oplot, theta, gdiss(*,3)^10,linestyle=3
    !P.THICK=1  
    oplot, theta, theta*0.+1.,linestyle=0
        
        
    !P.THICK=2  
    !Y.TICKS=5
    amin=min(gdisp) & amax=max(gdisp)
    print,amin,amax
    amin=-0. & amax=1.25
    !P.POSITION=[0.2,0.5,0.8,0.73]
    plot, theta, gdisp(*,0),$
        title='Dispersion',$
        xtitle='k dx', ytitle='', font=3, yrange=[amin, amax],$
        xstyle=1,ystyle=1,linestyle=0
    oplot, theta, gdisp(*,1),linestyle=1
    oplot, theta, gdisp(*,2),linestyle=2
    oplot, theta, gdisp(*,3),linestyle=3
    !P.THICK=1  
    oplot, theta, theta*0.+1.,linestyle=0
         xpos=1.02
         ypos=amin+1.0*(amax-amin)
         ypos0=amin+0.9*(amax-amin)
         ypos1=amin+0.8*(amax-amin)
         ypos2=amin+0.7*(amax-amin)
         ypos3=amin+0.6*(amax-amin)
         ypos4=amin+0.5*(amax-amin)
         ypos5=amin+0.4*(amax-amin)
         ypos6=amin+0.3*(amax-amin)
         ypos7=amin+0.2*(amax-amin)
	 xyouts,xpos,ypos,'u = '+string(u,'(f4.2)')
	 xyouts,xpos,ypos0,'alpha = '+string(alpha,'(f5.3)')
	 xyouts,xpos,ypos1,'c = '+string(c,'(f5.3)')
	 xyouts,xpos,ypos2,'s = '+string(s,'(f6.4)')
	 xyouts,xpos,ypos3,'r_cell = '+string(rcell,'(f6.2)')
	 xyouts,xpos,ypos4,'c^2 = '+string(c^2,'(f5.3)')
	 xyouts,xpos,ypos5,'s_star = '+string(sstar,'(f6.4)')
	 xyouts,xpos,ypos6,'dt = '+string(dt,'(f5.3)')
	 xyouts,xpos,ypos7,'dx = '+string(dx,'(f5.3)')
        
  endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end


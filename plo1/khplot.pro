; START OF PROGRAM

  nx=101

  q1=fltarr(nx,/NOZERO) & q2=q1 & q3=q1 & q4=q1 & v=findgen(nx) & phi=v
  pi=3.141593

  vmin=0.0 & vmax=4.0 & dv=(vmax-vmin)/float(nx-1)
  phimin=0.0 & phimax=45 & dphi=(phimax-phimin)/float(nx-1)
  v=v*dv+vmin & phi=phi*dphi+phimin

  rho1=0.25 & rho2=1.75 & rho1a=0.4 & rho2a=1.6
  b1=1.0 & b2=1.5
  phi0=25.   & phi1=15.   & v0=3.
  pn=pi/180 &  phi=phi*pn & phi0=phi0*pn & phi1=phi1*pn
  i=0

vnorm=105.0
t0=5.7
q0=2*pi/40/t0

print, rho1*rho2/(rho1+rho2)^2*v0^2*cos(phi0)^2
print, (b1^2+b2^2)*sin(phi0)^2/(rho1+rho2)

  for i=0,nx-1 do begin
    if ( v(i)^2 ge (rho1+rho2)/rho1/rho2*(b1^2+b2^2) $
                        *sin(phi0)^2/cos(phi0)^2 ) $
      then q1(i)=sqrt( rho1*rho2/(rho1+rho2)^2*v(i)^2*cos(phi0)^2 $
                       -(b1^2+b2^2)*sin(phi0)^2/(rho1+rho2) ) $
      else q1(i)=0.0
    if ( v(i)^2 ge (rho1+rho2)/rho1/rho2*(b1^2+b2^2) $
                      *sin(phi1)^2/cos(phi1)^2 ) $
      then q2(i)=sqrt( rho1*rho2/(rho1+rho2)^2*v(i)^2*cos(phi1)^2 $
                       -(b1^2+b2^2)*sin(phi1)^2/(rho1+rho2) ) $
      else q2(i)=0.0
    if ( v0^2 ge (rho1+rho2)/rho1/rho2*(b1^2+b2^2) $
                     *sin(phi(i))^2/cos(phi(i))^2 ) $
      then q3(i)=sqrt( rho1*rho2/(rho1+rho2)^2*v0^2*cos(phi(i))^2 $
                       -(b1^2+b2^2)*sin(phi(i))^2/(rho1+rho2) ) $
      else q3(i)=0.0
    if ( v0^2 ge (rho1a+rho2a)/rho1a/rho2a*(b1^2+b2^2) $
                     *sin(phi(i))^2/cos(phi(i))^2 ) $
      then q4(i)=sqrt( rho1a*rho2a/(rho1a+rho2a)^2*v0^2*cos(phi(i))^2 $
                       -(b1^2+b2^2)*sin(phi(i))^2/(rho1a+rho2a) ) $
      else q4(i)=0.0
  endfor

;  phit=40.0
;  print,'phit:',phit,cos(phit),sin(phit)
;  
;  print, rho1*rho2/(rho1+rho2)^2*v0^2*cos(phit)^2
;  print, (b1^2+b2^2)*sin(phit)^2/(rho1+rho2)


    again='y' & withps='n' & contin='y'
    while again eq 'y' do begin

       !P.CHARSIZE=2.0
;       !P.FONT=3
      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='khdisp.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

; first page
  !P.REGION=[0.,0.,1.0,1.25]

;  print, 'plot first page?'
;  read, contin
;  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=1.2
    !P.CHARTHICK=2.5
  
    !P.MULTI=[0,0,2]

    !P.POSITION=[0.2,0.6,0.75,0.8]
    amax = max([q3*q0,q4*q0])
    amin = min([q3*q0,q4*q0])
    plot, phi*180/pi, q3*q0,$
        title='KH Growth Rate', xstyle=1, $
        ytitle='q / s!U-1!N', yrange=[min(amin), max(amax)]
    oplot, phi*180/pi, q4*q0,linestyle=2
    xpos=0.6*phimax
    ypos=amin+0.95*(amax-amin)
	xyouts,xpos,ypos,'v!Dsh!N ='+string(v0*vnorm,'(i4)')+' km s!U-1!N'
    xpost=0.07*phimax
    ypost=amin+0.65*(amax-amin)
	xyouts,xpost,ypost,'q(!MP!3)',size=1.5
    xposx=0.75*phimax
    yposx=amin-0.25*(amax-amin)
	xyouts,xposx,yposx,'!MP!3',size=1.5

    !P.POSITION=[0.2,0.35,0.75,0.55]
    amax = max(q1*q0)
    amin = min(q1*q0)
    plot, v*vnorm, q1*q0,$
        title='',$
        ytitle='q / s!U-1!N', xstyle=1, $
        xrange=[vmin*vnorm, vmax*vnorm],yrange=[amin, amax]
    oplot, v*vnorm, q2*q0,linestyle=2
    xpos1=0.4*vmax*vnorm
    xpos2=0.7*vmax*vnorm
    ypos=amin+0.55*(amax-amin)
	xyouts,xpos1,ypos,'!MP!3 ='+string(phi1/pn,'(i3)')
	xyouts,xpos2,ypos,'!MP!3 ='+string(phi0/pn,'(i3)')
    xpost=0.07*vmax*vnorm
    ypost=amin+0.7*(amax-amin)
	xyouts,xpost,ypost,'q(v!Dsh!N)',size=1.5
    xposx=0.7*vmax*vnorm
    yposx=amin-0.3*(amax-amin)
	xyouts,xposx,yposx,'v!Dsh!N / km s!U-1!N',size=1.5

;   endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'

   endwhile

end


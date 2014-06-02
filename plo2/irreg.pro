; MAIN PROGRAM
;   contour plots of irregularly shaped domains

   name='' & contin='' & again='y' & withps='n' & run=''
   pi=3.141593

; GRID 
   nr = 51   &   ntheta = 51
   phi=fltarr(nr,ntheta) & x=phi & y=phi & r=phi & theta=phi
   dr=findgen(ntheta) & rmin=dr & th=dr
   
   thmin=0 & thmax=pi/2
   r0min=0.1 & r0max=1.0 & r1min=0.1 & r1max=4.0
   
   dtheta = (thmax-thmin)/(ntheta-1)
   th = thmin + dtheta*th
   for k=0, nr-1 do     theta(k,*) = th(*)
   dr0 = (r0max-r0min)/(nr-1) & dr1 = (r1max-r1min)/(nr-1)
   ddr = (dr1-dr0)/(ntheta-1)
   dr = dr0 + ddr*dr
   drmin = (r1min-r0min)/(ntheta-1)
   rmin = r0min + drmin*rmin
   for k=0, nr-1 do     r(k,*) = rmin(*) + k*dr(*)
      
   x = r*cos(theta)
   y = r*sin(theta)
   phi= sin(theta)/r
   
   bx1=fltarr(ntheta) & bx3=bx1 & bx2=fltarr(nr) & bx4=bx2
   bx1=x(0,*)        & by1=y(0,*)
   bx2=x(*,ntheta-1) & by2=y(*,ntheta-1)
   bx3=x(nr-1,*)     & by3=y(nr-1,*)
   bx4=x(*,0)        & by4=y(*,0)
     

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
;        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

        xmax=max(x) & xmin=min(x) & dxpos=(xmax-xmin)
        ymax=max(y) & ymin=min(y) & dypos=(ymax-ymin)
       
        xpos=xmax + 0.02*dxpos
        ypos=ymin + 0.02*dypos   
        xa1 = 0.3 & xe1 = 0.7
        ylo = 0.2 & yup = 0.8

       
  print, 'plot  page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=2.0
       !P.FONT=3
;       !X.TICKS=2
;       !Y.TICKS=8
;       !Y.TICKlen=0.04
;       !X.THICK=2
;       !Y.THICK=2
       !P.THICK=1.
       !X.RANGE=[xmin,xmax]
       !Y.RANGE=[ymin,ymax]

	!P.POSITION=[xa1,ylo,xe1,yup]
	contour,phi,x,y,nlevels=23,$
        title=' Phi',xstyle=4,ystyle=4,$
        xtitle='x',ytitle='y'
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run 
;	xyouts,xpos,ypos3,'DEL A='
;	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)')

        axis, xax=0, -0.2,-0.2, xtitle='x'
        axis, yax=0, -0.2,-0.2, ytitle='y'

        !P.THICK=3.
        plots, bx1,by1,/data
        plots, bx2,by2,/data
        plots, bx3,by3,/data
        plots, bx4,by4,/data
        
  endif

     !P.FONT=3


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
     !P.THICK=1.
   endwhile

end


; MAIN PROGRAM
;   contour plots of irregularly shaped domains

   name='' & contin='' & again='y' & withps='n' & run='' & fnumber=1
   pi=3.141593

; GRID 
   nr = long(51)  &  ntheta = long(51) &  niter = long(51)  &  rms=0.1 

   openr, 8, 'fivol.bin',/F77_UNFORMATTED
   readu, 8,  nr,ntheta
   print, 'dimension nr=',nr,'     ntheta=',ntheta
   phi=fltarr(nr,ntheta) & x=phi & y=phi & phix=phi 
   readu, 8,  x,y
   readu, 8,  phix,phi
   readu, 8,  niter,rms
   close,8
   
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
        xa1 = 0.25 & xe1 = 0.75
        ylo1 = 0.6 & yup1 = 0.9
        ylo2 = 0.1 & yup2 = 0.4

       
  print, 'plot  page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; plot first page
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,5,0,0,0]
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

        nlevels=25  & lvec=fltarr(nlevels) & lvecmax=1./y(0,ntheta-1) 
        lvec(0)=lvecmax & for i=1,nlevels-1 do lvec(i)=0.75*lvec(i-1)
        lvec=lvec(sort(lvec))
        TRIANGULATE, X, Y, tri	
	!P.POSITION=[xa1,ylo1,xe1,yup1]
	contour,phix,x,y,levels=lvec, TRIANGULATION = tri,$
        title=' Phi_exact',xstyle=4,ystyle=4,$
        xtitle='x',ytitle='y'
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run 
;	xyouts,xpos,ypos3,'DEL A='
;	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)')

        PLOT, x, y, psym=1,xstyle=4,ystyle=4
        axis, xax=0, -0.2,-0.2, xtitle='x'
        axis, yax=0, -0.2,-0.2, ytitle='y'

        !P.THICK=3.
        plots, bx1,by1,/data
        plots, bx2,by2,/data
        plots, bx3,by3,/data
        plots, bx4,by4,/data
        
        !P.THICK=1.
	!P.POSITION=[xa1,ylo2,xe1,yup2]
	contour,phi,x,y,levels=lvec, $
        title=' Phi',xstyle=4,ystyle=4,$
        xtitle='x',ytitle='y'
;	xyouts,xpos,ypos0,'time'
;	xyouts,xpos,ypos1,' '+string(time,'(i3)')
;	xyouts,xpos,ypos2,run 
;	xyouts,xpos,ypos3,'DEL A='
;	xyouts,xpos,ypos4,'  '+string(delf,'(f6.2)')

        PLOT, x, y, psym=1,xstyle=4,ystyle=4
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


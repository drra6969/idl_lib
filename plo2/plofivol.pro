; MAIN PROGRAM
; contour plots of irregularly shaped domains

   name='' & contin='' & again='y' & withps='n' & run='' & fnumber=1
   pi=3.141593

; GRID 
   nr = long(51)  &  ntheta = long(51) &  niter = long(51)  &  rms=0.1 

; READ PARAMETERS 
   openr, 8, 'fivol.bin',/F77_UNFORMATTED
   readu, 8,  nr,ntheta
   print, 'dimension nr=',nr,'     ntheta=',ntheta
   phi=fltarr(nr,ntheta) & x=phi & y=phi & phix=phi & source=phi 
   readu, 8,  x,y
   readu, 8,  phix,phi
   readu, 8,  niter,rms,omega
   close,8
   
; BOUNDARIES FOR PLOTS
   bx1=fltarr(ntheta) & bx3=bx1 & bx2=fltarr(nr) & bx4=bx2
   bx1=x(0,*)         & by1=y(0,*)
   bx2=x(*,ntheta-1)  & by2=y(*,ntheta-1)
   bx3=x(nr-1,*)      & by3=y(nr-1,*)
   bx4=x(*,0)         & by4=y(*,0)
     

    print, 'Which case?'
    read, run

     while (again eq 'y') do begin

      !P.THICK=1.
; POSTSCIPT OUTPUT      
      print, 'With postscript?'
      read, withps
      if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='con.ps'
        !P.THICK=2.
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif

; COORDINATES OF PLOT AND WRITTEN OUTPUT
        xmax=max(x) & xmin=min(x) & dxpos=(xmax-xmin)
        ymax=max(y) & ymin=min(y) & dypos=(ymax-ymin)
       
        xpos=xmax - 0.1*dxpos
        ypos0=ymin + 0.6*dypos   
        ypos1=ymin + 0.7*dypos   
        ypos2=ymin + 0.8*dypos   
        ypos3=ymin + 0.9*dypos  
         
        xa1 = 0.1 & xe1 = 0.85
        ylo1 = 0.3 & yup1 = 0.9
        ylo2 = 0.07 & yup2 = 0.47

       
  print, 'plot  page? '
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; PLOT PAGE
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,3,0,0,0]
       !P.CHARSIZE=1.0
       !P.FONT=3
;       !X.TICKS=2
;       !Y.TICKS=8
;       !Y.TICKlen=0.04
       !X.THICK=2
       !Y.THICK=2
       !P.THICK=2.
       !X.RANGE=[xmin,xmax]
       !Y.RANGE=[ymin,ymax]

        nlevels=23  & nlhalf=(nlevels-1)/2 & lvec=findgen(nlevels) 
        dlvec1=(phix(nr-1,ntheta-1)-min(phix))/(nlhalf-1)
        lvec(0:nlhalf-1)=lvec(0:nlhalf-1)*dlvec1+min(phix) 
        
        dlvec2=( max(y(*,ntheta-1))-min(y(*,ntheta-1)))/(nlhalf+1) 
        lvec(nlhalf:nlevels-1)=$
           (lvec(nlhalf:nlevels-1)-nlhalf)*dlvec2+dlvec2
        lvec(nlhalf:nlevels-1)=1./lvec(nlhalf:nlevels-1) & lvec=lvec(sort(lvec))
	!P.POSITION=[xa1,ylo1,xe1,yup1]
	contour,phix,x,y,levels=lvec,$
        title=' PHI (Exact solution dotted)',$
        xstyle=4,ystyle=4,c_linestyle=1
	contour,phi,x,y,levels=lvec,xstyle=4,ystyle=4
	xyouts,xpos,ypos0,'No. iter.: '+string(niter,'(i3)')
	xyouts,xpos,ypos1,'Error: '+string(rms,'(f6.4)')
	xyouts,xpos,ypos2,'Omega: '+string(omega,'(f6.2)')
	xyouts,xpos,ypos3,$
	    'nr:'+string(nr,'(i2)')+'   ntheta:'+string(ntheta,'(i2)')

        PLOT, x, y, psym=1,xstyle=4,ystyle=4
        axis, xax=0, -0.05*dxpos,-0.1*dypos, xtitle='X'
        axis, yax=0, -0.05*dxpos,-0.1*dypos, ytitle='Y'

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


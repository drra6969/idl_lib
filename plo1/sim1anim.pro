



; DECLARE SOME PAPRAMETERS:
   ntmax=201 & nx=long(21) & run=''
   time=0.0 & error=0.0 & t=fltarr(ntmax) & rms=t 
; READ DATA
   openr, 8, 'sim1.bin',/F77_UNFORMATTED
   readu, 8, nx
   x=fltarr(nx,/NOZERO) & f=fltarr(nx,/NOZERO)& fex=f
   farr=fltarr(nx,ntmax,/NOZERO) & fexarr=farr
   readu, 8, x
   print, x
   readu, 8, c
   readu, 8, alpha
   print, c,alpha
   itime = 0
   while not eof(8)  do begin
     readu, 8, time, error, f, fex
     farr(*,itime) = f(*) &  t(itime) = time
     fexarr(*,itime) = fex(*) &  rms(itime) = error
     itot=itime
     if itime lt ntmax then itime = itime + 1 else stop, ' to many records '
   endwhile
   close, 8
;----PARAMETERS-------
   tmin = t(0)      &  xmin = x(1)
   tmax = t(itot-1) &  xmax = x(nx-2)
   print, 'tmin=',tmin, '   tmax=',tmax 
   print, 'xmin=',xmin, '   xmax=',xmax 
  
   print, 'Which case?' & read, run

   !P.REGION=[0.,0.,1.0,1.0]  &  !P.POSITION=[0.1,0.2,0.85,0.9] 
   !P.MULTI=[0,0,1]  &  !P.CHARSIZE=2.0   &  !P.FONT=3    &  !P.THICK=2.  
   !X.THICK=2        &  !Y.THICK=2        &  !X.TICKS=4   &  !Y.TICKS=4
   amax = max(farr(*,0:itot)) &  amin = min(farr(*,0:itot))
; OR EXPLICITLY
    amin=-0.5 & amax = 1.5
    print, 'amin=',amin,'  amax=',amax
    !X.RANGE=[xmin,xmax]       &  !Y.RANGE=[amin,amax]
    
    WINDOW, XSIZE=550, YSIZE=350, TITLE='Convection Equation'      
    xloadct  ;CHOCE OF COLORTABLE
    
; ACTUAL ANIMATION
    xanimate,set=[550,350,(itot+1),0]
    for i=0,itot do  begin
      erase
      plot, x, farr(*,i),$
          title='Temperature',xtitle='X',$
          xstyle=1, ystyle=1, line=2
      oplot, x, fexarr(*,i), line=0
       xpos=1.02*xmax
       ypos=amin+0.6*(amax-amin)
       ypos1=amin+0.5*(amax-amin)
       ypos2=amin+0.4*(amax-amin)
       xyouts,xpos,ypos,'time='+string(t(i),'(f5.2)')
       xyouts,xpos,ypos1,'rms='+string(rms(i),'(f6.3)')
       xyouts,xpos,ypos2,run
      xanimate,frame=i,window=!d.window
   endfor
  
end


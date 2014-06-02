; START OF MAIN PROGRAM

  ntmax=201 & nx=long(303) & ntf=7 & nxf=11

  name='' & contin='' & again='y' & withps='n' & run=''
  time=0.0 & error=0.0 & xtit='x' & change ='n'
  c=1.0  & alpha=1.0
  
  t=fltarr(ntmax) & rms=t & ioxf=fltarr(nxf)


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
    print,time
    farr(*,itime) = f(*) &  t(itime) = time
    fexarr(*,itime) = fex(*) &  rms(itime) = error
    itot=itime
    if itime lt ntmax then itime = itime + 1 else stop, ' to many records '
  endwhile
  close, 8

;----PARAMETER-------
  tmin = t(0)    &  xmin = x(0)
  tmax = t(itot) &  xmax = x(nx-1)
  print, 'tmin=',tmin, '   tmax=',tmax 
  print, 'xmin=',xmin, '   xmax=',xmax 
; CHANGE PARAMETERS:
  tmin = t(0)    &  xmin = x(1)
  tmax = t(itot-1) &  xmax = x(nx-2)
  
  print, 'Which case?'
  read, run
  fall=run
;  fall='run:'+run

  while again eq 'y' do begin

    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
        !P.THICK=2.
     	set_plot,'ps'
        device,filename='sat.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
      endif
    xp1=0.2 & xp2=0.7 
    mpl=float(indgen(5)) & yp1=0.83-0.19*mpl & yp2=0.95-0.19*mpl 
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,0,1]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=1.0  
      !P.FONT=3
      !X.THICK=2
      !Y.THICK=2
       !X.TICKS=6
      !Y.TICKS=3
;      !Y.TICKlen=0.04
      amax = max(farr(*,0:itot)) &  amin = min(farr(*,0:itot))
      amin=-0.0 & amax = 1.5
      print, 'amin=',amin,'  amax=',amax
      !X.RANGE=[xmin,xmax]
      !Y.RANGE=[amin,amax]
      
       !P.THICK=2.      
       !P.POSITION=[0.2,0.6,0.8,0.95] 
        plot, x, farr(*,itot),$
          title='1D Burgers Equation',xtitle='X',$
          xstyle=1, ystyle=1, line=0
        oplot, x, farr(*,0), line=0
       !P.THICK=1.      
        oplot, x, fexarr(*,itot), line=0
        for i=1,itot-1 do oplot, x, farr(*,i), line=2
         xpos=1.02*xmax
         ypos0=amin+0.8*(amax-amin)
         ypos=amin+0.7*(amax-amin)
         ypos1=amin+0.6*(amax-amin)
         ypos2=amin+0.5*(amax-amin)
         ypos3=amin+0.4*(amax-amin)
	 xyouts,xpos,ypos0,run
	 xyouts,xpos,ypos,'time='+string(t(i),'(f5.2)')
	 xyouts,xpos,ypos1,'rms='+string(rms(i),'(f6.3)')
	 xyouts,xpos,ypos2,'c='+string(c,'(f6.4)')
	 xyouts,xpos,ypos3,'alpha='+string(alpha,'(f6.4)')


    endif


    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then begin
       device,/close & !P.THICK=1. & set_plot,'x'
    endif
  endwhile
  
  
  
end


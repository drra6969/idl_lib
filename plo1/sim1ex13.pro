; START OF MAIN PROGRAM

  ntmax=201 & nx1=long(303) & nx2=long(303) & t=fltarr(ntmax)

  name='' & contin='' & again='y' & withps='n' & run=''
  time=0.0 & xtit='x' & change ='n'
  
  while again eq 'y' do begin

;  FIRST COARSE GRID:
    openr, 8, 'sim2.bin',/F77_UNFORMATTED
    readu, 8, nx1
    x1=fltarr(nx1,/NOZERO) & f1=fltarr(nx1,/NOZERO)
    farr1=fltarr(nx1,ntmax,/NOZERO)
    readu, 8, x1
    itime1 = 0
    while not eof(8)  do begin
      readu, 8, time, f1
      farr1(*,itime1) = f1(*) &  t(itime1) = time
      itot1=itime1
      print, 'itot1/time: ',itot1,time
      if itime1 lt ntmax then itime1 = itime1 + 1$
        else stop, ' to many records '
    endwhile
    close, 8
    print, 'itot1: ',itot1
    fcoarse=f1 & ffine=f1 & finter=f1 & fexact=f1
    fcoarse(*)=farr1(*,itot1-1) & fexact=farr1(*,itot1)
    

;  FINE GRID:
    openr, 8, 'sim3.bin',/F77_UNFORMATTED
    readu, 8, nx2
    x2=fltarr(nx2,/NOZERO) & f2=fltarr(nx2,/NOZERO)
    farr2=fltarr(nx2,ntmax,/NOZERO)
    readu, 8, x2
    itime2 = 0
    while not eof(8)  do begin
      readu, 8, time, f2
      farr2(*,itime2) = f2(*) &  t(itime2) = time
      itot2=itime2
      print, 'itot2/time: ',itot2,time
      if itime2 lt ntmax then itime2 = itime2 + 1$
        else stop, ' to many records '
    endwhile
    close, 8
    print, 'itot2: ',itot2
    
    if (itime1 ne itime2) then $
        print, 'WARNING time indices it1,it2 not the same!!!!! ',$
               itime1, itime2
    for j=0,nx1-1 do ffine(j)=farr2(2*j,itot2-1)
    finter=4./3.*ffine-1./3.*fcoarse
               
    sum1=0. & sum2=0.
    for j=0,nx1-1 do begin
      sum1 = sum1 + ( fexact(j)-ffine(j) )^2
      sum2 = sum2 + ( fexact(j)-finter(j) )^2
    endfor
    rms1 = sqrt(sum1/nx1)
    rms2 = sqrt(sum2/nx1)
    

;----PARAMETER-------
    tmin = t(0)    &  xmin = x1(0)
    tmax = t(itot1) &  xmax = x1(nx1-1)
    print, 'tmin=',tmin, '   tmax=',tmax 
    print, 'xmin=',xmin, '   xmax=',xmax 
; CHANGE PARAMETERS:
    tmin = t(0)    &  xmin = x1(0)
    tmax = t(itot1-1) &  xmax = x1(nx1-1)
  
    print, 'Which case?'
    read, run
    fall = run

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
    !P.MULTI=[0,0,4]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2.0  
      !P.FONT=3
      !P.THICK=1.      
      !X.THICK=2
      !Y.THICK=2
      !X.TICKS=4
      !Y.TICKS=4
      !P.POSITION=[0.2,0.55,0.7,0.9] 
;      !Y.TICKlen=0.04
      amax = max(farr1(*,0:itot1)) &  amin = min(farr1(*,0:itot1))
      print, 'amin=',amin,'  amax=',amax
      print, 'plotmin=',amin,'  plotmax=',amax, $
             '   Change? Enter y and desired values!'
      read, change
      if change eq 'y' then read, amin, amax
      !X.RANGE=[xmin,xmax]
      !Y.RANGE=[amin,amax]
      
      plot, x1, fexact(*),$
          title='Temperature',$
          xtitle='x', xstyle=1, ystyle=1, line=0
      !P.THICK=2.      
      oplot, x1, finter(*), line=1
      !P.THICK=1.      
      oplot, x1, fcoarse(*), line=2
      oplot, x1, ffine(*), line=3
        xpos=1.02*x1(nx1-1)
        ypos=amin+0.6*(amax-amin)
        ypos1=amin+0.4*(amax-amin)
        ypos2=amin+0.2*(amax-amin)
	xyouts,xpos,ypos,fall
	xyouts,xpos,ypos1,'rms  = '+string(rms1,'(f8.6)') 
	xyouts,xpos,ypos2,'rms_RE= '+string(rms2,'(f8.6)') 
    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then begin
       device,/close & !P.THICK=1. & set_plot,'x'
    endif
  endwhile  
  
  
end


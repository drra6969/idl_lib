; START OF MAIN PROGRAM

  ncases=3 & ntmax=41 & nxmax=41
  nx=lonarr(ncases)  & nxx=long(303) & sval=0.1 & dt=0.1 & tim=10.0 
  contin='' & again='y' & withps='n' & run='' & xtit='x'
  time=fltarr(ncases) & rms=fltarr(ncases) & svalue=fltarr(ncases) 
  delt=fltarr(ncases) & itot=intarr(ncases)
  t=fltarr(ntmax) 
  farrarr=fltarr(nxmax,ntmax,ncases) & xarr=fltarr(nxmax,ncases)
  posarr=fltarr(4,ncases)
  
  for j=0,ncases-1 do begin
    openr, 8, 'sim'+string((j+1),'(i1)')+'.bin',/F77_UNFORMATTED
    readu, 8, nxx  &  nx(j)=nxx
      x=fltarr(nx(j),/NOZERO) & f=fltarr(nx(j),/NOZERO)
      farr=fltarr(nx(j),ntmax,/NOZERO)
    readu, 8, x
    readu, 8, sval,dt  &  svalue(j)=sval & delt(j)=dt

    itime = 0
    while not eof(8)  do begin
      readu, 8, tim, f
      farr(*,itime) = f(*) &  t(itime) = tim
      itot(j)=itime & time(j)=tim
      if itime lt ntmax then itime = itime + 1 else stop, ' to many records '
    endwhile
    close, 8
    ferror=total( (farr(*,itot(j))-farr(*,itot(j)-1))^2 )
    rms(j)=sqrt(ferror/nx(j))
    
    xarr( 0:nx(j)-1, j)=x
    farrarr( 0:nx(j)-1, 0:itot(j), j) = farr( 0:nx(j)-1, 0:itot(j))
  endfor

;----PARAMETERS-------
  xmin = x(0) &  xmax = x(nx(ncases-1)-1)
  print, 'tmin=',t(0), '   tmax=',t(itot-1)
  print, 'xmin=',xmin, '   xmax=',xmax 
  amax = max(farrarr) &  amin = min(farrarr)
  print, 'amin=',amin,'  amax=',amax
  !X.RANGE=[xmin,xmax]
  !Y.RANGE=[amin,amax]
  !P.REGION=[0.,0.,1.0,1.0]
  !P.MULTI=[0,0,3]
  !P.CHARSIZE=1.0  
  !P.FONT=3
  !X.THICK=2
  !Y.THICK=2
  !X.TICKS=4
  !Y.TICKS=4
;  !Y.TICKlen=0.04
   dy=0.65/float(ncases) & dydist=0.2/float(ncases) & dysum=dy+dydist
  for j=0,ncases-1 do $
    posarr(0:3,j)=[0.2,0.1+j*dysum,0.7, 0.1+j*dysum+dy]
  
;  print, 'Which case?'
;  read, run
;  fall=run
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

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      
    for j=0,ncases-1 do begin
      farr=fltarr(nx(j),ntmax,/NOZERO)
      farr( 0:nx(j)-1, 0:itot(j))=farrarr( 0:nx(j)-1, 0:itot(j), j)
      x=fltarr(nx(j),/NOZERO) & x=xarr( 0:nx(j)-1, j)
      pos=posarr(*,j)
      pl1d, farr,nx(j),x,itot(j),amin,amax,time(j),delt(j),svalue(j),rms(j),pos,xtit
    endfor

    endif


    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then begin
       device,/close & !P.THICK=1. & set_plot,'x'
    endif
  endwhile
  
  
  
end


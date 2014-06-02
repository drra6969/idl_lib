  time = fltarr(10001,8)
  jp = fltarr(10001,8) & ep=jp
  xep=fltarr(10001,8) & yep=xep & zep=xep & xjp=xep & yjp=xep & zjp=xep
  itot = intarr(8)
  dumm = ''
  zeit=0.0
  r1=1.1 & r2=r1 & r3=r1 & r4=r1 & r5=r1 & r6=r1 & r7=r1 & r8=r1 
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1 & nop=1

  tnorm=5.7

  print, 'how many plots'
  read, nop
  
 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
;  openr, 2, 'magkhgrow'
  openr, 2, 'magkhgrow'+string(fnumber,'(i1)')

  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm

  i=0
  while not eof(2) do begin
    readf, 2, r1,r2,r3,r4,r5,r6,r7
    time(i,iop)=r1 & jp(i,iop)=r2 
    itot(iop)=i
    if i lt 10000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
 endfor
  
   print, 'Which case?'
   read, fall

   while again eq 'y' do begin

    print, 'With postscript?'
    read, withps
     if withps eq 'y' then begin 
      set_plot,'ps'
      device,filename='khp.ps'
      device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
      device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
      device,/times,/bold,font_index=3
     endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,2]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
     !P.CHARSIZE=1.2  
     !P.CHARTHICK=2.5
     !P.POSITION=[0.1,0.67,0.7,0.95]
     allmax = max(jp(1:10000,*),kax)
     print, kax, allmax
;     trange = max(time(*,0:(nop-1)))
     trange = 80.*tnorm
     plot, tnorm*time(2:itot(0),0), alog(jp(2:itot(0),0)),$
        title='KH Growth',xstyle=1,ystyle=1,$
        xtitle='time / s', xrange=[0,trange], yrange=[-5.0,1.0]
     for k=1,nop-1 do oplot,tnorm*time(2:itot(k),k),$
                            alog(jp(2:itot(k),k)), line=k 
     for k=0,nop-1 do oplot,tnorm*[69,75],$
                            alog(exp(1.5*[-1,-1]-0.7*[k,k])), line=k 
   endif
    xpos=0.78*trange
    ypos0=-1.6
    ypos1=-2.3
    ypos2=-3.0
    ypos3=-3.7
    ypos4=-4.4
	xyouts,xpos,ypos0,'25'
	xyouts,xpos,ypos1,'10'
	xyouts,xpos,ypos2,'20'
	xyouts,xpos,ypos3,'30'
	xyouts,xpos,ypos4,'35'
    xpost=0.7*trange
    ypost=-3
    xyouts,xpost,ypost,'!MP!3:',size=1.5
    xposy=0.05*trange
    yposy=0
    xyouts,xposy,yposy,'ln !7d!3v',size=1.5


    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
   endwhile

end  
  

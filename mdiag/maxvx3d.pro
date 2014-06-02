  time = fltarr(10001,8)
  jp = fltarr(10001,8) & ep=jp
  xep=fltarr(10001,8) & yep=xep & zep=xep & xjp=xep & yjp=xep & zjp=xep
  itot = intarr(8)
  dumm = ''
  zeit=0.0
  r1=1.1 & r2=r1 & r3=r1 & r4=r1 & r5=r1 & r6=r1 & r7=r1 & r8=r1 
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1 & nop=1

  print, 'how many plots'
  read, nop
  
 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
  openr, 2, 'magdmax'
;  openr, 2, 'magdmax'+string(fnumber,'(i1)')

  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm

  i=0
  while not eof(2) do begin
    readf, 2, zeit
    print, zeit
    time(i,iop)=zeit
    for k=0,3 do  readf, 2, format='(a80)', dumm
    readf,2,r1,r2,r3,r4,r5,r6 ,r7,r8
    for k=0,2 do  readf, 2, format='(a80)', dumm

    jp(i,iop)=r1-r5 
    if jp(i,iop) le 0.00001 then jp(i,iop)=0.00001
    for k=0,13 do  readf, 2, format='(a80)', dumm
    itot(iop)=i
    if i lt 3999 then i=i+1 else stop, ' to many records '
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
      device,filename='maxvx.ps'
      device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
      device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
      device,/times,/bold,font_index=3
     endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,4]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
     !P.CHARSIZE=2.0  
     !P.POSITION=[0.1,0.67,0.7,0.95]
     allmax = max(jp(1:10000,*),kax)
     print, kax, allmax
     trange = max(time(*,0:(nop-1)))
     plot, time(2:itot(0),0), alog(jp(2:itot(0),0)),$
        title='ln Vx - t',$
        xtitle='t', font=3, xrange=[0,trange], yrange=[-10,0]
;     for k=1,nop-1 do  oplot, time(2:itot(k),k), jp(2:itot(k),k), line=k
    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
   endwhile

end  
  

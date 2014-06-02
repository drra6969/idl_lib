  time = fltarr(1001,8)
  jp = fltarr(1001,8) & ep=jp
  xep=fltarr(1001,8) & yep=xep & zep=xep & xjp=xep & yjp=xep & zjp=xep
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
  openr, 2, 'magdmax'+string(fnumber,'(i2)')

  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm

  i=0
  while not eof(2) do begin
    readf, 2, zeit
;    print, zeit
    time(i,iop)=zeit
;  first B
    for k=0,3 do  readf, 2, format='(a80)', dumm
;  then V
    for k=0,3 do  readf, 2, format='(a80)', dumm
;  next J
    for k=0,3 do  readf, 2, format='(a80)', dumm
;  and E
    for k=0,3 do  readf, 2, format='(a80)', dumm
;  and J, E parallel
    readf, 2, r1,r2,r3,r4,r5,r6,r7,r8
    jp(i,iop)=r1 & xjp(i,iop)=r2 & yjp(i,iop)=r3 & zjp(i,iop)=r4 
    readf, 2, r1,r2,r3,r4,r5,r6,r7,r8
    ep(i,iop)=r1 & xep(i,iop)=r2 & yep(i,iop)=r3 & zep(i,iop)=r4 
;   print, r1,r2,r3,r4,r5,r6,r7,r8
;   and total pressure, pressure, and rho
;   readf, 2, r1,r2,r3,r4
    for k=0,2 do  readf, 2, format='(a80)', dumm
    
    itot(iop)=i
    if i lt 999 then i=i+1 else stop, ' to many records '
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
      device,filename='sat.ps'
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
     allmax = max(jp(1:1000,*),kax)
     print, kax, allmax
     trange = max(time(*,0:(nop-1)))
     plot, time(2:itot(0),0), jp(2:itot(0),0),$
        title='Maximum Field-Aligned Current Density',$
        xtitle='t', font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), jp(2:itot(k),k), line=k
     !P.POSITION=[0.1,0.45,0.7,0.6]
     allmax = max(xjp)
     plot, time(2:itot(0),0), xjp(2:itot(0),0),$
        title='X Pos of Max Field-Aligned Current Density',$
        font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), xjp(2:itot(k),k), line=k
     !P.POSITION=[0.1,0.25,0.7,0.4]
     allmax = max(yjp)
     plot, time(1:itot(0),0), yjp(2:itot(0),0),$
        title='Y Pos of Max Field-Aligned Current Density',$
        font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), yjp(2:itot(k),k), line=k
     !P.POSITION=[0.1,0.05,0.7,0.2]
     allmax = max(zjp)
     plot, time(2:itot(0),0), zjp(2:itot(0),0),$
        title='Z Pos of Max Field-Aligned Current Density',$
        xtitle='t', font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), zjp(2:itot(k),k), line=k
    endif

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
     !P.CHARSIZE=2.0  
     !P.POSITION=[0.1,0.67,0.7,0.95]
     allmax = max(ep)
     trange = max(time(*,0:(nop-1)))
     plot, time(2:itot(0),0), ep(2:itot(0),0),$
        title='Maximum Parallel Electric Field',$
        xtitle='t', font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), ep(2:itot(k),k), line=k
     !P.POSITION=[0.1,0.45,0.7,0.6]
     allmax = max(xep)
     plot, time(2:itot(0),0), xep(2:itot(0),0),$
        title='X Pos of Max Parallel Electric Field',$
        font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), xep(2:itot(k),k), line=k
     !P.POSITION=[0.1,0.25,0.7,0.4]
     allmax = max(yep)
     plot, time(2:itot(0),0), yep(2:itot(0),0),$
        title='Y Pos of Max Parallel Electric Field',$
        font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), yep(2:itot(k),k), line=k
     !P.POSITION=[0.1,0.05,0.7,0.2]
     allmax = max(zep)
     plot, time(2:itot(0),0), zep(2:itot(0),0),$
        title='Z Pos of Max Parallel Electric Field',$
        xtitle='t', font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do  oplot, time(2:itot(k),k), zep(2:itot(k),k), line=k
    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
   endwhile

end  
  

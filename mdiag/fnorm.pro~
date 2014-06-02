  time = fltarr(1001,8)
  fnorm = fltarr(1001,8) & fmax=fnorm & xfm=fnorm & yfm=fnorm
  itot = intarr(8)
  dumm = ''
  zeit=0.0 & r1=0.1 & r2=r1 & r3=r1 & r4=r1 & r5=r1
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1 & nop=1

;  print, 'how many plots'
;  read, nop
;   for iop=0, nop-1  do  begin 
;  print, 'what filenumber'
;  read, fnumber
;  openr, 2, 'magdmax'
  openr, 2, 'magsfnorm'
;+string(fnumber,'(i1)')

  i=0 & iop=0
  while not eof(2) do begin
;    readf, 2, zeit
;    print, zeit;
;    time(i,iop)=zeit
    readf, 2, r1,r2,r3,r4,r5
    time(i,iop)=r1 & fnorm(i,iop)=r2 & fmax(i,iop)=r3 
    xfm(i,iop)=r4 & yfm(i,iop)=r5 
    itot(iop)=i
    if i lt 999 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
  
   print, 'Which case?'
   read, fall

   while again eq 'y' do begin

    print, 'With postscript?'
    read, withps
     if withps eq 'y' then begin 
      set_plot,'ps'
      device,filename='fnorm.ps'
      device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
      device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
      device,/times,/bold,font_index=3
     endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,4]
    !P.thick=2

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
     !P.CHARSIZE=2.0  
     !P.POSITION=[0.1,0.67,0.7,0.95]
     trange = max(time(*,0:(nop-1)))
     plot, time(1:itot(0),0),alog10(fnorm(1:itot(0),0)),$
        title='Integr. Forces',thick=2,$
        xtitle='t', font=3, xrange=[0,trange], yrange=[0., 5.]
     oplot, time(1:itot(0),0),alog10(fmax(1:itot(0),0)), line=2,thick=2
;     for k=1,nop-1 do  oplot, time(2:itot(k),k), jp(2:itot(k),k), line=k
    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
   endwhile

end  
  

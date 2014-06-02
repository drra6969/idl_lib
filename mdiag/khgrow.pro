  t = fltarr(10001)
  bn=t & width=t & q=t & qt=t & qn=t & dq=t

  dumm4 = strarr(4)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''

  openr, 2, 'magkhgrow'
  readf, 2, dumm3
  print, dumm3

  i=0
  while not eof(2) do begin
    readf, 2, ts, bns, ws, qs, qts, qns, dqs

    t(i)=ts & bn(i)=bns & width(i)=ws & q(i)=qs  
    qt(i)=qts & qn(i)=qns & dq(i)=dqs  

    itot=i
    if i lt 10000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
  ilnb=0
  while  ( bn(ilnb) eq 0 ) do ilnb=ilnb+1 
  for i=ilnb, itot do    if bn(i) eq 0.0 then bn(i)=bn(i-1)

     print, 'Which case?'
     read, fall

     while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='khgrow.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]
  xmin=0.0
  xmax=150.0

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=2.0  
    !P.MULTI=[0,0,4]
    !P.POSITION=[0.15,0.05,0.7,0.23]
    plot, t(ilnb:itot), alog(bn(ilnb:itot)),$
      title='Log Vn', font=3,xstyle=1,ystyle=1,$
;      xtitle='time',yrange=[min(alog(bn(ilnb:itot))/alog(10.0)), 0.0],$ 
      xtitle='time',yrange=[-10.0, 0.0],$ 
      xrange=[xmin,xmax]


  endif


     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end





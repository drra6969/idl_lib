  time = fltarr(10001,8)
  jp = fltarr(10001,8) & ep=jp
  xep=fltarr(10001,8) & yep=xep & zep=xep & xjp=xep & yjp=xep & zjp=xep
  itot = intarr(8)
  dumm = ''
  zeit=0.0
  r1=1.1 & r2=r1 & r3=r1 & r4=r1 & r5=r1 & r6=r1 & r7=r1 & r8=r1 
  r9=r1 & r10=r1 & resist1=r1 & resist2=r1
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1 & nop=1 & co=1

  print, 'how many plots'
  read, nop
  
 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
;  openr, 2, 'ftn50'
  openr, 2, 'ftn50'+string(fnumber,'(i1)')
  if iop eq 0 then co=-1
  if iop eq 1 then co=1
  if iop eq 2 then co=-1
  i=0
  r6=0
  rs=0.0
  rt=0.0
  while not eof(2) do begin
  readf, 2, format='(2f10.6)',r1,r2
  resist1=r1
  resist2=r2
  readf, 2, format='(5f11.5)',r1,r2,r3,r4,r5
  readf, 2, format='(5f11.5)',r6,r7,r8,r9,r10  

    time(i,0)=r1

    dt=0
    if i gt 0 then begin
     
    dt=time(i,0)-time(i-1,0)
      endif
 ;   rs=rs+(resist1*r9-resist2*r10)*dt
    jp(i,iop)=co*(resist1*r9-resist2*r10)
    zjp(i,iop)=abs(r7)
    itot(0)=i
    yep(i,iop)=r2
    if i lt 13999 then i=i+1 else stop, ' to many records '
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
      device,filename='rh.ps'
      device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
      device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
      device,/times,/bold,font_index=3
     endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,4]

     !P.CHARSIZE=2.0  
     !P.POSITION=[0.1,0.75,0.7,0.95]
     allmax = max(jp(1:10000),kax)
     print, kax, allmax
     trange = 130 
;     allmax=max(jp)  
     allmax=max(yep)
     plot, time(2:itot(0),0), yep(2:itot(0),0),$
        title='Max Current Density', xstyle=1,ystyle=1,$
            xtitle='t', font=3, xrange=[0,trange], yrange=[0,allmax]
      for iop=0,nop-1 do begin
     xjp(2,iop)=trange/20
     xjp(3,iop)=trange/20*3
     yjp(2,iop)=allmax*9/10-allmax*iop/15
     yjp(3,iop)=allmax*9/10-allmax*iop/15
      endfor
      for k=1,nop-1 do oplot, time(2:itot(0),0), yep(2:itot(0),k), line=k
      for k=0,nop-1 do oplot, xjp(2:3,k),yjp(2:3,k),line=k
      xyouts,xjp(2,0)+trange/20*8,yjp(2,0), 'V!I0!N=1.5'

      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,0), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,1), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,2), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,3), 'S='

     !P.POSITION=[0.1,0.50,0.7,0.70]
     allmax = max(jp(1:10000),kax)
     print, kax, allmax
     trange = 130 
;     allmax=max(jp)  
     allmax=0.035
     plot, time(2:itot(0),0), jp(2:itot(0),0),$
        title='Reconnection Rate', xstyle=1,ystyle=1,$
            xtitle='t', font=3, xrange=[0,trange], yrange=[0,allmax]
      for iop=0,nop-1 do begin
     xjp(2,iop)=trange/20
     xjp(3,iop)=trange/20*3
     yjp(2,iop)=allmax*9/10-allmax*iop/15
     yjp(3,iop)=allmax*9/10-allmax*iop/15
      endfor
      for k=1,nop-1 do oplot, time(2:itot(0),0), jp(2:itot(0),k), line=k
      for k=0,nop-1 do oplot, xjp(2:3,k),yjp(2:3,k),line=k
      xyouts,xjp(2,0)+trange/20*8,yjp(2,0), 'V!I0!N=1.5'

      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,0), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,1), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,2), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,3), 'S='

     !P.POSITION=[0.1,0.25,0.7,0.45]
     allmax=1.0
     plot, time(1:itot(0),0), zjp(2:itot(0),0),$
        title='Reconnected Flux ',xstyle=1,ystyle=1,$
        xtitle='t', font=3, xrange=[0,trange], yrange=[0, allmax]
     for k=1,nop-1 do oplot, time(2:itot(0),0), zjp(2:itot(0),k), line=k
      for iop=0,nop-1 do begin
     xjp(2,iop)=trange/20
     xjp(3,iop)=trange/20*3
     yjp(2,iop)=allmax*9/10-allmax*iop/15
     yjp(3,iop)=allmax*9/10-allmax*iop/15
      endfor
      for k=0,nop-1 do oplot, xjp(2:3,k),yjp(2:3,k),line=k
      xyouts,xjp(2,0)+trange/20*8,yjp(2,0), 'V!I0!N=1.5'

      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,0), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,1), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,2), 'S='
      xyouts,xjp(2,0)+trange/20*3.05,yjp(2,3), 'S='

  
    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
   endwhile

end  
  

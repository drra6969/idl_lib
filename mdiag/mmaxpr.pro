  time = fltarr(1001,7)
  jp = fltarr(1001,7) & bx1=jp & bx2=jp & ptot=jp & pthe=jp
  xb1=jp & yb1=jp & xb2=jp & yb2=jp & yvx=jp & bamp=jp
  itot = intarr(7) & itmax = intarr(7)
  dumm = ''
  zeit=0.0
  r1=1.1 & r2=r1 & r3=r1 & r4=r1 & r5=r1 & r6=r1 & r7=r1 & r8=r1 
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1 & nop=1 & cas=strarr(7) & incase='1'
  for k=0, 6 do cas(k)=string((k+1),'(i1)')

     tmin=0
     tmax=180
     trange=tmax-tmin
     for iop=0, nop-1  do itmax(iop)=0
     
  print, 'how many plots'
  read, nop
  nop1=nop-1

 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
  print, 'what case'
  read, incase
  if (incase ne '') then cas(iop)=incase
  
  openr, 2, 'magdmax'+string(fnumber,'(i1)')

  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm
  readf, 2, format='(a80)', dumm

  i=0
  while not eof(2) do begin
    readf, 2, zeit
;    print, zeit
    time(i,iop)=zeit & if zeit le tmax then itmax(iop)=i
;  first Bx
    readf, 2, r1,r2,r3,r4,r5,r6
    bx1(i,iop)=r1 & xb1(i,iop)=r2 & yb1(i,iop)=r3 
    bx2(i,iop)=r4 & xb2(i,iop)=r5 & yb2(i,iop)=r6 
    bamp(i,iop)=0.5*(r1-r4)
    for k=0,1 do  readf, 2, format='(a80)', dumm
    readf, 2, r1,r2,r3
    pthe(i,iop)=r1 
;  then V
    readf, 2, r1,r2,r3,r4,r5,r6
    yvx(i,iop)=r3 
    for k=0,2 do  readf, 2, format='(a80)', dumm
;  next J
    for k=0,3 do  readf, 2, format='(a80)', dumm
;  and E
    for k=0,3 do  readf, 2, format='(a80)', dumm
;  and J, E parallel
    for k=0,1 do  readf, 2, format='(a80)', dumm
;   print, r1,r2,r3,r4,r5,r6,r7,r8
;   and total pressure, pressure, and rho
    readf, 2, r1,r2,r3
    ptot(i,iop)=r1 
    readf, 2, r1,r2,r3
    pthe(i,iop)=r1 
;    readf, 2, format='(a80)', dumm
    readf, 2, format='(a80)', dumm
    
    itot(iop)=i
    if i lt 999 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
  
  if iop eq 1 then begin 
  a=where(bamp(0:itot(iop),iop) lt 0.25, count)
  itot(iop)=count-1
  endif
  a=where(time(0:itot(iop),iop) lt tmax, count)
  itot(iop)=count-1
  
 endfor
 
  
   while again eq 'y' do begin

    dx=0.5 & dy=0.23 & dyint=0.1
    x1=0.2 & x2=x1+dx
    y11=0.08 & y21=y11+dy & y12=y21+dyint & y22=y12+dy
    y13=y22+dyint & y23=y13+dy 
    
    print, 'With postscript?'
    read, withps
     if withps eq 'y' then begin 
      !P.THICK=2
      set_plot,'ps'
      device,filename='sat.ps'
      device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
      device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
      device,/times,/bold,font_index=3
     endif

    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,4]
    !P.CHARSIZE=2.0  
    !X.THICK=2
    !Y.THICK=2

    print, 'plot page?'
    

     !P.POSITION=[x1, y13, x2, y23]
     ymin=min(yb2(*,0:nop1))
     ymax=max(yb2(*,0:nop1))
     plot, time(1:itot(0),0), yb2(1:itot(0),0),$
        title='Z Locations of BN Min',$
        xtitle='time', font=3,$
        xrange=[tmin,tmax], yrange=[ymin,ymax], $
        xstyle=1,ystyle=1
     for k=1,nop1 do  oplot, time(1:itot(k),k), yb2(1:itot(k),k), line=k
      if (tmax-time(itot(0),0)) lt 0.05*trange then $
          xpos=tmax+0.02*trange    else $
          xpos=time(itot(0),0)+0.01*trange
      ypos=yb2(itot(0),0) & xyouts,xpos,ypos,cas(0),font=3
      print, itot(0), time(itot(0),0), ypos
      for k=1,nop1 do begin 
        if (tmax-time(itot(k),k)) lt 0.05*trange then $
          xpos=tmax+0.04*trange    else $
          xpos=time(itot(k),k)+0.02*trange
          if (k eq 4) then   xpos=tmax+0.07*trange
        ypos=yb2(itot(k),k) & xyouts,xpos,ypos,cas(k),font=3
        print, itot(k), time(itot(k),k), ypos
      endfor

     !P.POSITION=[x1, y11, x2, y21]
     plot, time(1:itot(0),0), (pthe(1:itot(0),0)-1.0),$ 
        title='Pressure Perturbation',font=3, $ 
;        title='Pressure',font=3, $ 
;        xrange=[tmin,tmax], yrange=[0, max(pthe(*,0:nop1)-1.0)], $
        xrange=[tmin,tmax], yrange=[0, 2.0], $
        xstyle=1,ystyle=1
     for k=1,nop1 do $
        oplot, time(1:itot(k),k), (pthe(1:itot(k),k)-1.0), line=k
;     if (k eq 3) then $
;          oplot, time(1:itot(k),k), (pthe(1:itot(k),k)-0.5), line=k $
;        else oplot, time(1:itot(k),k), (pthe(1:itot(k),k)-1.0), line=k
      if (tmax-time(itot(0),0)) lt 0.05*trange then $
          xpos=tmax+0.01*trange    else $
          xpos=time(itot(0),0)+0.01*trange
      ypos=pthe(itot(0),0)-1.0 & xyouts,xpos,ypos,cas(0),font=3
      print, itot(0), time(itot(0),0), ypos
      for k=1,nop1 do begin 
        if (tmax-time(itot(k),k)) lt 0.05*trange then $
          xpos=tmax+0.05*trange    else $
          xpos=time(itot(k),k)+0.01*trange
        ypos=pthe(itot(k),k)-1.0
        xyouts,xpos,ypos,cas(k),font=3
        print, itot(k), time(itot(k),k), ypos
      endfor
      
     !P.POSITION=[x1, y12, x2, y22]
     plot, time(1:itot(0),0), bamp(1:itot(0),0),$
         title='BN Amplitude',$
         xtitle='time', font=3, $
         xrange=[tmin,tmax], yrange=[0,0.5],$
;        yrange=[0,max(bamp(*,0:nop1))], $
         xstyle=1,ystyle=1
     for k=1,nop1 do  oplot, time(1:itot(k),k), bamp(1:itot(k),k), line=k
      if (tmax-time(itot(0),0)) lt 0.05*trange then $
          xpos=tmax+0.02*trange    else $
          xpos=time(itot(0),0)+0.02*trange
      ypos=bamp(itot(0),0) & xyouts,xpos,ypos,cas(0),font=3
      print, itot(0), time(itot(0),0), ypos
      for k=1,nop1 do begin 
        if (tmax-time(itot(k),k)) lt 0.05*trange then $
          xpos=tmax+0.04*trange    else $
          xpos=time(itot(k),k)+0.02*trange
        ypos=bamp(itot(k),k) 
        if (k eq 4) then   xpos=tmax+0.07*trange
        xyouts,xpos,ypos,cas(k),font=3
        print, itot(k), time(itot(k),k), ypos
      endfor
;      if (tmax-time(itot(0),0)) lt 0.05*trange then $
;          xpos=tmax+0.04*trange    else $
;          xpos=time(itot(0),0)+0.02*trange
;      ypos=bamp(itot(0),0) & xyouts,xpos,ypos,cas(0),font=3
;      print, itot(0), time(itot(0),0), ypos
;      for k=1,nop1 do begin 
;        if (tmax-time(itot(k),k)) lt 0.05*trange then $
;          xpos=tmax+0.04*trange    else $
;          xpos=time(itot(k),k)+0.02*trange
;        ypos=bamp(itot(k),k) & xyouts,xpos,ypos,cas(k),font=3
;        print, itot(k), time(itot(k),k), ypos
;      endfor
      

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then device,/close
    set_plot,'x'
    !P.THICK=1
   endwhile

end  
  

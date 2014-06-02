  time = fltarr(501)
  p = fltarr(501,5)
  xsat=fltarr(5) & zsat=xsat
  bl=p & bm=p & bn=p & vl=p & vm=p & vn=p  
  rho=p & ptot=p & b2=p 
  head = strarr(5)
  dumm7 = strarr(7)
  dumm = ''
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1
; remark number of satellites changes dumm7 and head dimension
  print, 'what filenumber'
  read, fnumber

  openr, 2, 'sat'+string(fnumber,'(i1)')
  readf, 2, format='(a80)', dumm
  print, dumm
  readf, 2, format='(a80)', dumm
  print, dumm
  readf, 2, format='(a80)', dumm
  print, dumm
  for i=0,4 do begin 
    readf, 2, format='(a7,f7.2,a3,f7.2)', dumm,xs,dumm,ys
    xsat(i)=xs & zsat(i)=ys
    print, dumm,'xsat=',xsat(i),'   zsat=',zsat(i)
  endfor
  readf, 2, dumm7
  print, dumm7

  i=0
  while not eof(2) do begin
   for k=0,4 do begin
    readf, 2, times,bls,bms,bns,vls,vms,vns,rhos,ps,bs,ptots
    time(i)=times & bl(i,k)=bls & bm(i,k)=bms & bn(i,k)=bns
    vl(i,k)=vls & vm(i,k)=vms & vn(i,k)=vns
    rho(i,k)=rhos & p(i,k)=ps & b2(i,k)=bs*bs & ptot(i,k)=ptots
   endfor

    itot=i
    if i lt 499 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2

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

for i=0,4 do begin
 print, 'plot page?'
 read, contin
 if (contin eq '' or contin eq 'y') then begin
  !P.CHARSIZE=2.0  
  !P.POSITION=[0.1,0.05,0.7,0.23]
  allmax = [[max(p(1:itot,i))], [max(ptot(1:itot,i))], [max(b2(1:itot,i))]]
  allmin = [[min(p(1:itot,i))], [min(ptot(1:itot,i))], [min(b2(1:itot,i))]]
  plot, time(1:itot), ptot(1:itot,i),$
        title='therm., magn., and total Pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  oplot, time(1:itot), p(1:itot,i), line=1
  oplot, time(1:itot), b2(1:itot,i), line=2

  !P.POSITION=[0.1,0.3,0.7,0.48]
  plot, time(1:itot), rho(1:itot,i), title='Density', font=3,$ 
        yrange=[min(rho(1:itot,i)), max(rho(1:itot,i))]

  !P.POSITION=[0.1,0.55,0.7,0.73]
  allmax = [[max(vl(1:itot,i))], [max(vm(1:itot,i))], [max(vn(1:itot,i))]]
  allmin = [[min(vl(1:itot,i))], [min(vm(1:itot,i))], [min(vn(1:itot,i))]]
  plot, time(1:itot), vl(1:itot,i),$
        title='Bulk Velocity', font=3, yrange=[min(allmin), max(allmax)]
  oplot, time(1:itot), vm(1:itot,i), line=1
  oplot, time(1:itot), vn(1:itot,i), line=2

  !P.POSITION=[0.1,0.80,0.7,0.98]
  blmax = max(bl(1:itot,i)) &  blmin = min(bl(1:itot,i))
  bmmax = max(bm(1:itot,i)) &  bmmin = min(bm(1:itot,i))
  bnmax = max(bn(1:itot,i)) &  bnmin = min(bn(1:itot,i))
  print, 'blmin=',blmin,'  blmax=',blmax
  print, 'bmmin=',bmmin,'  bmmax=',bmmax
  print, 'bnmin=',bnmin,'  bnmax=',bnmax
  fmin=bnmin-0.2*(bnmax-bnmin) & fmax=bnmax+0.2*(bnmax-bnmin)
  print, 'plotmin=',fmin,'  plotmax=',fmax, $
         '   Change? Enter y and desired values!'
  read, change
  if change eq 'y' then read, fmin, fmax
  plot, time(1:itot), bl(1:itot,i),$
        title='Magnetic Field Components', font=3, yrange=[fmin,fmax]
  oplot, time(1:itot), bm(1:itot,i), line=1
  oplot, time(1:itot), bn(1:itot,i), line=2
    xpos=time(itot)+0.05*time(itot)
    ypos=fmin+0.8*(fmax-fmin)
    xyouts,xpos,ypos,fall,font=3
    ypos=fmin+0.5*(fmax-fmin)
    xyouts,xpos,ypos,'x='+string(xsat(i),'(f4.1)'),font=3
    ypos=fmin+0.3*(fmax-fmin)
    xyouts,xpos,ypos,'z='+string(zsat(i),'(f4.1)'),font=3
 endif
endfor

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile

end

  read, dumm
;second page
  !P.POSITION=[0.1,0.05,0.75,0.25]
  allmax = [[max(p(1:itot,1))], [max(ptot(1:itot,1))], [max(b2(1:itot,1))]]
  allmin = [[min(p(1:itot,1))], [min(ptot(1:itot,1))], [min(b2(1:itot,1))]]
  plot, time(1:itot), ptot(1:itot,1),$
        title='therm., magn., and total Pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  oplot, time(1:itot), p(1:itot,1), line=1
  oplot, time(1:itot), b2(1:itot,1), line=2

  !P.POSITION=[0.1,0.3,0.75,0.5]
  plot, time(1:itot), rho(1:itot,1),$
        title='Density', font=3,$ 
        yrange=[min(rho(1:itot,1)), max(rho(1:itot,1))]

  !P.POSITION=[0.1,0.55,0.75,0.75]
  plot, time(1:itot), vl(1:itot,1),$
        title='Bulk Velocity', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), vm(1:itot,1), line=1
  oplot, time(1:itot), vn(1:itot,1), line=2

  !P.POSITION=[0.1,0.80,0.75,1.0]
  plot, time(1:itot), bl(1:itot,1),$
        title='Magnetic Field Components', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), bm(1:itot,1), line=1
  oplot, time(1:itot), bn(1:itot,1), line=2

  read, dumm

;third page
  !P.POSITION=[0.1,0.05,0.75,0.25]
  allmax = [[max(p(1:itot,2))], [max(ptot(1:itot,2))], [max(b2(1:itot,2))]]
  allmin = [[min(p(1:itot,2))], [min(ptot(1:itot,2))], [min(b2(1:itot,2))]]
  plot, time(1:itot), ptot(1:itot,2),$
        title='therm., magn., and total Pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  oplot, time(1:itot), p(1:itot,2), line=2
  oplot, time(1:itot), b2(1:itot,2), line=1

  !P.POSITION=[0.1,0.3,0.75,0.5]
  plot, time(1:itot), rho(1:itot,2),$
        title='Density', font=3,$ 
        yrange=[min(rho(1:itot,2)), max(rho(1:itot,2))]

  !P.POSITION=[0.1,0.55,0.75,0.75]
  plot, time(1:itot), vl(1:itot,2),$
        title='Bulk Velocity', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), vm(1:itot,2), line=2
  oplot, time(1:itot), vn(1:itot,2), line=1

  !P.POSITION=[0.1,0.80,0.75,1.0]
  plot, time(1:itot), bl(1:itot,2),$
        title='Magnetic Field Components', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), bm(1:itot,2), line=1
  oplot, time(1:itot), bn(1:itot,2), line=2

  read, dumm

;fourth page
  !P.POSITION=[0.1,0.05,0.75,0.25]
  allmax = [[max(p(1:itot,3))], [max(ptot(1:itot,3))], [max(b2(1:itot,3))]]
  allmin = [[min(p(1:itot,3))], [min(ptot(1:itot,3))], [min(b2(1:itot,3))]]
  plot, time(1:itot), ptot(1:itot,3),$
        title='therm., magn., and total Pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  oplot, time(1:itot), p(1:itot,3), line=2
  oplot, time(1:itot), b2(1:itot,3), line=1

  !P.POSITION=[0.1,0.3,0.75,0.5]
  plot, time(1:itot), rho(1:itot,3),$
        title='Density', font=3,$ 
        yrange=[min(rho(1:itot,3)), max(rho(1:itot,3))]

  !P.POSITION=[0.1,0.55,0.75,0.75]
  plot, time(1:itot), vl(1:itot,3),$
        title='Bulk Velocity', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), vm(1:itot,3), line=2
  oplot, time(1:itot), vn(1:itot,3), line=1

  !P.POSITION=[0.1,0.80,0.75,1.0]
  plot, time(1:itot), bl(1:itot,3),$
        title='Magnetic Field Components', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), bm(1:itot,3), line=1
  oplot, time(1:itot), bn(1:itot,3), line=2


  read, dumm

;fifth page
  !P.POSITION=[0.1,0.05,0.75,0.25]
  allmax = [[max(p(1:itot,4))], [max(ptot(1:itot,4))], [max(b2(1:itot,4))]]
  allmin = [[min(p(1:itot,4))], [min(ptot(1:itot,4))], [min(b2(1:itot,4))]]
  plot, time(1:itot), ptot(1:itot,4),$
        title='therm., magn., and total Pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  oplot, time(1:itot), p(1:itot,4), line=2
  oplot, time(1:itot), b2(1:itot,4), line=1

  !P.POSITION=[0.1,0.3,0.75,0.5]
  plot, time(1:itot), rho(1:itot,4),$
        title='Density', font=3,$ 
        yrange=[min(rho(1:itot,4)), max(rho(1:itot,4))]

  !P.POSITION=[0.1,0.55,0.75,0.75]
  plot, time(1:itot), vl(1:itot,4),$
        title='Bulk Velocity', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), vm(1:itot,4), line=2
  oplot, time(1:itot), vn(1:itot,4), line=1

  !P.POSITION=[0.1,0.80,0.75,1.0]
  plot, time(1:itot), bl(1:itot,4),$
        title='Magnetic Field Components', font=3, yrange=[-.2,.2]
  oplot, time(1:itot), bm(1:itot,4), line=1
  oplot, time(1:itot), bn(1:itot,4), line=2


end

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




for i=0,4 do begin
 !P.MULTI=[0,1,4,0,1]
 print, 'plot page?'
 read, contin
 if (contin eq '' or contin eq 'y') then begin

  allmax = [[max(p(1:itot,i))], [max(ptot(1:itot,i))], [max(b2(1:itot,i))]]
  allmin = [[min(p(1:itot,i))], [min(ptot(1:itot,i))], [min(b2(1:itot,i))]]
  plot, time(1:itot), ptot(1:itot,i),$
        title='Total pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), p(1:itot,i),$
	title='Thermal pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), b2(1:itot,i),$
	title='Magnetic pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), rho(1:itot,i), title='Density', font=3,$ 
        yrange=[min(rho(1:itot,i)), max(rho(1:itot,i))],$
	xtitle='t'

  print, 'Continue with remaining plots? (y or n)'
  read, change
  if change eq '' or change eq 'y' then !P.MULTI=[0,2,3,0,1]

  allmax = [[max(vl(1:itot,i))], [max(vm(1:itot,i))], [max(vn(1:itot,i))]]
  allmin = [[min(vl(1:itot,i))], [min(vm(1:itot,i))], [min(vn(1:itot,i))]]
  plot, time(1:itot), vl(1:itot,i),$
	xtitle='t',$
        title='Vl', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vm(1:itot,i),$
	xtitle='t',$
	title='Vm', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vn(1:itot,i),$
	xtitle='t',$
	title='Vn', font=3, yrange=[min(allmin), max(allmax)]

  blmax = max(bl(1:itot,i)) &  blmin = min(bl(1:itot,i))
  bmmax = max(bm(1:itot,i)) &  bmmin = min(bm(1:itot,i))
  bnmax = max(bn(1:itot,i)) &  bnmin = min(bn(1:itot,i))
  newmax =max([blmax, bmmax, bnmax])
  newmin = min([blmin, bmmin, bnmin])

  fmin=newmin & fmax=newmax

  plot, time(1:itot), bl(1:itot,i),$
	xtitle='t',$
        title='Bl', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bm(1:itot,i),$
	xtitle='t',$
	title='Bm', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bn(1:itot,i),$
	xtitle='t',$
	title='Bn', font=3, yrange=[fmin, fmax]
    xpos=time(itot)+0.05*time(itot)
    ypos=fmin+0.8*(fmax-fmin)
    xyouts,1250,450,fall,font=3,charsize=0.5, /device
    ypos=fmin+0.5*(fmax-fmin)
    xyouts,1250,365,'x='+string(xsat(i),'(f4.1)'),font=3,charsize=0.5, /device
    ypos=fmin+0.3*(fmax-fmin)
    xyouts,1250,280,'z='+string(zsat(i),'(f4.1)'),font=3,charsize=0.5, /device
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
  !P.MULTI=[0,1,4,0,1]
  allmax = [[max(p(1:itot,i))], [max(ptot(1:itot,i))], [max(b2(1:itot,i))]]
  allmin = [[min(p(1:itot,i))], [min(ptot(1:itot,i))], [min(b2(1:itot,i))]]
  plot, time(1:itot), ptot(1:itot,i),$
        title='Total pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), p(1:itot,i),$
	title='Thermal pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), b2(1:itot,i),$
	title='Magnetic pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), rho(1:itot,i), title='Density', font=3,$ 
        yrange=[min(rho(1:itot,i)), max(rho(1:itot,i))],$
	xtitle='t'

  print, 'Continue with remaining plots? (y or n)'
  read, change
  if change eq '' or change eq 'y' then !P.MULTI=[0,2,3,0,1]

  allmax = [[max(vl(1:itot,i))], [max(vm(1:itot,i))], [max(vn(1:itot,i))]]
  allmin = [[min(vl(1:itot,i))], [min(vm(1:itot,i))], [min(vn(1:itot,i))]]
  plot, time(1:itot), vl(1:itot,i),$
	xtitle='t',$
        title='Vl', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vm(1:itot,i),$
	xtitle='t',$
	title='Vm', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vn(1:itot,i),$
	xtitle='t',$
	title='Vn', font=3, yrange=[min(allmin), max(allmax)]

  blmax = max(bl(1:itot,i)) &  blmin = min(bl(1:itot,i))
  bmmax = max(bm(1:itot,i)) &  bmmin = min(bm(1:itot,i))
  bnmax = max(bn(1:itot,i)) &  bnmin = min(bn(1:itot,i))
  newmax =max([blmax, bmmax, bnmax])
  newmin = min([blmin, bmmin, bnmin])

  fmin=newmin & fmax=newmax
  plot, time(1:itot), bl(1:itot,i),$
	xtitle='t',$
        title='Bl', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bm(1:itot,i),$
	xtitle='t',$
	title='Bm', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bn(1:itot,i),$
	xtitle='t',$
	title='Bn', font=3, yrange=[fmin, fmax]

  read, dumm

;third page
 !P.MULTI=[0,1,4,0,1]
  allmax = [[max(p(1:itot,i))], [max(ptot(1:itot,i))], [max(b2(1:itot,i))]]
  allmin = [[min(p(1:itot,i))], [min(ptot(1:itot,i))], [min(b2(1:itot,i))]]
  plot, time(1:itot), ptot(1:itot,i),$
        title='Total pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), p(1:itot,i),$
	title='Thermal pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), b2(1:itot,i),$
	title='Magnetic pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), rho(1:itot,i), title='Density', font=3,$ 
        yrange=[min(rho(1:itot,i)), max(rho(1:itot,i))],$
	xtitle='t'

  print, 'Continue with remaining plots? (y or n)'
  read, change
  if change eq '' or change eq 'y' then !P.MULTI=[0,2,3,0,1]

  allmax = [[max(vl(1:itot,i))], [max(vm(1:itot,i))], [max(vn(1:itot,i))]]
  allmin = [[min(vl(1:itot,i))], [min(vm(1:itot,i))], [min(vn(1:itot,i))]]
  plot, time(1:itot), vl(1:itot,i),$
	xtitle='t',$
        title='Vl', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vm(1:itot,i),$
	xtitle='t',$
	title='Vm', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vn(1:itot,i),$
	xtitle='t',$
	title='Vn', font=3, yrange=[min(allmin), max(allmax)]

  blmax = max(bl(1:itot,i)) &  blmin = min(bl(1:itot,i))
  bmmax = max(bm(1:itot,i)) &  bmmin = min(bm(1:itot,i))
  bnmax = max(bn(1:itot,i)) &  bnmin = min(bn(1:itot,i))
  newmax =max([blmax, bmmax, bnmax])
  newmin = min([blmin, bmmin, bnmin])

  fmin=newmin & fmax=newmax
  plot, time(1:itot), bl(1:itot,i),$
	xtitle='t',$
        title='Bl', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bm(1:itot,i),$
	xtitle='t',$
	title='Bm', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bn(1:itot,i),$
	xtitle='t',$
	title='Bn', font=3, yrange=[fmin, fmax]

  read, dumm

;fourth page
 !P.MULTI=[0,1,4,0,1]
  allmax = [[max(p(1:itot,i))], [max(ptot(1:itot,i))], [max(b2(1:itot,i))]]
  allmin = [[min(p(1:itot,i))], [min(ptot(1:itot,i))], [min(b2(1:itot,i))]]
  plot, time(1:itot), ptot(1:itot,i),$
        title='Total pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), p(1:itot,i),$
	title='Thermal pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), b2(1:itot,i),$
	title='Magnetic pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), rho(1:itot,i), title='Density', font=3,$ 
        yrange=[min(rho(1:itot,i)), max(rho(1:itot,i))],$
	xtitle='t'

  print, 'Continue with remaining plots? (y or n)'
  read, change
  if change eq '' or change eq 'y' then !P.MULTI=[0,2,3,0,1]

  allmax = [[max(vl(1:itot,i))], [max(vm(1:itot,i))], [max(vn(1:itot,i))]]
  allmin = [[min(vl(1:itot,i))], [min(vm(1:itot,i))], [min(vn(1:itot,i))]]
  plot, time(1:itot), vl(1:itot,i),$
	xtitle='t',$
        title='Vl', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vm(1:itot,i),$
	xtitle='t',$
	title='Vm', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vn(1:itot,i),$
	xtitle='t',$
	title='Vn', font=3, yrange=[min(allmin), max(allmax)]

  blmax = max(bl(1:itot,i)) &  blmin = min(bl(1:itot,i))
  bmmax = max(bm(1:itot,i)) &  bmmin = min(bm(1:itot,i))
  bnmax = max(bn(1:itot,i)) &  bnmin = min(bn(1:itot,i))
  newmax =max([blmax, bmmax, bnmax])
  newmin = min([blmin, bmmin, bnmin])

  fmin=newmin & fmax=newmax
  plot, time(1:itot), bl(1:itot,i),$
	xtitle='t',$
        title='Bl', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bm(1:itot,i),$
	xtitle='t',$
	title='Bm', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bn(1:itot,i),$
	xtitle='t',$
	title='Bn', font=3, yrange=[fmin, fmax]


  read, dumm

;fifth page
 !P.MULTI=[0,1,4,0,1]
  allmax = [[max(p(1:itot,i))], [max(ptot(1:itot,i))], [max(b2(1:itot,i))]]
  allmin = [[min(p(1:itot,i))], [min(ptot(1:itot,i))], [min(b2(1:itot,i))]]
  plot, time(1:itot), ptot(1:itot,i),$
        title='Total pressure',$
        xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), p(1:itot,i),$
	title='Thermal pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), b2(1:itot,i),$
	title='Magnetic pressure',$
	xtitle='t', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), rho(1:itot,i), title='Density', font=3,$ 
        yrange=[min(rho(1:itot,i)), max(rho(1:itot,i))],$
	xtitle='t'

  print, 'Continue with remaining plots? (y or n)'
  read, change
  if change eq '' or change eq 'y' then !P.MULTI=[0,2,3,0,1]

  allmax = [[max(vl(1:itot,i))], [max(vm(1:itot,i))], [max(vn(1:itot,i))]]
  allmin = [[min(vl(1:itot,i))], [min(vm(1:itot,i))], [min(vn(1:itot,i))]]
  plot, time(1:itot), vl(1:itot,i),$
	xtitle='t',$
        title='Vl', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vm(1:itot,i),$
	xtitle='t',$
	title='Vm', font=3, yrange=[min(allmin), max(allmax)]
  plot, time(1:itot), vn(1:itot,i),$
	xtitle='t',$
	title='Vn', font=3, yrange=[min(allmin), max(allmax)]

  blmax = max(bl(1:itot,i)) &  blmin = min(bl(1:itot,i))
  bmmax = max(bm(1:itot,i)) &  bmmin = min(bm(1:itot,i))
  bnmax = max(bn(1:itot,i)) &  bnmin = min(bn(1:itot,i))
  newmax =max([blmax, bmmax, bnmax])
  newmin = min([blmin, bmmin, bnmin])

  fmin=newmin & fmax=newmax
  plot, time(1:itot), bl(1:itot,i),$
	xtitle='t',$
        title='Bl', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bm(1:itot,i),$
	xtitle='t',$
	title='Bm', font=3, yrange=[fmin,fmax]
  plot, time(1:itot), bn(1:itot,i),$
	xtitle='t',$
	title='Bn', font=3, yrange=[fmin, fmax]


end

  time = fltarr(501)
  p = fltarr(501,5)
  xsat=fltarr(5) & zsat=xsat
  bl=p & bm=p & bn=p & vl=p & vm=p & vn=p  
  rho=p & ptot=p & b2=p 
  head = strarr(5)
  dumm7 = strarr(7)
  dumm = '' & names = strarr(15) & names=replicate(' ',15)
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
  i=0
  time0=time(0)
  time(0:itot)=2.0*(time(0:itot)-time0)+time0
     
  print, 'Which case?'
  read, fall

  while again eq 'y' do begin

    print,  'Which sat number'
    read, i
    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
        !P.THICK=3.
     	set_plot,'ps'
        device,filename='sat.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
    endif

    xp1=0.2 & xp2=0.7 
    mpl=float(indgen(3)) & yp1=0.7-0.2*mpl & yp2=0.9-0.2*mpl 
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,0,3]
    !P.FONT=3
    !P.CHARSIZE=2.0 
    !X.RANGE=[time0,time(itot)]
    !X.THICK=2
    !Y.THICK=2

    !P.POSITION=[xp1,yp1(0),xp2,yp2(0)]
    allmax = [[max(bl(1:itot,i))],[max(bm(1:itot,i))],[max(bn(1:itot,i))],0]
    allmin = [[min(bl(1:itot,i))],[min(bm(1:itot,i))],[min(bn(1:itot,i))],0]
    fmax=max(allmax) & fmin=min(allmin)
    fmax=1.2 & fmin=-1.2
    plot, time(1:itot), bl(1:itot,i), $
          title='Flux Rope', ytitle='B', $
;          title='Stationary Reconnection', ytitle='B', $
;          title='Linkage Region', ytitle='B', $
          xtickname=names, yrange=[fmin,fmax], $
          xstyle=1, ystyle=1, line=0
    oplot, time(1:itot), bm(1:itot,i), line=1
    oplot, time(1:itot), bn(1:itot,i), line=2
    oplot, time(1:itot), 0*bn(1:itot,i), line=1
      xpos=time(itot)+0.05*time(itot)
      ypos=fmin+0.8*(fmax-fmin)
    xyouts,xpos,ypos,fall,font=3
    ypos=fmin+0.5*(fmax-fmin)
    xyouts,xpos,ypos,'x='+string(xsat(i),'(f4.1)'),font=3
    ypos=fmin+0.3*(fmax-fmin)
    xyouts,xpos,ypos,'z='+string(zsat(i),'(f4.1)'),font=3
   
    !P.POSITION=[xp1,yp1(1),xp2,yp2(1)]
    allmax = [[max(vl(1:itot,i))],[max(vm(1:itot,i))],[max(vn(1:itot,i))],0]
    allmin = [[min(vl(1:itot,i))],[min(vm(1:itot,i))],[min(vn(1:itot,i))],0]
    fmax=1.0 & fmin=-1.0
    plot, time(1:itot), vl(1:itot,i),$
          ytitle='V', xtickname=names, yrange=[fmin, fmax], $
          xstyle=1, ystyle=1, line=0
    oplot, time(1:itot), vm(1:itot,i), line=1
    oplot, time(1:itot), vn(1:itot,i), line=2
    oplot, time(1:itot), 0*bn(1:itot,i), line=1

    !P.POSITION=[xp1,yp1(2),xp2,yp2(2)]
    allmax = [[max(p(1:itot,i))],[max(ptot(1:itot,i))],[max(b2(1:itot,i))],0]
    allmin = [[min(p(1:itot,i))],[min(ptot(1:itot,i))],[min(b2(1:itot,i))],0]
    fmax=4.0 & fmin=0.0
    plot, time(1:itot), ptot(1:itot,i),$
          ytitle='P', yrange=[fmin, fmax], $
          xtitle='time', xstyle=1, ystyle=1, line=0
    oplot, time(1:itot), p(1:itot,i), line=1
    oplot, time(1:itot), b2(1:itot,i), line=2


    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then begin 
        device,/close & !P.THICK=1. & set_plot,'x'
    endif
  endwhile

end


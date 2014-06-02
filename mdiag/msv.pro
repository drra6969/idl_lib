  time = fltarr(801)
  p = fltarr(801,5)
  xsat=fltarr(5) & zsat=xsat & vxsat=xsat & vzsat=xsat
  bl=p & bm=p & bn=p & by=p & bz=p
  vl=p & vm=p & vn=p & vy=p & vz=p
  rho=p & ptot=p & b2=p 
  yp1=xsat & yp2=xsat
  philm=0.79
  head = strarr(5)
  dumm7 = strarr(7)
  dumm = ''
  contin='' & again='y' & withps='n' & fall = '' & change=''
  fnumber=1
; remark number of satellites changes dumm7 and head dimension
  print, 'what filenumber'
  read, fnumber

  openr, 2, 'sat'+string(fnumber,'(i1)')
  readf, 2, format='(a53,f7.2)', dumm,philm
  print, dumm,philm
  pi=asin(1.0) & print ,pi & philm=pi*15.0/90.0 & print, dumm,philm
  readf, 2, format='(a80)', dumm
  print, dumm
  readf, 2, format='(a80)', dumm
  print, dumm
  for i=0,4 do begin 
    readf, 2, format='(a7,f7.2,a3,f7.2,a3,f7.2,a3,f7.2)',$
              dumm,xs,dumm,ys,dumm,vxs,dumm,vys
    xsat(i)=xs & zsat(i)=ys & vxsat(i)=vxs & vzsat(i)=vys
    print, dumm,' xsat=', xsat(i),'   zsat=',zsat(i)
    print, dumm,'vxsat=',vxsat(i),'  vzsat=',vzsat(i)
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
    if i lt 799 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
  vy(0:itot,*)=vl(0:itot,*)*cos(philm)-vm(0:itot,*)*sin(philm)
  vz(0:itot,*)=vl(0:itot,*)*sin(philm)+vm(0:itot,*)*cos(philm)

   time0=time(0)
   time(0:itot)=2.0*(time(0:itot)-time0)+time0

  print, 'Which case?'
  read, fall

  while again eq 'y' do begin

    print, 'With postscript?'
    read, withps
    if withps eq 'y' then begin 
        !P.THICK=2.
     	set_plot,'ps'
        device,filename='sat.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
      endif
    xp1=0.2 & xp2=0.7 
    mpl=float(indgen(5)) & yp1=0.83-0.19*mpl & yp2=0.95-0.19*mpl 
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,0,5]

    print, 'plot page?'
    read, contin
    if (contin eq '' or contin eq 'y') then begin
      !P.CHARSIZE=2.0  
      !P.FONT=3
      !X.THICK=2
      !Y.THICK=2
      !Y.TICKS=4
;      !Y.TICKlen=0.04
      vlmax = max(vl(1:itot,*)) &  vlmin = min(vl(1:itot,*))
      vmmax = max(vm(1:itot,*)) &  vmmin = min(vm(1:itot,*))
      vnmax = max(vn(1:itot,*)) &  vnmin = min(vn(1:itot,*))
      print, 'vlmin=',vlmin,'  vlmax=',vlmax
      print, 'vmmin=',vmmin,'  vmmax=',vmmax
      print, 'vnmin=',vnmin,'  vnmax=',vnmax
      fmin=-0.4 & fmax=0.4 & delf=fmax-fmin & delt=time(itot)-time(0)
;      fmin=vnmin-1.0*(vnmax-vnmin) & fmax=vnmax+1.0*(vnmax-vnmin)
      !X.RANGE=[time0,time(itot)]
      !Y.RANGE=[fmin,fmax]
      
      print, 'plotmin=',fmin,'  plotmax=',fmax, $
             '   Change? Enter y and desired values!'
      read, change
      if change eq 'y' then read, fmin, fmax
  
      for i=0,4 do begin
       !P.POSITION=[xp1,yp1(i),xp2,yp2(i)] 
;       plot, time(1:itot), -0.333*vy(1:itot,i),$
       plot, time(1:itot), -0.333*vz(1:itot,i),$
          title='Velocity Components',$
          xtitle='time', xstyle=1, ystyle=1, line=2
;       oplot, time(1:itot), 0.333*vz(1:itot,i), line=3
       oplot, time(1:itot), 0.333*vy(1:itot,i), line=3
       oplot, time(1:itot), vn(1:itot,i), line=0
       oplot, time(1:itot), 0.0*vn(1:itot,i), line=1
        xpos=time(itot)+0.15*delt
        ypos=fmin+0.9*delf
        xyouts,xpos,ypos,fall,font=3
        ypos=fmin+0.6*delf
        xyouts,xpos,ypos,'x='+string(xsat(i),'(f5.1)')
        ypos=fmin+0.45*delf
        xyouts,xpos,ypos,'z='+string(zsat(i),'(f5.1)')
        ypos=fmin+0.25*delf
        xyouts,xpos,ypos,'vx='+string(vxsat(i),'(f5.2)')
        ypos=fmin+0.1*delf
        xyouts,xpos,ypos,'vz='+string(vzsat(i),'(f5.2)')
        xpos=time(0)-0.07*delt & ypos=fmax+0.08*delf
        xyouts,xpos,ypos,'VX'
        xpos=time(itot)-0.02*delt & ypos=fmax+0.08*delf
        xyouts,xpos,ypos,'VY/VZ'
       axis, yaxis=1, yrange=[3.0*fmin,3.0*fmax], ystyle=1
      endfor
    endif

    print, 'view results again or make ps file?'
    read, again
    if withps eq 'y' then begin
       device,/close & !P.THICK=1. & set_plot,'x'
    endif
  endwhile

end


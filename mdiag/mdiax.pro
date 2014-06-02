  t = fltarr(1001,5)
  m=fltarr(1001,5) & msp=m & ek=m & eb=m & et=m & es=m
  vm=m & vmsp=m
  vek=m & veb=m & vet=m & ves=m
  vgp=m & vjb=m & rj2=m & dek=m & deb=m & det=m

  dumm4 = strarr(4)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''
  fnumber=1 & nop=1 & itot=intarr(5)

  print, 'how many plots (n=1 digit<=5)'
  read, nop
  
 for iop=0, nop-1  do  begin 
  print, 'what filenumber'
  read, fnumber
  openr, 2, 'magdia1'+string(fnumber,'(i1)')
  readf, 2, dumm3
  print, dumm3

  i=0
  while not eof(2) do begin
    readf, 2, ts,ms,msps,eks,ebs,ets

    t(i,iop)=ts & m(i,iop)=ms & msp(i,iop)=msps 
    ek(i,iop)=eks & eb(i,iop)=ebs & et(i,iop)=ets

    itot(iop)=i
    if i lt 1000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
  es(*,iop)=ek(*,iop)+eb(*,iop)+et(*,iop)

;second dataset:
  openr, 4, 'magdia2'+string(fnumber,'(i1)')
  readf, 4, dumm3
  print, dumm3

  i=0
  while not eof(4) do begin
    readf, 4, ts,vgps,vjbs,rj2s,deks,debs,dets

    vgp(i,iop)=vgps & vjb(i,iop)=vjbs & rj2(i,iop)=rj2s
    dek(i,iop)=deks & deb(i,iop)=debs & det(i,iop)=dets

    itot(iop)=i
    if i lt 1000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 4
 endfor


     while again eq 'y' do begin
       print, 'which dataset; 1 <=input<= ',nop
       read, iop
       iop=iop-1
       print, 'Which case?'
       read, fall

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='dia.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin
    !P.CHARSIZE=2.0  
    !P.MULTI=[0,0,4]
    !P.POSITION=[0.15,0.05,0.7,0.23]
    
    vek(*,iop)=ek(*,iop)-ek(0,iop) & veb(*,iop)=eb(*,iop)-eb(0,iop)  
    vet(*,iop)=et(*,iop)-et(0,iop) & ves(*,iop)=es(*,iop)-es(0,iop)
    amax = [[max(vek(1:itot(iop),iop))], [max(veb(1:itot(iop),iop))], $
          [max(vet(1:itot(iop),iop))], [max(ves(1:itot(iop),iop))]]
    amin = [[min(vek(1:itot(iop),iop))], [min(veb(1:itot(iop),iop))], $
          [min(vet(1:itot(iop),iop))], [min(ves(1:itot(iop),iop))]]
    plot, t(1:itot(iop),iop), ves(1:itot(iop),iop),$
        title='Var of Energy',$
        xtitle='time', font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot(iop),iop), vek(1:itot(iop),iop), line=1
    oplot, t(1:itot(iop),iop), veb(1:itot(iop),iop), line=2
    oplot, t(1:itot(iop),iop), vet(1:itot(iop),iop), line=3

    !P.POSITION=[0.15,0.28,0.7,0.46]
    plot, t(1:itot(iop),iop), es(1:itot(iop),iop),$
        title='therm., magn., kinet., and total Energy',$
        font=3, yrange=[0, max(es(1:itot(iop),iop))]
    oplot, t(1:itot(iop),iop), ek(1:itot(iop),iop), line=1
    oplot, t(1:itot(iop),iop), eb(1:itot(iop),iop), line=2
    oplot, t(1:itot(iop),iop), et(1:itot(iop),iop), line=3

    !P.POSITION=[0.15,0.51,0.7,0.69]
    vm(*,iop)=m(*,iop)-m(0,iop) & vmsp(*,iop)=msp(*,iop)-msp(0,iop)
    amax = [[max(vm(1:itot(iop),iop))], [max(vmsp(1:itot(iop),iop))]]
    amin = [[min(vm(1:itot(iop),iop))], [min(vmsp(1:itot(iop),iop))]]
    plot, t(1:itot(iop),iop), vm(1:itot(iop),iop),$
        title='Var of total and magnetosph. Mass',$
        font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot(iop),iop), vmsp(1:itot(iop),iop), line=2

    !P.POSITION=[0.15,0.74,0.7,0.92]
    plot, t(1:itot(iop),iop), m(1:itot(iop),iop),$
        title='Total and Magnetosph. Mass',$
        font=3, yrange=[0, max(m(1:itot(iop),iop))]
    oplot, t(1:itot(iop),iop), msp(1:itot(iop),iop), line=2
    xpos=t(itot(iop),iop)+0.05*t(itot(iop),iop)
    ypos=0.5*max(m(1:itot(iop),iop))
    xyouts,xpos,ypos,fall,font=3


  endif

  print, 'continue with next plot'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; second page 
    !P.CHARSIZE=1.0  
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.15,0.2,0.7,0.4]

    amax = [[max(vgp(1:itot(iop),iop))], $
           [max(vjb(1:itot(iop),iop))], [max(rj2(1:itot(iop),iop))]]
    amin = [[min(vgp(1:itot(iop),iop))], $
           [min(vjb(1:itot(iop),iop))], [min(rj2(1:itot(iop),iop))]]
    plot, t(1:itot(iop),iop), vgp(1:itot(iop),iop),$
        title='Energysource terms: v grad p; v (jxB); res j**2',$
        xtitle='time', font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot(iop),iop), vjb(1:itot(iop),iop), line=2
    oplot, t(1:itot(iop),iop), rj2(1:itot(iop),iop), line=3

    !P.POSITION=[0.15,0.6,0.7,0.8]
    amax = [[max(det(1:itot(iop),iop))], $
           [max(dek(1:itot(iop),iop))], [max(deb(1:itot(iop),iop))]]
    amin = [[min(det(1:itot(iop),iop))], $
           [min(dek(1:itot(iop),iop))], [min(deb(1:itot(iop),iop))]]
    plot, t(1:itot(iop),iop), det(1:itot(iop),iop),$
        title='Rate of Energy Change',$
        font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot(iop),iop), dek(1:itot(iop),iop), line=3
    oplot, t(1:itot(iop),iop), deb(1:itot(iop),iop), line=2
    xpos=t(itot(iop),iop)+0.05*t(itot(iop),iop)
    ypos=0.5*(min(amin)+max(amax))
    xyouts,xpos,ypos,fall,font=3

   endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end





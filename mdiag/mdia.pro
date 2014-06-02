  t = fltarr(8001)
  m=t & msp=t & ek=t & eb=t & et=t & es=t
  vm=t & vmsp=t
  vek=t & veb=t & vet=t & ves=t
  vgp=t & vjb=t & rj2=t & dek=t & deb=t & det=t

  dumm4 = strarr(4)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''

  openr, 2, 'magdia1'
  readf, 2, dumm3
  print, dumm3

  i=0
  while not eof(2) do begin
;    readf, 2, format='(f8.3,2(a1,f12.2),a1,f12.3,2(a1,f12.2))', $
;              ts,dumm,ms,dumm,msps,dumm,eks,dumm,ebs,dumm,ets
    readf, 2, ts,ms,msps,eks,ebs,ets

    t(i)=ts & m(i)=ms & msp(i)=msps & ek(i)=eks & eb(i)=ebs & et(i)=ets

    itot=i
    if i lt 8000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 2
  es=ek+eb+et

;second dataset:
  openr, 4, 'magdia2'
  readf, 4, dumm3
  print, dumm3

  i=0
  while not eof(4) do begin
;    readf, 4, format='(f8.3,6(a1,f10.4))', $
;          ts,dumm,vgps,dumm,vjbs,dumm,rj2s,dumm,deks,dumm,debs,dumm,dets
    readf, 4, ts,vgps,vjbs,rj2s,deks,debs,dets

    vgp(i)=vgps & vjb(i)=vjbs & rj2(i)=rj2s
    dek(i)=deks & deb(i)=debs & det(i)=dets

    itot=i
    if i lt 8000 then i=i+1 else stop, ' to many records '
  endwhile
  close, 4

     print, 'Which case?'
     read, fall

     while again eq 'y' do begin

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
    vek=ek-ek(0) & veb=eb-eb(0) & vet=et-et(0) & ves=es-es(0)
    amax = [[max(vek(1:itot))], [max(veb(1:itot))], $
          [max(vet(1:itot))], [max(ves(1:itot))]]
    amin = [[min(vek(1:itot))], [min(veb(1:itot))], $
          [min(vet(1:itot))], [min(ves(1:itot))]]
    plot, t(1:itot), ves(1:itot),$
        title='Var of Energy',$
        xtitle='time', font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot), vek(1:itot), line=1
    oplot, t(1:itot), veb(1:itot), line=2
    oplot, t(1:itot), vet(1:itot), line=3

    !P.POSITION=[0.15,0.28,0.7,0.46]
    plot, t(1:itot), es(1:itot),$
        title='therm., magn., kinet., and total Energy',$
        font=3, yrange=[0, max(es(1:itot))]
    oplot, t(1:itot), ek(1:itot), line=1
    oplot, t(1:itot), eb(1:itot), line=2
    oplot, t(1:itot), et(1:itot), line=3

    !P.POSITION=[0.15,0.51,0.7,0.69]
    vm=m-m(0) & vmsp=msp-msp(0)
    amax = [[max(vm(1:itot))], [max(vmsp(1:itot))]]
    amin = [[min(vm(1:itot))], [min(vmsp(1:itot))]]
    plot, t(1:itot), vm(1:itot),$
        title='Var of total and magnetosph. Mass',$
        font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot), vmsp(1:itot), line=2

    !P.POSITION=[0.15,0.74,0.7,0.92]
    plot, t(1:itot), m(1:itot),$
        title='Total and Magnetosph. Mass',$
        font=3, yrange=[0, max(m(1:itot))]
    oplot, t(1:itot), msp(1:itot), line=2
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*max(m(1:itot))
    xyouts,xpos,ypos,fall,font=3


  endif

  print, 'continue with next plot'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

; second page 
    !P.CHARSIZE=1.0  
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.15,0.2,0.7,0.4]

    amax = [[max(vgp(1:itot))], [max(vjb(1:itot))], [max(rj2(1:itot))]]
    amin = [[min(vgp(1:itot))], [min(vjb(1:itot))], [min(rj2(1:itot))]]
    plot, t(1:itot), vgp(1:itot),$
        title='Energysource terms: v grad p; v (jxB); res j**2',$
        xtitle='time', font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot), vjb(1:itot), line=2
    oplot, t(1:itot), rj2(1:itot), line=3

    !P.POSITION=[0.15,0.6,0.7,0.8]
    amax = [[max(det(1:itot))], [max(dek(1:itot))], [max(deb(1:itot))]]
    amin = [[min(det(1:itot))], [min(dek(1:itot))], [min(deb(1:itot))]]
    plot, t(1:itot), det(1:itot),$
        title='Rate of Energy Change',$
        font=3, yrange=[min(amin), max(amax)]
    oplot, t(1:itot), dek(1:itot), line=3
    oplot, t(1:itot), deb(1:itot), line=2
    xpos=t(itot)+0.05*t(itot)
    ypos=0.5*(min(amin)+max(amax))
    xyouts,xpos,ypos,fall,font=3

   endif

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end





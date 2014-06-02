  t = fltarr(1001)
  xl=fltarr(1001,10) & yxl=xl & vyxl=xl & cx=xl & efx=xl & sxl=xl & flx=xl 
  ol=xl & yol=xl & vyol=xl & co=xl & efo=xl & sol=xl & flo=xl 
  eflx=xl
  dfll=fltarr(1001) & flx1=dfll & flo1=dfll & efx1=dfll & efo1=dfll 
  yxl1=dfll & yol1=dfll & cux1=dfll & cuo1=dfll & noxo1=intarr(1001)
  efll=dfll

  nomax=10 & xoo=0 & xox=0 & nox=1 & noo=1
  dumm4 = strarr(4)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''

  openr, 2, 'magdxl'
  readf, 2, dumm3
  print, dumm3

  i=-1 & no=1
  while not eof(2) do begin
    readf, 2, ts, xox, xxls, yxls, vyxls, cusx, efsx, slsx, flsx, nox
    readf, 2, ts, xoo, xols, yols, vyols, cuso, efso, slso, flso, noo

    if (nox ne noo) then begin
      print, t(i), nox,' not equal ',noo
      stop
    endif

    if (noo le no) then i=i+1 
    no=noo
    if (no gt nomax) then print, 'raise no'

    if (xox eq 1) then begin
      t(i)=ts & xl(i,no-1)=xxls & yxl(i,no-1)=yxls & cx(i,no-1)=cusx 
              efx(i,no-1)=efsx & sxl(i,no-1)=slsx & flx(i,no-1)=flsx
              ol(i,no-1)=xols & yol(i,no-1)=yols & co(i,no-1)=cuso 
              efo(i,no-1)=efso & sol(i,no-1)=slso & flo(i,no-1)=flso
              noxo1(i)=no
    endif
    if (xox eq 0) then begin
      t(i)=ts & ol(i,no-1)=xxls & yol(i,no-1)=yxls & co(i,no-1)=cusx 
              efo(i,no-1)=efsx & sol(i,no-1)=slsx & flo(i,no-1)=flsx
              xl(i,no-1)=xols & yxl(i,no-1)=yols & cx(i,no-1)=cuso 
              efx(i,no-1)=efso & sxl(i,no-1)=slso & flx(i,no-1)=flso
              noxo1(i)=no
    endif
    if (i eq 1000) then stop, ' to many records '
    itot=i
  endwhile
  close, 2

  for it=0, itot do begin
    inflx=where( flx(it,*) eq min(flx(it,*)) )
    enflx=where( efx(it,*) eq max(efx(it,*)) )
    inflo=where( flo(it,*) eq max(flo(it,*)) )
    flx1(it)=flx(it,inflx)
    flo1(it)=flo(it,inflo)
    efx1(it)=efx(it,enflx)
    efo1(it)=efo(it,inflo)
    cux1(it)=cx(it,enflx)
    cuo1(it)=co(it,inflo)
    yxl1(it)=yxl(it,enflx)
    yol1(it)=yol(it,inflo)
  endfor
    
  
  
  dfl=flo1-flx1 & dfl=dfl-dfl(0)
  dt=t(1:itot)-t(0:(itot-1))
  efl=efx1-efo1

; reconnected flux from integr of reconnection rate
  for it=1,itot do $
    dfll(it)=dfll(it-1)+0.5*dt(it-1)*( efl(it-1)+efl(it) )       
; reconnection rate from the reconnected flux
    efll(1:itot)=(dfl(1:itot)-dfl(0:(itot-1)))/dt  
; smoothed reconnection rates:
    efls=smooth(efl,3)
    eflls=smooth(efll,3)

                  

     print, 'Which case?'
     read, fall

     while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='xl.ps'
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
    !P.POSITION=[0.15,0.78,0.7,0.96]
    amax=max(yol1-yxl1)
    amin=min(yol1-yxl1)
    plot, t(0:itot), yol1(0:itot)-yxl1(0:itot), line=0,$
        title='Y_ol-Y_xl and No of X lines',$
        font=3, yrange=[amin,amax]
    oplot, t(0:itot), noxo1(0:itot), line=2

    !P.POSITION=[0.15,0.54,0.7,0.72]
    amax=max(cx)
;    amin=0.0
    amin=min(cx)
    plot, t(0:itot), cux1(0:itot), line=0,$
        title='Current Density at X and O Line',$
        font=3, yrange=[amin,amax]
    oplot, t(0:itot), cuo1(0:itot), line=2

    !P.POSITION=[0.15,0.30,0.7,0.48]
    amax=max([dfl(0:itot),dfll(0:itot)])
    amin=min([dfl(0:itot),dfll(0:itot)])
    plot, t(0:itot), dfl(0:itot), line=0,$
        title='Reconnected Flux',$
        font=3, yrange=[amin,amax]
    for in=1, nomax-1 do oplot, t(0:itot), dfl(0:itot), line=in
    oplot, t(0:itot), dfll(0:itot), line=2


    !P.POSITION=[0.15,0.06,0.7,0.24]
    amax=max([eflls, efls])
;    amax=0.1
    amin=0.
;    amax=max(eflx(0:itot,nomax-1))
;    amin=min(eflx(0:itot,nomax-1))
    plot, t(0:itot), efls(0:itot), line=0,$
        title='E_xl-E_ol',$
        xtitle='time', font=3, yrange=[amin,amax]
    oplot, t(0:itot), eflls(0:itot), line=2

  endif

openw, 4, 'datxl'
  printf, 4, '      time     flux      rekrate'
  for it=1,itot do begin
   printf, 4, t(it),dfl(it),efl(it)
 endfor
close,4

openw, 5, 'datxl1'
  printf, 5, '      time     flux      e-field '
  for it=1,itot do begin
   printf, 5, t(it), dfl(it), efls(it)
 endfor
close,5

     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end





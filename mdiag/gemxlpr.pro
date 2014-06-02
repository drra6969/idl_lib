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
    
  dfl=flo1-flx1 & dfl=dfl-dfl(0)  ; dfl - reconnected flux
  dt=t(1:itot)-t(0:(itot-1))      ; time step (t)
  efl=efx1-efo1                   ; reconnection rate 

; reconnected flux from integr of reconnection rate
  for it=1,itot do $
    dfll(it)=dfll(it-1)+0.5*dt(it-1)*( efl(it-1)+efl(it) )       
; reconnection rate from the reconnected flux
    efll(1:itot)=(dfl(1:itot)-dfl(0:(itot-1)))/dt  
; smoothed reconnection rate:
    efls=smooth(efl,3)
    eflls=smooth(efll,3)


openw, 4, 'datxl'
  printf, 4, '      time     flux      rekrate'
  for it=1,itot do begin
   printf, 4, t(it),dfl(it),efl(it)
 endfor
close,4

openw, 5, 'datxl1'
  printf, 5, '      time     flux      e-field      rekrate'
  for it=1,itot do begin
   printf, 5, t(it),dfl(it),efls(it),eflls(it),dfll(it)
 endfor
close,5



end





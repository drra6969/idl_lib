  nof=4
  t = fltarr(1001,nof)  &  itot=intarr(nof)
  xl=fltarr(1001,nof) & yxl=xl & cx=xl & efx=xl & sxl=xl & flx=xl 
  ol=xl & yol=xl & co=xl & efo=xl & sol=xl & flo=xl 
  eflx=xl
  

  no=1 & nomax=1 & xoo=0 & xox=0 & nox=1 & noo=1
  dumm4 = strarr(4)  & incase='ll' & cas=strarr(nof)
  dumm3 = strarr(3)
  dumm=''
  contin='' & again='y' & withps='n' & fall=''

  for iff=0, nof-1 do begin
    print, 'what filenumber'
    read, fnumber
    print, 'what case'
    read, incase
    if (incase ne '') then cas(iff)=incase
  
    openr, 2, 'magdxl'+string(fnumber,'(i1)')

    readf, 2, dumm3
    print, dumm3

    i=-1
    while not eof(2) do begin
      readf, 2, ts, xoo, xols, yols, cuso, efso, slso, flso, nox
      readf, 2, ts, xox, xxls, yxls, cusx, efsx, slsx, flsx, noo

      if (nox ne noo) then begin
         print, t(i), nox,' not equal ',noo,'  Correct File!'
         stop
      endif

      i=i+1
      t(i,iff)=ts & xl(i,iff)=xxls & yxl(i,iff)=yxls & cx(i,iff)=cusx 
              efx(i,iff)=efsx & sxl(i,iff)=slsx & flx(i,iff)=flsx
              ol(i,iff)=xols & yol(i,iff)=yols & co(i,iff)=cuso 
              efo(i,iff)=efso & sol(i,iff)=slso & flo(i,iff)=flso
      if (i eq 1000) then stop, ' to many records '
      itot(iff)=i-1
    endwhile
    close, 2

    dt=t(1:itot(iff),iff)-t(0:(itot(iff)-1),iff)
    eflx(1:itot(iff),iff)=(flx(1:itot(iff),iff)-flx(0:(itot(iff)-1),iff))/dt
  endfor



     while again eq 'y' do begin

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
         set_plot,'ps'
        device,filename='xl.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
        !P.THICK=3
       endif
; first page
  !P.REGION=[0.,0.,1.0,1.25]

  print, 'plot first page?'
  read, contin
  if (contin eq '' or contin eq 'y') then begin

    !P.CHARSIZE=2.0  
    !P.MULTI=[0,0,4]

    !P.THICK=1.5


    !P.POSITION=[0.1,0.50,0.65,0.8]
    tmax=max(t)
    amax=max(flx)
    amin=min(flx)
    xo1=tmax-25
    yt1=0.95*amax
    yt2=0.85*amax
    yt3=0.75*amax
    yt4=0.65*amax


    plot, t(0:itot(0),0), flx(0:itot(0),0), line=0,$
        title='Reconnected Flux',$
        font=3, yrange=[amin,amax], xrange=[0,tmax]
    for in=1, nof-1 do oplot, t(0:itot(in),in), flx(0:itot(in),in), line=in
        xyouts, xo1, yt1,'_____ Hall-res(j)',charsize=1.3,charthick=2
        xyouts, xo1, yt2,'_ _ _ Hall-res=const',charsize=1.3,charthick=2
        xyouts, xo1, yt3,'_ . _ MHD-res(j)',charsize=1.3,charthick=2
        xyouts, xo1, yt4,'........ MHD-res=const',charsize=1.3,charthick=2


    !P.POSITION=[0.1,0.1,0.65,0.4]
    amax=max(efx-efo)
    amin=min(efx-efo)
;    amax=max(eflx(0:itot,nomax-1))
;    amin=min(eflx(0:itot,nomax-1))
     pp1=smooth( efx(0:itot(0),0)-efo(0:itot(0),0),3 ,/EDGE_TRUNCATE)
;     pp1=efx(0:itot(0),0)-efo(0:itot(0),0)
    plot, t(0:itot(0),0), pp1, line=0,$
        title='E_xl-E_ol',$
        xtitle='time', font=3, yrange=[0,0.2], xrange=[0,tmax]
    for in=1, nof-1 do begin
        pp=smooth( efx(0:itot(in),in)-efo(0:itot(in),in),3,/EDGE_TRUNCATE )
        oplot, t(0:itot(in),in), pp, line=in
    endfor


;    for in=0, nomax-1 do oplot, t(0:itot), $
;                          eflx(0:itot,in), line=2

  endif



     print, 'view results again or make ps file?'
     read, again
     if withps eq 'y' then device,/close
     set_plot,'x'
   endwhile


end





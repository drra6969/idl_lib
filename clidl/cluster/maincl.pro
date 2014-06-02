; PROGRAM to read and sort cluster data
;----------------------------------------
FUNCTION XTICKS, axis, index, value
COMMON plaxis,tax
;print,'tax:',tax
if tax eq 'h' then begin
 hour = LONG(value)
 minute = LONG(60.*(value-hour)) 
 sec = 3600*value mod 60
endif
if (tax eq 'm') or (tax eq 'ms') then begin
; print, 'value:',LONG(value)
 hour = LONG(value)/60
 minute = LONG(60*value-3600 * hour) / 60
 sec = 60*value mod 60
endif
if tax eq 's' then begin
 print, 'value:',LONG(value)
 hour = LONG(value)/3600
 minute = LONG(value-3600 * hour) / 60
 sec = value mod 60
endif
RETURN, STRING(hour, minute, sec, $
        FORMAT="(i2.2, ':', i2.2, ':', i2.2)")
END

COMMON fields, b,varbt,varbb,n,v,t,np,no,nhe,nhia,vp,vhia,tp,thia
COMMON sc, rsc,dxsc,dysc,dzsc,volsc,volrat
COMMON plaxis,tax
   
  withps='n' & closeps='n' & psname='ps' & smo='n'
  i=0
  whatindex='0' & index=0 & conti='y'
  contin='' & again='y'
  re=6371.2

  nameaux=FINDFILE('CL_SP_AUX_*') ; auxiliary data
  namecis=FINDFILE('CL_SP_CIS_*') ; plasma data
  namefgm=FINDFILE('CL_SP_FGM_*') ; magnetic data

  testa=size(nameaux) & testc=size(namecis) & testf=size(namefgm)
  if ( (testa(1) ne testc(1)) or (testa(1) ne testf(1)) $
                            or (testc(1) ne testf(1)) )  then begin
     print, 'number of files is not identical!!'
     stop
  endif
  saux=size(nameaux)
  nfiles=saux(1)

; READ DATA
rdat:
  namea = nameaux(i)
  readaux, namea, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild
  namef = namefgm(i)
  readfgm, namef, nfgm,timem,statsm,b,varbt,varbb
  namec = namecis(i)
  readcis, namec, ncis,timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia


; COMMON TIMES IN AUX, B, AND P
  reducetocom, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild,$
                   nfgm,timem,statsm,b,varbt,varbb,$
                   ncis,timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia, nn

; ELIMINATE BAD DATA
  nobadat, time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild,$
               timem,statsm,b,varbt,varbb,$
               timep,statsp,np,no,nhe,nhia,vp,vhia,tp,thia, nn

; ROTATE INTO GSM
  psi = !pi*gsegsm/180.
  gsetogsm,psi,b & gsetogsm,psi,vp & gsetogsm,psi,vhia
  gsetogsm,psi,sccoor & gsetogsm,psi,scvel 
  gsetogsm,psi,scdr1 & gsetogsm,psi,scdr2 & gsetogsm,psi,scdr3
  gsetogsm,psi,scdr4

; DETERMINE CLUSTER CONFIGURATION PARAMETERS
;  - MAX SEPARATION IN X, Y, AND Z
;  - CLUSTER VOLUME/IDEAL VOLUME
  clconfig, re,nn,sccoor,scdr1,scdr2,scdr3,scdr4,$
                    dxsc,dysc,dzsc,volsc,volrat,rsc
                    
; BULK PROPERITIES
  n = np+nhia   &  t = (np*tp+nhia*thia)/n  &  v=vp
  v(0,*) = ( np(*)*vp(0,*)+nhia(*)*vhia(0,*) ) / n(*)
  v(1,*) = ( np(*)*vp(1,*)+nhia(*)*vhia(1,*) ) / n(*)
  v(2,*) = ( np(*)*vp(2,*)+nhia(*)*vhia(2,*) ) / n(*)

; MODIFY TIME (to minutes of the day) AND CONTINUOUS INTERVALS
timeprop, nn, time, datestrn, strnn,nint,itstart,itend
 print,'nint:', nint

; 3. Menu:
;---------
cut: 
  if withps eq 'y' then  begin 
     withps = 'n' & closeps='n' & device,/close & set_plot,'x'
     !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
  endif

  print, 'Dataset:', i
  print, 'Total Time range:',time(0)/60.,time(nn-1)/60.
  print, 'Continuous Time Intervals'
  for ii=0,nint-1 do print,'Interval:',$
                           ii, time(itstart(ii))/60.,time(itend(ii))/60.
  if smo eq 'y' then print, 'Smoothing is presently on!'$ 
       else print, 'Smoothing is presently off!'
                         
  print, 'OPTIONS:   <return> -> plot all available data'
  print, '                 n  -> plot next data set'
  print, '                 r  -> plot previous data set'
  print, '                 i  -> choose data set index'
  print, '                 m  -> data interval m'
  print, '                 t  -> data interval for tmin, tmax in h'
  print, '                 g  -> smooth on (y) and off (n)'
  print, '                 p  -> Postscrip output'
  print, '                 q  -> TERMINATE'
  read, whatindex

  if whatindex eq 'q' then stop
  
  if whatindex eq 'n' then begin
    i=i+1  &  if i gt nfiles-1 then i=0
    goto, rdat
  endif
  if whatindex eq 'r' then begin
    i=i-1  &  if i lt 0 then i=0
    goto, rdat
  endif
  if whatindex eq 'i' then begin
    print,'Input data index!'
    read, i &  if ((i lt 0) or i gt nfiles-1) then i=0
    goto, rdat
  endif

  if whatindex eq '' then begin
    tmin=time(0)-0.5 & tmax=time(nn-1)+0.5 
    its=0 & ite=nn-1 & endif
  if whatindex eq 'm' then begin
    print,'Input interval index'
    read, m   &   if((m lt 0) or (m gt nint-1)) then m=0
    tmin=time(itstart(m))-0.5 & tmax=time(itend(m))+0.5
    its=itstart(m) & its=itend(m)
  endif
  if whatindex eq 't' then begin
    print,'Input tmin>=',time(0)/60.,'  and tmax<=',time(nn-1)/60.
    read, tmin,tmax
    tmin=tmin*60. & tmax=tmax*60.
    if((tmin lt time(0)) or (tmin gt time(nn-1))) then tmin=time(0)-0.5
    if((tmax lt time(0)) or (tmax gt time(nn-1))) then tmax=time(nn-1)+0.5
    its=0 & while time(its) lt tmin do its=its+1
    ite=nn-1 & while time(ite) gt tmax do ite=ite-1
    print, 'tmin, tmax:', tmin, tmax
    print, 'itmin, itmax:', its, ite
  endif
  
  if whatindex eq 'p' then begin
     withps = 'y'
     !P.THICK=2.
     !X.THICK=1.5
     !Y.THICK=1.5
     !P.CHARTHICK=2.
     psname='cl'+datestrn+':'+string(tmin/60.,'(i3.3)')$
                             +'_'+string(tmax/60.,'(i3.3)')
     set_plot,'ps'
     device,/color,bits=8
     device,filename=psname+'.ps'
     device, /portrait, /times, /bold, font_index=3
     device, /inches, xsize=8., ysize=10., scale_factor=0.95, $
                      xoffset=0.5,yoffset=0.8
  endif

if (withps ne 'y') then  window,0,xsize=720,ysize=900,title='Overview Plot'
plotsum, nn, time, tmin, tmax, its, ite, strnn,withps

goto, cut

end

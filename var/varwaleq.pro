; PROGRAM to read and plot satellite data
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

COMMON ref, bxr,byr,bzr,vxr,vyr,vzr, $
            rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            xsat1,ysat1,zsat1,vxsat1,vysat1,vzsat1, $
            xsat2,ysat2,zsat2,vxsat2,vysat2,vzsat2, $
            index, starttime, phi, xtit, withps

COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv
COMMON plaxis,tax

  pi = 3.14159265536 & phi0=0.0 & phi=0.0  & time2d=0.0 & day=''
  
  coor = 's' & whatangle='o' & withunits='y' & satchoice='b' & unitype='e'
  whatindex='0' & index=0 & conti='y'
  contin='' & again='y' & withps='n' & closeps='n' & psname='ps' 
  countps=0 & wcount=0 & scount=0 & hcount=0 & rcount=0 
  fall = '' & change='' & cas=''
  xtit='time (h:m:s)'   &   plotvars='t' & vardat='absent'
  ebasis='b' & ebst='B'
  xangle='' & zangle='' & phix=0.0 & phiz=0.0
  evo1=[1.,0.,0.] & evo2=[0.,1.,0.] & evo3=[0.,0.,1.] & ewo=[1.,1.,1.]

  tv0min=0.0 & tv0max=0.0 & ib01=0L & ib02=0L & iv01=0L & iv02=0L 
  varian='absent'
  deltm = 2.0 &  fdelt = 0.2 & delt=deltm/60.
  slwal0 = 0.8 & slwal1 = 1.3 &  slht0 = 0.9 & slht1 = 1.1 &  itest=0
  succes=0  & smo='y'
; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
;    readeqs,day          ; proton data only
    readhiab,day
;    readeqso2,day          ; proton and O+data
;    readeqso,day          ; proton and O+data
;   restore, file='980310_GSE_1030_1110.dat'
;   restore, file='980310_GSE_1150_1220.dat'
;   restore, file='980310_GSE_1250_1345.dat'

;     neqs=4.*neqs
;    readeqhr,day          ; high resolution proton data only
    strnn='CLUSTER, DAY: '+day
    print,strnn

; tmb - timearray for B dimension: ntb
; tmv - timearray for Plasma dimension: ntv
; t0min, t0max are min and max time cavered by B and plasma data
; ib0, ie0: min and max indices for B
; ivb0, ive0: min and max indices for plasma
 
; 1. Determine a common time range for B, plasma
;-----------------------------------------------
    print, 'Time min and max for B:', tmb(0), tmb(ntb-1)
    print, 'Time min and max for Plasma:', tmv(0), tmv(ntv-1)
    t0min=max([tmb(0),tmv(0)]) & t0max=min([tmb(ntb-1),tmv(ntv-1)])
    tmin=t0min & tmax=t0max
    tvmin=tmin & tvmax=tmax
    if tmin ge tmax then begin
      print, 'Plasma and field data do not overlap -> Stop'
      stop
    endif
    ib=0L & ie=ntb-1 & ivb=0L & ive=ntv-1
    while tmb(ib) lt tmin do ib=ib+1 &   if ib gt 0 then ib=ib-1
    while tmb(ie) gt tmax do ie=ie-1 &   if ie lt ntb-1 then ie=ie+1
    while ( ((ie-ib) lt 2) and (ib gt 0) and (ie lt ntb-1)) do begin
     ib=ib-1 & ie=ie+1
    endwhile
    while tmv(ivb) lt tvmin do ivb=ivb+1 &   if ivb gt 0 then ivb=ivb-1
    while tmv(ive) gt tvmax do ive=ive-1 &   if ive lt ntv-1 then ive=ive+1
    while ( ((ive-ivb) lt 2) and (ivb gt 0) and (ive lt ntv-1)) do begin
     ivb=ivb-1 & ive=ive+1
    endwhile
    while (0.5*(tmv(ivb)+tmv(ivb+1))) le tmb(0) do ivb=ivb+1
    while (0.5*(tmv(ive)+tmv(ive-1))) ge tmb(ntb-1) do ive=ive-1
    ib0=ib & ie0=ie & ivb0=ivb & ive0=ive
    aveb, ivb,ive 

; 1a. Set search time range = total time range
    ibr=ib0 & ier=ie0 & ivbr=ivb0 & iver=ive0
    trmin=t0min & trmax=t0max & trvmin=t0min & trvmax=t0max
    
; 2. Open some windows before starting menues:
;---------------------------------------------
;  window,0,xsize=640,ysize=512,title='Overview Plot'
;  window,1,xsize=640,ysize=512,title='Variance Plots'
;  window,2,xsize=640,ysize=512,title='Var Data'

; 3. Menu:
;---------
cut: 
  if withps eq 'y' then  begin 
     withps = 'n' & closeps='n' & device,/close & set_plot,'x'
     !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
  endif

  print, '        Selected test: itest=',itest
  print, '        Required slop for Walenrel (min,max):',slwal0,slwal1
  print, '        Required slop for HT frame(min,max):',slht0,slht1
  print, '        Search interval (min) and increment analysis:',deltm,fdelt
  print, '        Search Time range:',trmin, trmax
  print, '        Maximum Time range:',t0min, t0max
  if smo eq 'y' then print, 'Smoothing velocity is presently on!'$ 
       else print, 'Smoothing velocity is presently off!'
                         
  print, 'OPTIONS:         d  -> Define a search time range'
  print, '                 u  -> Set search time range back to total range'
  print, '                 t  -> Time period and increment for search'
  print, '                 g  -> smooth on (y) and off (n)'
  print, '                 s  -> Start search'
  print, '                 h  -> Change required slope for HT frame'
  print, '                 w  -> Change required slope for Walen relation'
  print, '                 i  -> Choose itest: i=0 -> Walen rel and HT frame'
  print, '                                     i=1 -> Walen rel only'
  print, '                                     i=2 -> HT frame only'
  print, '                 p  -> Postscrip output'
  print, '                 q  -> TERMINATE'
  read, whatindex
  
  if whatindex eq 'q' then stop
  
; Set a search time range trmin, trmax, trvmin, trvmax
;   and array range (ibr, ier, ivbr, iver) to return to:
  if whatindex eq 'd' then begin
     timereqs, t0min,t0max,tmin,tmax,ib,ie,ivb,ive,success 
; print,ivb    
     if success eq 'none' then goto, cut
     ibr=ib & ier=ie & ivbr=ivb & iver=ive
     trmin=tmin & trmax=tmax & trvmin=tmin & trvmax=tmax
     setplotvar,ie,ib,ive,ivb,timeb,timev,exr,eyr,ezr,smo
     print,'Size timeb:',size(timeb)
     goto, cut
  endif
  
; reset search time range and arrays to maximum range 
  if whatindex eq 'u' then begin
     ib=ib0 & ie=ie0 & ivb=ivb0 & ive=ive0
     trmin=t0min & trmax=t0max & trvmin=t0min & trvmax=t0max
     setplotvar,ie,ib,ive,ivb,timeb,timev,exr,eyr,ezr,smo
     goto, cut
  endif

;  Set period for HT+Walen analysis delt and increment to shift this period
;     in minutes
  if whatindex eq 't' then begin
     print, 'Old period (minutes):', deltm
     print, 'Old increment (as fraction of period):', fdelt
     print, 'Input new values:'
     read, deltm, fdelt &  delt=deltm/60.
     goto, cut
  endif

  if whatindex eq 'h' then begin
     print, 'Old slope for HT search:', slht0, slht1
     print, 'Input new value:'
     read, slht0, slht1
     goto, cut
  endif

  if whatindex eq 'w' then begin
     print, 'Old slope for Walen search:', slwal0, slwal1
     print, 'Input new value:'
     read, slwal0, slwal1
     goto, cut
  endif

  if whatindex eq 'i' then begin
     print, 'Old value for itest:', itest
     print, 'Input new value:'
     read, itest
     goto, cut
  endif
  
  if whatindex eq 'g' then begin
     if smo eq 'y' then print, 'Smoothing velocity is presently on!'$ 
       else print, 'Smoothing velocity is presently off!'
     print, 'Switch smoothing on (y) or off (n):'
     read, smo
     if smo ne 'y' then smo='n'
     setplotvar,ie,ib,ive,ivb,timeb,timev,exr,eyr,ezr,smo
     goto, cut
 endif

  if whatindex eq 'p' then begin
     withps = 'y'
     !P.THICK=2.
     !X.THICK=1.5
     !Y.THICK=1.5
     !P.CHARTHICK=2.
     psname='plot'+string(itest,'(i1.1)')+'p'+string(countps,'(i2.2)')
     countps=countps+1 & set_plot,'ps'
      device,filename=psname+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
      goto, ps
  endif

  iv1a=intarr(1000) & iv2a=iv1a & ib1a=iv1a & ib2a=iv1a
  t1a=fltarr(1000) & t2a=t1a
  numint=0 & success=0
  iiter = 0
  tmin = trmin & tmax=tmin+delt
; Carry out ht and walen analysis: tmin, tmax  
  if whatindex eq 's' then begin
   while tmax le trmax do begin     
     indrange, trmin,trmax,tmin,tmax,ib1,ib2,iv1,iv2,success
     if success eq 'none' then goto, nextiter
     nv=iv2-iv1+1  & nbb=ib2-ib1+1
;     print, 't:',ib1,ib2,iv1,iv2
;     print, 'Variance Analysis for the Time Range:', tmin, tmax
;     print, '  Number of data points:', nv
  
; Determine the actual data set to be analyzed and give it physical 
;     (or normalized) units 
     nv=iv2-iv1+1  & nbb=ib2-ib1+1
     vxv = fltarr(nv) & vyv = vxv  & vzv = vxv
     baxv = vxv       & bayv = vxv & bazv = vxv
     exv = vxv        & eyv = vxv  & ezv = vxv
     nnv = vxv
     bxv = fltarr(nbb) & byv=bxv & bzv=bxv

     vxv=veqs(iv1:iv2,0) & vyv=veqs(iv1:iv2,1) & vzv=veqs(iv1:iv2,2)
     nnv=neqs(iv1:iv2)
     if smo eq 'y' then begin
         vxv=smooth(vxv,3) & vyv=smooth(vyv,3) & vzv=smooth(vzv,3)
         nnv=smooth(nnv,3) & endif
     baxv=bave(iv1:iv2,0) & bayv=bave(iv1:iv2,1) & bazv=bave(iv1:iv2,2)
     efieldeqs, vxv,vyv,vzv, baxv,bayv,bazv, exv,eyv,ezv
;    bxv=beqs(ib1:ib2,0) & byv=beqs(ib1:ib2,1) & bzv=beqs(ib1:ib2,2)
     tv=tmv(iv1:iv2) & tb=tmb(ib1:ib2) 

; Hoffmann Teller velocity and variance for Eht
     htcoor, nv, vxv,vyv,vzv, baxv,bayv,bazv, vht
     ehtfieldeqs, unitype,vht,baxv,bayv,bazv,ehxv,ehyv,ehzv
; Some stuff for plots testing the walen relation and the HT frame
     vmhtx=vxv-vht(0)  & vmhty=vyv-vht(1)  & vmhtz=vzv-vht(2)
     vmhp=fltarr(nv,3) & vmhp(*,0)=vmhtx & vmhp(*,1)=vmhty  & vmhp(*,2)=vmhtz
     valfv=vmhp 
      valfv(*,0)=baxv/sqrt(nnv)
      valfv(*,1)=bayv/sqrt(nnv)
      valfv(*,2)=bazv/sqrt(nnv)
      valfv=21.8*valfv
     ewh=vmhp & ewh(*,0)=exv & ewh(*,1)=eyv &  ewh(*,2)=ezv
     ewht=vmhp & ewht(*,0)=ehxv & ewht(*,1)=ehyv &  ewht(*,2)=ehzv
; Test the walen relation and the HT frame
     lfit0=poly_fit(valfv,vmhp,1,yfit,yband,sigma,a0)
     lfit1=poly_fit(ewh,ewht,1,yfit,yband,sigma,a1)

     ccoef0=findgen(2,2) & cstd=findgen(2,2) & creg=findgen(2)
     ccoef0(*,0)=lfit0(*) & ccoef0(*,1)=lfit1(*)
     cstd(0,0)=sqrt(a0(0,0)) & cstd(1,0)=sqrt(a0(1,1)) 
     cstd(0,1)=sqrt(a1(0,0)) & cstd(1,1)=sqrt(a1(1,1)) 
     creg(0)=correlate(valfv,vmhp) & creg(1)=correlate(ewh,ewht)


     case itest of
      0: if ( (abs(ccoef0(1,0)) gt slwal0) and (abs(ccoef0(1,0)) lt slwal1)$ 
            and (abs(ccoef0(1,1)) gt slht0) and $
                (abs(ccoef0(1,1)) lt slht1) )then begin
           print, 'Interval: ',cvhms(tmin),' - ',cvhms(tmax),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
           print, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
           print, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
           print, '  dHT velocity:', vht
           succes = 1
         endif
      1: if ( (abs(ccoef0(1,0)) gt slwal0) and $
            (abs(ccoef0(1,0)) lt slwal1) ) then begin
           print, 'Interval: ',cvhms(tmin),' - ',cvhms(tmax),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
           print, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
           print, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
           print, '  dHT velocity:', vht
           succes = 1
         endif
      2: if ( (abs(ccoef0(1,1)) gt slht0) and $
            (abs(ccoef0(1,1)) lt slht1) ) then begin
           print, 'Interval: ',cvhms(tmin),' - ',cvhms(tmax),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
           print, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
           print, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
           print, '  dHT velocity:', vht
           succes = 1
         endif
     endcase
     if succes eq 1 then begin
         iv1a(numint) = iv1 & iv2a(numint) = iv2 
         ib1a(numint) = ib1 & ib2a(numint) = ib2
         t1a(numint) = tmin & t2a(numint) = tmax
         numint=numint+1
     endif
     succes=0

nextiter:
     tmin = tmin+fdelt*delt  &  tmax=tmin+delt
     iiter = iiter+1
 endwhile

 ii=0
 for i=1,numint-1 do begin
     if t1a(i) lt t2a(ii) then begin
         t2a(ii)=t2a(i) ; iv2a(ii)=iv2a(i) & ib2a(ii)=ib2a(i)
         endif else begin
         ii=ii+1
         t1a(ii)=t1a(i) & t2a(ii)=t2a(i)
;         iv1a(ii)=iv1a(i) & iv2a(ii)=iv2a(i)
;         ib1a(ii)=ib1a(i) & ib2a(ii)=ib2a(i)
     endelse
 endfor
 totnumint=ii+1 & t1a=t1a(0:(totnumint-1)) & t2a=t2a(0:(totnumint-1)) 
 for i=0,totnumint-1 do print, 'Intervals:', cvhms(t1a(i)),' - ',cvhms(t2a(i))

ps:
  plotvarsum, trmin,trmax,timeb,timev,baxr,bayr,bazr,strnn,$
              totnumint,t1a,t2a,withps

  endif
  goto, cut

end

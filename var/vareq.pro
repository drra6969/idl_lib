; PROGRAM to read and plot satellite data
;----------------------------------------
FUNCTION XTICKS, axis, index, value
COMMON plaxis,tax
print,'tax:',tax
if tax eq 'h' then begin
 hour = LONG(value)
 minute = LONG(60.*(value-hour)) 
 sec = 3600*value mod 60
endif
if (tax eq 'm') or (tax eq 'ms') then begin
 print, 'value:',LONG(value)
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

  pi = 3.14159265536 & phi0=0.0 & phi=0.0  & time2d=0.0 
  
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
  
; READ INPUT DATA FOR NSAT SATELLITES
;------------------------------------
;    readeqs,day          ; proton data only
    readeqso2,day          ; proton and O+data
;    readeqso,day          ; proton and O+data
;   restore, file='980310_GSE_1030_1110.dat'
;   restore, file='980310_GSE_1150_1220.dat'
;   restore, file='980310_GSE_1250_1345.dat'

;     neqs=4.*neqs
;    readeqhr,day          ; high resolution proton data only
    strnn='EQUATOR S, DAY: '+day
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

; Reference time range
    ibr=ib0 & ier=ie0 & ivbr=ivb0 & iver=ive0
    trmin=tmin & trmax=tmax & trvmin=tmin & trvmax=tmax
    
    
; 2. Open some windows before starting menues:
;---------------------------------------------
  window,0,xsize=640,ysize=512,title='Overview Plot'
  window,1,xsize=640,ysize=512,title='Variance Plots'
  window,2,xsize=640,ysize=512,title='Var Data'

; 3. Menu:
;---------
cut: 
  if withps eq 'y' then  begin 
    print, 'Postscript device is still open! Close postscript device? <y> or <n>'
    read, closeps
    if closeps eq 'y' then begin
         withps = 'n' & closeps='n' & device,/close & set_plot,'x'
         !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
  endif

  print, 'Maximum Time Range for B:', tmb(0), tmb(ntb-1)
  print, 'Maximum Time Range for Plasma:', tmv(0), tmv(ntv-1)
  print, 'Present Range:', tmin,tmax
  
  print, '        Time range for overview plots (t)   :',tmin, tmax
  print, '        Actualime range for variance analysis (v):',tvmin, tvmax
  print, '        Set time range for variance analysis (v):',tv0min, tv0max
  print, '        Reference Time range:',trmin, trmax
  print, '        Maximum Time range:',t0min, t0max
  print, '        Variance plots use time range:  ', plotvars
  print, '        Variance plots (h) and (r) use basis from:  ', ebst
                         
  print, 'OPTIONS:         p  -> Postscrip output'
  print, '                 d  -> Define a reference frame to return to'
  print, '                 f  -> Use reference times'
  print, '                 t  -> Time range for plots'
  print, '                 u  -> Set time range back to total range'
  print, '                 v  -> Set time range for VARIANCE'
  print, '                 vt -> Variance plots for time range t (so)'
  print, '                 vv -> Variance plots for reference range (v) only'
  print, '                 vw -> Variance plots for variance time (v) only'
  print, ' VARIANCE plots:  '
  print, '                 s  -> Standard (predefined) variance plots'
  print, '                       with a regular plot in '+ebst+' variance coordinates'
  print, '                 h  -> Hodograms for selected BASIS coord.'
  print, '                 r  -> Regular plots V,B,E for selected BASIS coord.'
  print, '                 w  -> Plot walen relation and print data'
  print, ' Select BASIS for inputs (h) and (r):  '
  print, '                 bo -> original Basis'
  print, '                 bb -> Basis using Magnetic field variance'
  print, '                 be -> Basis using Electric field variance'
  print, '                 beh-> Basis using HT Electric field variance'
  print, '                 bv -> Basis using Velocity variance'
  print, '                 q  -> TERMINATE'
  read, whatindex
  
  if whatindex eq 'p' then begin
     withps = 'y'
     !P.THICK=2.
     !X.THICK=1.5
     !Y.THICK=1.5
     !P.CHARTHICK=2.
     psname='testeqs'+string(countps,'(i2.2)')
     countps=countps+1 
     whatindex=whatold        ; to make a postscript file of the last choice
     if whatindex eq 't' then whatindex=''  ; unless this was to set a new time range
  endif
  whatold=whatindex
  print,'INDICES:',whatindex,whatold
  
; Set a reference time range trmin, trmax, trvmin, trvmax
;   and array range (ibr, ier, ivbr, iver) to return to:
  if whatindex eq 'd' then begin
     timereqs, t0min,t0max,tmin,tmax,ib,ie,ivb,ive,success     
     if success eq 'none' then goto, cut
     ibr=ib & ier=ie & ivbr=ivb & iver=ive
     trmin=tmin & trmax=tmax & trvmin=tmin & trvmax=tmax
     vardat='absent'
;     if (vardat eq 'present') and (plotvars eq 't') then goto, plotvart
  endif
  
; set time range for plots: tmin, tmax  
  if whatindex eq 't' then begin
     timereqs, t0min,t0max,tmin,tmax,ib,ie,ivb,ive,success
     if success eq 'none' then goto, cut
     vardat='absent'
;     if (vardat eq 'present') and (plotvars eq 't') then goto, plotvart
      print, 't:',ib,ie,ivb,ive
  endif
  
; reset time range and arrays to maximum range 
  if whatindex eq 'u' then begin
      tmin=t0min & tmax=t0max
      tvmin=tmin & tvmax=tmax
      ib=ib0 & ie=ie0 & ivb=ivb0 & ive=ive0
  endif
; reset time range and arrays to reference range
  if whatindex eq 'f' then begin
      tmin=trmin & tmax=trmax
      tvmin=tmin & tvmax=tmax
      ib=ibr & ie=ier & ivb=ivbr & ive=iver
      print, 'f:',ib,ie,ivb,ive
     vardat='absent'
  endif
  
  if whatindex eq 'q' then stop
 
  if (vardat ne 'present') and $
    ( (whatindex eq 'vt') or (whatindex eq 'vv') or (whatindex eq 's') or $
      (whatindex eq 'h') or (whatindex eq 'r') or (whatindex eq 'bb') or $
      (whatindex eq 'be') or (whatindex eq 'beh') or (whatindex eq 'bv') or $
      (whatindex eq 'w') )   then begin
      print, 'Determine Variance data first!!!! -> v'
      goto, cut
  endif

  
; Determine the actual data set to be analyzed and give it physical 
;     (or normalized) units
; 
    bxr=fltarr(ie-ib+1) & byr=bxr & bzr=bxr
    bxr=beqs(ib:ie,0)  & byr=beqs(ib:ie,1) & bzr=beqs(ib:ie,2)
    babr=beqstot(ib:ie) 
    vxr=fltarr(ive-ivb+1) & vyr=vxr & vzr=vxr
    baxr=vxr & bayr=vxr & bazr=vxr
    baxr=bave(ivb:ive,0) & bayr=bave(ivb:ive,1) & bazr=bave(ivb:ive,2) 
    vxr=veqs(ivb:ive,0)  & vyr=veqs(ivb:ive,1) & vzr=veqs(ivb:ive,2)
    rhor=neqs(ivb:ive) & pr=peqs(ivb:ive)      
    ptotr=peqs(ivb:ive)+pbave(ivb:ive) & pbr=pbave(ivb:ive) 
    tempr=teqs(ivb:ive) & beta=peqs(ivb:ive)/pbeqs(ivb:ive)
    timeb=tmb(ib:ie) & timev=tmv(ivb:ive)
    efieldeqs, vxr,vyr,vzr, baxr,bayr,bazr, exr,eyr,ezr


  if (whatindex eq 'v' or whatindex eq 'vw' or whatindex eq 't' $
      or whatindex eq 'f') then begin
; CARRY OUT THE VARIANCE ANALYSIS
; -------------------------------
    if (whatindex eq 'v') then begin
       timereqs,  tmin,tmax,tv0min,tv0max,ib01,ib02,iv01,iv02,success
       tvmin=tv0min & tvmax=tv0max 
       ib1=ib01 & ib2=ib02 & iv1=iv01 & iv2=iv02
       varian='present'
    endif   
    if (whatindex eq 'vw') then begin
       if ((varian eq 'present') and (tmin le tv0min)$
                                 and (tmax ge tv0max)) then begin
         tvmin=tv0min & tvmax=tv0max 
         ib1=ib01 & ib2=ib02 & iv1=iv01 & iv2=iv02
       endif else begin
         print, 'Time for variance not defined or in range!!!'
         varian='absent' 
         goto, cut
       endelse
    endif
    if (whatindex eq 't' or whatindex eq 'f') then begin
      tvmin=tmin & tvmax=tmax 
      ib1=ib & ib2=ie & iv1=ivb & iv2=ive
    endif

    
    nv=iv2-iv1+1  & nbb=ib2-ib1+1
    ib1=ib1-ib & ib2=ib2-ib
    iv1=iv1-ivb & iv2=iv2-ivb
    if success eq 'none' then goto, cut
    vardat='present'
    print, 'Variance Analysis for the Time Range:', tvmin, tvmax
    print, '  Number of data points:', nv
    tv=timev(iv1:iv2) & tb=timeb(ib1:ib2) 
    
; Variance for V, B, and E
    vxv=vxr(iv1:iv2) & vyv=vyr(iv1:iv2) & vzv=vzr(iv1:iv2)
    baxv=baxr(iv1:iv2) & bayv=bayr(iv1:iv2) & bazv=bazr(iv1:iv2)
    bxv=bxr(ib1:ib2) & byv=byr(ib1:ib2) & bzv=bzr(ib1:ib2)
    exv=exr(iv1:iv2) & eyv=eyr(iv1:iv2) & ezv=ezr(iv1:iv2)
    nnv=rhor(iv1:iv2)
    varmat,nv,0.01*nnv*vxv,0.01*nnv*vyv,0.01*nnv*vzv,vav,vmat
    vav(0)=total(vxv)/nv & vav(1)=total(vyv)/nv & vav(2)=total(vzv)/nv
    varmat,nbb,bxv,byv,bzv,bav,bmat
    varmat,nv,exv,eyv,ezv,eav,emat
    eigen, vmat,evv1,evv2,evv3,ewv
    eigen, bmat,evb1,evb2,evb3,ewb
    eigen, emat,eve1,eve2,eve3,ewe
    sorteigenb, evb1,evb2,evb3
    sorteigen, evb1,evb2,evb3,evv1,evv2,evv3
    sorteigen, evb1,evb2,evb3,eve1,eve2,eve3
; Hoffmann Teller velocity and variance for Eht
    htcoor, nv, vxv,vyv,vzv, baxv,bayv,bazv, vht
    ehtfieldeqs, unitype,vht,baxr,bayr,bazr,ehxr,ehyr,ehzr
    ehxv=ehxr(iv1:iv2) & ehyv=ehyr(iv1:iv2) & ehzv=ehzr(iv1:iv2)
    varmat,nv,ehxv,ehyv,ehzv,ehav,ehmat
    eigen, ehmat,eveh1,eveh2,eveh3,eweh
    sorteigen, evb1,evb2,evb3,eveh1,eveh2,eveh3
; Variance for Vht
    vmhtx=vxv-vht(0)  & vmhty=vyv-vht(1)  & vmhtz=vzv-vht(2)
    varmat,nv,vmhtx,vmhty,vmhtz,vmhtav,vmhtmat
    eigen, vmhtmat,evmhtv1,evmhtv2,evmhtv3,ewmhtv
    sorteigen, evb1,evb2,evb3,evmhtv1,evmhtv2,evmhtv3
; Some stuff for plots testing the walen relation and the HT frame
    vmhp=fltarr(nv,3) & vmhp(*,0)=vmhtx & vmhp(*,1)=vmhty  & vmhp(*,2)=vmhtz
    valfv=vmhp 
      valfv(*,0)=baxv/sqrt(nnv)
      valfv(*,1)=bayv/sqrt(nnv)
      valfv(*,2)=bazv/sqrt(nnv)
      valfv=21.8*valfv
    ewh=vmhp & ewh(*,0)=exv & ewh(*,1)=eyv &  ewh(*,2)=ezv
    ewht=vmhp & ewht(*,0)=ehxv & ewht(*,1)=ehyv &  ewht(*,2)=ehzv
; Testing the walen relation and the HT frame
    if withps eq 'n' then begin 
      wset,1
      waleneqs, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),$
                ccoef0,cstd,creg,strnn
      wset,2
      printeqs, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             ccoef0,cstd,creg,strnn
    endif
; Determine actual velocity and field components for variance plots        
    if ebasis eq 'v' then begin
       ebst='V' & eb1=evv1 & eb2=evv2 & eb3=evv3 & eb=ewv & endif
    if ebasis eq 'b' then begin
       ebst='B' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewb & endif
    if ebasis eq 'e' then begin
       ebst='E' & eb1=eve1 & eb2=eve2 & eb3=eve3 & eb=ewe & endif
    if ebasis eq 'eh' then begin
       ebst='Eht' & eb1=eveh1 & eb2=eveh2 & eb3=eveh3 & eb=eweh & endif
    if ebasis eq 'o' then begin
       ebst='Orig' & eb1=evo1 & eb2=evo2 & eb3=evo3 & eb=ewo & endif
    if plotvars eq 'v' then goto, plotvarv
    if plotvars eq 't' then goto, plotvart
    if (whatindex ne 't') then goto, cut
  endif
  
  
  if (whatindex eq 'vt') then begin
plotvart:
    plotvars='t'
      np=ive-ivb+1  & ntt=ie-ib+1  
      vp=fltarr(np,3) & bp=fltarr(ntt,3) & ep=vp & ehp=vp & rhop=fltarr(np)
      tpv=timev & tpb=timeb  & rhop=rhor
      itv1=iv1 & itv2=iv2 & itb1=ib1 & itb2=ib2
      vp(*,0) =vxr  & vp(*,1) =vyr  &  vp(*,2)=vzr
      bp(*,0) =bxr  & bp(*,1) =byr  &  bp(*,2)=bzr
      ep(*,0) =exr  & ep(*,1) =eyr  &  ep(*,2)=ezr
      ehp(*,0)=ehxr & ehp(*,1)=ehyr &  ehp(*,2)=ehzr
      if (whatindex ne 't') then goto, cut    
  endif


  if (whatindex eq 'vv') then begin
plotvarv:
    plotvars='v'
    np=nv &  ntt=nbb
    vp=fltarr(np,3) & bp=fltarr(ntt,3) & ep=vp & ehp=vp & rhop=fltarr(np)
    tpv=tv & tpb=tb  & rhop=rhor(iv1:iv2)
    itv1=0 & itv2=nv-1 & itb1=0 & itb2=nbb-1
    vp(*,0)=vxv & vp(*,1)=vyv &  vp(*,2)=vzv
    bp(*,0)=bxv & bp(*,1)=byv &  bp(*,2)=bzv
    ep(*,0)=exv & ep(*,1)=eyv &  ep(*,2)=ezv
    ehp(*,0)=ehxv & ehp(*,1)=ehyv &  ehp(*,2)=ehzv
    if (whatindex ne 't') then goto, cut
  endif


; Determine basis for variance plots
  if (whatindex eq 'bv') then begin
    ebasis='v' & ebst='V' & eb1=evv1 & eb2=evv2 & eb3=evv3 & eb=ewv & goto, cut 
  endif
  if (whatindex eq 'bb') then begin
    ebasis='b' & ebst='B' & eb1=evb1 & eb2=evb2 & eb3=evb3 & eb=ewb & goto, cut
  endif
  if (whatindex eq 'be') then begin
    ebasis='e' & ebst='E' & eb1=eve1 & eb2=eve2 & eb3=eve3 & eb=ewe & goto, cut
  endif
  if (whatindex eq 'beh') then begin
    ebasis='eh' & ebst='Eht' & eb1=eveh1 & eb2=eveh2 & eb3=eveh3 & eb=eweh 
    goto, cut
  endif
  if (whatindex eq 'bo') then begin
    ebasis='o' & ebst='Orig' & eb1=evo1 & eb2=evo2 & eb3=evo3 & eb=ewo 
    goto, cut
  endif


  if (whatindex eq 'w') then begin
    if withps ne 'y' then begin 
    wset,1
    waleneqs, tpv,vmhp,valfv,ewh,ewht,vht,tpv(0),tpv(nv-1),$
              ccoef0,cstd,creg,strnn
    wset,2
    printeqs, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             ccoef0,cstd,creg,strnn
    endif
    if withps eq 'y' then begin
      set_plot,'ps' & wcount=wcount+1
      device,filename=psname+'w'+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
      waleneqs, tv,vmhp,valfv,ewh,ewht,vht,tv(0),tv(nv-1),$
                ccoef0,cstd,creg,strnn
      printeqs, nv, tvmin,tvmax,evv1,evv2,evv3,ewv,vav,evb1,evb2,evb3,ewb,bav,$
             eve1,eve2,eve3,ewe,eav,eveh1,eveh2,eveh3,eweh,ehav,vht,$
             ccoef0,cstd,creg,strnn
      withps = 'n' & closeps='n' & device,/close & set_plot,'x'
      !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
    goto, cut
  endif
  
   
; Standard (predetermined) variance plots
  if (whatindex eq 's') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      set_plot,'ps' & scount=scount+1
      device,filename=psname+'s'+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
   !p.multi=[0,2,2,0,0]    
    hodoeqs, ntt,tpb,tvmin,tvmax,itb1,itb2,bp,evb1,evb2,evb3,ewb,$
          'Magnetic Field','B',strnn,0,vht
	  
    hodoeqs, np,tpv,tvmin,tvmax,itv1,itv2,vp,evv1,evv2,evv3,ewv,$
          'Velocity','V',strnn,1,vht
    print,'Continue with electric field ?'
    read, conti
    if (conti eq 'y') or (conti eq '') then begin
      !p.multi=[0,2,2,0,0]    
      hodoeqs, np,tpv,tvmin,tvmax,itv1,itv2,ep,eve1,eve2,eve3,ewe,$
          'Electric Field','E',strnn,0,vht
      hodoeqs, np,tpv,tvmin,tvmax,itv1,itv2,ehp,eveh1,eveh2,eveh3,eweh,$
          'HT Electric Field','Eht',strnn,1,vht
    endif
    print,'Continue with regular plot?'
    read, conti
    if (conti eq 'y') or (conti eq '') then  begin
      !p.multi=[0,1,3,0,0]    
      plobveqs1, np,tpv,ntt,tpb,tvmin,tvmax,bp,vp,ep,eb1,eb2,eb3,eb,ebst,$
                strnn,xtit,vht
    endif
    if withps eq 'y' then begin
      withps = 'n' & closeps='n' & device,/close & set_plot,'x'
      !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
    goto, cut
  endif
  
    
; Hodograms in variance coordinates
  if (whatindex eq 'h') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      set_plot,'ps' & hcount=hcount+1
      device,filename=psname+'h'+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
   !p.multi=[0,2,2,0,0]    
    hodoeqs, ntt,tpb,tvmin,tvmax,itb1,itb2,bp,eb1,eb2,eb3,eb,$
          'Magnetic Field',ebst,strnn,0,vht
    hodoeqs, np,tpv,tvmin,tvmax,itv1,itv2,vp,eb1,eb2,eb3,eb,$
          'Velocity',ebst,strnn,1,vht
    print,'Continue with electric field plots?'
    read, conti
    if (conti eq 'y') or (conti eq '') then begin
      !p.multi=[0,2,2,0,0]    
      hodoeqs, np,tpv,tvmin,tvmax,itv1,itv2,ep,eb1,eb2,eb3,eb,$
          'Electric Field',ebst,strnn,0,vht
      hodoeqs, np,tpv,tvmin,tvmax,itv1,itv2,ehp,eb1,eb2,eb3,eb,$
          'HT Electric Field',ebst,strnn,1,vht
    endif
    if withps eq 'y' then begin
      withps = 'n' & closeps='n' & device,/close & set_plot,'x'
      !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
    goto, cut
  endif
  
    
; Regular variance plots
  if (whatindex eq 'r') then begin
    if withps eq 'n' then wset,1
    if withps eq 'y' then begin
      set_plot,'ps' & rcount=rcount+1
      device,filename=psname+'r'+'.ps'
      device, /inches, xsize=8., ysize=4., scale_factor=1.0, xoffset=0.5
      device, /landscape, /times, /bold, font_index=3
    endif
    !p.multi=[0,1,3,0,0]    
    plobveqs1, np,tpv,ntt,tpb,tvmin,tvmax,bp,vp,ep,eb1,eb2,eb3,eb,ebst,$
              strnn,xtit,vht
    if withps eq 'y' then begin
      withps = 'n' & closeps='n' & device,/close & set_plot,'x'
      !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
    goto, cut
  endif
  

; Finally the overview plots:
; for simulation frame
  

  if withps eq 'n' then wset,0
  if withps eq 'y' then begin
     set_plot,'ps' & device,filename=psname+'.ps'
     device,/portrait
     device,/inches,xsize=8.,ysize=10.,scale_factor=1.0,xoffset=0.5,yoffset=0.5
     !P.THICK=2. & !X.THICK=1.5 & !Y.THICK=1.5 & !P.CHARTHICK=2.
  endif
  
;  pltvpar, tmin,tmax,timeb,timev,baxr,bayr,bazr,strnn
  plt0eqs, tmin,tmax,timeb,timev,baxr,bayr,bazr,strnn

    if withps eq 'y' then begin
      withps = 'n' & closeps='n' & device,/close & set_plot,'x'
      !P.THICK=1. & !X.THICK=1. & !Y.THICK=1. & !P.CHARTHICK=1.
    endif
  goto, cut

end

PRO readhiab, day
; This program reads the equatore low resolution data  data.
; magnetic field is in nT
COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv

nt=100000	  ; Maximum possible array size (actual data may be smaller)

time = fltarr(nt) & tmb=time & tmv=time & beqstot=time
beqs=fltarr(nt,3) & veqs=beqs 
neqs=fltarr(nt) & teqs=neqs & peqs=neqs & pbeqs=neqs
dumm='' & stuff='  '

 print, 'READING magnetic data from file C3_010805gsepos.dat'
 print, '  NOTE: Remember to remove calibration entry'

; READ MAGNETIC FIELD
 openr, 9, '20010805/C3_010805gsepos.dat'
 readf, 9, dumm
 print, dumm  & day=strmid(dumm,0,10)
 print,'Date:',day
 
 formb='(a11,i2.2,a1,i2.2,a1,f6.3,a1,3f9.4)'
 i=0L
; while i lt 10000 do begin
 while not eof(9) do begin
  readf, 9, format=formb,stuff,hr,dumm,mt,dumm,sc,dumm,bxs,bys,bzs
;  print, stuff, sc, bxs, bys, bzs
  tmb(i)=float(hr+mt/60.+sc/3600.)
  beqs(i,0)=bxs & beqs(i,1)=bys & beqs(i,2)=bzs 
  beqstot(i)=bxs*bxs+bys*bys+bzs*bzs
  pbeqs(i)=0.0004*beqstot(i)             ; in nPascal
  beqstot(i)=sqrt(beqstot(i))
  itot=i & i=i+1
 endwhile
 
; resize the arrays to remove any zeros at the end
 temptime=tmb(0:itot) & Btemp=beqs(0:itot,*) & btottemp=beqstot(0:itot)
 tmb=fltarr(itot+1) & beqs=fltarr(itot+1,3) & beqstot=tmb
 tmb=temptime & beqs=Btemp & beqstot=btottemp
 ntb=itot+1
 close,9
; print, stuff, sc, bxs, bys, bzs 
 
; READ PLASMA
 openr, 7, 'Objetcs_3/obj6.asc'
 for i=1,91 do begin
   readf, 7, dumm
 endfor
 i=0L
 formn='(a11,i2.2,a1,i2.2,a1,f6.3,a2,E13.4)'
 while not eof(7) do begin
  readf, 7, format=formn, dumm,hr,dumm,mt,dumm,sc,dumm,rhos
  tmv(i)=hr+(mt/60.)+(sc/3600.)
  neqs(i)=rhos 
  itot2=i & i=i+1
 endwhile
 close, 7
 print,dumm,hr,dumm,mt,dumm,sc,dumm,rhos

 openr, 8, 'Objetcs_3/obj7.asc'
 for i=1,123 do begin
   readf, 8, dumm
 endfor
 i=0L
 formn='(a11,i2.2,a1,i2.2,a1,f6.3,a2,E11.4,a1,E11.4,a1,E11.4)'
; print,hr,mt,sc,vxs,vys,vzs
 while not eof(8) do begin
  readf, 8, format=formn, dumm,hr,dumm,mt,dumm,sc,dumm,vxs,dumm,vys,dumm,vzs
  tmv(i)=hr+(mt/60.)+(sc/3600.)
  veqs(i,0)=vxs & veqs(i,1)=vys & veqs(i,2)=vzs
  itot3=i & i=i+1
 endwhile
 close, 8
 print,dumm,hr,dumm,mt,dumm,sc,dumm,vxs,vys,vzs

 openr, 8, 'Objetcs_3/obj8.asc'
 for i=1,123 do begin
   readf, 8, dumm
   print, dumm
 endfor
 i=0L
 formn='(a11,i2.2,a1,i2.2,a1,f6.3,a2,E11.4,a1,E11.4,a1,E11.4)'
; print,hr,mt,sc,vxs,vys,vzs
 while not eof(8) do begin
  readf, 8, format=formn, dumm,hr,dumm,mt,dumm,sc,dumm,txs,dumm,tys,dumm,tzs
  tmv(i)=hr+(mt/60.)+(sc/3600.)
; T in 10^6 K -> eV
;  teqs(i)=86.2*temp
; T in eV
  teqs(i)=(txs+tys+tzs)/3.
  itot4=i & i=i+1
 endwhile
 close, 8
 peqs(i)=0.00016*neqs(i)*teqs(i)
 print,dumm,hr,dumm,mt,dumm,sc,dumm,txs,tys,tzs

 print,itot2,itot3,itot4
 if (itot2 ne itot3) or (itot2 ne itot4) then begin
   print,'CHECK SIZE OF PLASMA DATA!!!!'
   stop
 endif

 tempv=veqs(0:itot2,*) & temptm=tmv(0:itot2) & temprho=neqs(0:itot2)
 temptemp=teqs(0:itot2)
 tempp=0.00016*temprho*temptemp
 veqs=fltarr(itot2+1,3) & tmv=fltarr(itot2+1)
 neqs=tmv & peqs=tmv & teqs=tmv
 veqs=tempv & tmv=temptm & neqs=temprho & peqs=tempp & teqs=temptemp

 ntv=itot2+1

;  print, hr, mt, sc
;  print, dumm,stuff, rhos,vxs, vys, vzs, txs, tys,tzs, temps
;  print, format='(5E13.4)', rhos,vxs, vys, vzs, temps
  
return
end

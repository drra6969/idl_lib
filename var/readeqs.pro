PRO readeqs, day
; This program reads the equatore low resolution data  data.
; magnetic field is in nT
COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv

nt=100000	  ; Maximum possible array size (actual data may be smaller)

time = fltarr(nt) & tmb=time & tmv=time & beqstot=time
beqs=fltarr(nt,3) & veqs=beqs 
neqs=fltarr(nt) & teqs=neqs & peqs=neqs & pbeqs=neqs
dumm='' & stuff='  '

 day1='980224'
 day2='980309'
 day3='980310'
 print, 'READ data by providing date with  FORMAT yymmdd'
 print, '     or choose from the following dates: '+day1+'  enter (1)'
 print, '                                         '+day2+'  enter (2)'
 print, '                                         '+day3+'  enter (3)'
 read, dumm
 if dumm eq '1' then day=day1
 if dumm eq '2' then day=day2
 if dumm eq '3' then day=day3

; READ MAGNETIC FIELD
 openr, 9, 'PP19'+string(day,'(i6)')+'_GSE.mag'
 readf, 9, dumm
 print, dumm
 i=0L
; while i lt 10000 do begin
 while not eof(9) do begin
  readf, 9, format='(a15,4f15.3)',stuff, sc, bxs, bys, bzs
; print, stuff, sc, bxs, bys, bzs
  tmb(i)=sc/1000./3600
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
 print, stuff, sc, bxs, bys, bzs 
 print,string(day,'(i6)')
 
; READ PLASMA
 openr, 7, 'p10_'+strmid(string(day,'(i6)'),2,4)+'_moments.dat'
 print,'p10_'+strmid(string(day,'(i6)'),2,4)+'_moments.dat'
 for i=1,11 do begin
   readf, 7, dumm
 endfor
 print, dumm
 i=0L
 while not eof(7) do begin
; while i lt 1000 do begin
  readf, 7, format='(a10, a12, a2, 8E13.4)', dumm,stuff, dumm, rhos,$
	    vxs, vys, vzs, txs, tys,tzs, temp
  hr=float(strmid(stuff,0,2)) & mt=float(strmid(stuff,3,2))
  sc=float(strmid(stuff,6,6))
  tmv(i)=hr+(mt/60)+(sc/(60*60))
  
  veqs(i,0)=vxs & veqs(i,1)=vys & veqs(i,2)=vzs & neqs(i)=rhos 
; T in 10^6 K -> eV
;  teqs(i)=86.2*temp
; T in eV
  teqs(i)=temp
  peqs(i)=0.00016*neqs(i)*teqs(i)
  itot2=i & i=i+1
 endwhile
 tempv=veqs(0:itot2,*) & temptm=tmv(0:itot2) & temprho=neqs(0:itot2)
 tempp=peqs(0:itot2) & temptemp=teqs(0:itot2)
 veqs=fltarr(itot2+1,3) & tmv=fltarr(itot2+1)
 neqs=tmv & peqs=tmv & teqs=tmv
 veqs=tempv & tmv=temptm & neqs=temprho & peqs=tempp & teqs=temptemp
 close, 7
 ntv=itot2+1

;  print, hr, mt, sc
;  print, dumm,stuff, rhos,vxs, vys, vzs, txs, tys,tzs, temps
;  print, format='(5E13.4)', rhos,vxs, vys, vzs, temps
  
return
end

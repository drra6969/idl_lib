PRO readeqhr, day
; This program reads the equatore s data.
; magnetic field is in nT
COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv

nt=1000000	  ; Maximum possible array size (actual data may be smaller)

time = fltarr(nt) & tmb=time & tmv=time & beqstot=time
beqs=fltarr(nt,3) & veqs=beqs 
neqs=fltarr(nt) & teqs=neqs & peqs=neqs & pbeqs=neqs
dumm='' & stuff='  '

 int1='980310_GSE_1030_1110'   & day1='980310'
 mag1='HR19980310_GSE_1030_1110.mag'
 pp1 ='p_9803101020_1110_recal'

 int2='980310_GSE_1150_1220'   & day2='980310'
 mag2='HR19980310_GSE_1150_1220.mag'
 pp2 ='p_9803101145_1205_recal'
 int3='980310_GSE_1250_1345'   & day3='980310'
 mag3='HR19980310_GSE_1250_1350.mag'
 pp3 ='p_9803101250_1345_recal'


 print, 'READ data '
 print, '     or choose from the following dates: '+int1+'  enter (1)'
 print, '     or choose from the following dates: '+int2+'  enter (2)'
 print, '     or choose from the following dates: '+int3+'  enter (3)'
 read, dumm
 if dumm eq '1' then begin
   magfile=mag1  & ppfile=pp1 & intfile=int1 & day=day1
 endif
 if dumm eq '2' then begin
   magfile=mag2  & ppfile=pp2 & intfile=int2 & day=day2
 endif
 if dumm eq '3' then begin
   magfile=mag3  & ppfile=pp3 & intfile=int3 & day=day3
 endif

; READ MAGNETIC FIELD
 openr, 9, magfile
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
 openr, 7, ppfile
; print,'p10_'+strmid(string(day,'(i6)'),2,4)+'_moments.dat'
; for i=1,11 do begin
   readf, 7, dumm
; endfor
 print, dumm
 i=0L
 while not eof(7) do begin
; while i lt 1000 do begin
  readf, 7, sc, rhos,temp, vxs, vys, vzs
  sc=float(sc)
  tmv(i)=(sc/(60.*60.))
  
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

save,file=intfile+'.dat',/xdr,day,beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            tmb,tmv,ntb,ntv
print, 'fdata file saved'
  
return
end

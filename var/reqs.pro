readeqs.pro
; This program reads the equatore s data.

COMMON eqs, beqs,veqs,neqs,teqs,peqs,beqstot,it1,it2

nt=100000	  ; Maximum possible array size (actual data may be smaller)

time = fltarr(nt) & tme=time & tm=time & beqstot=time
beqs=fltarr(nt,3) & veqs=beqs & neqs=fltarr(nt) & teqs=neqs & peqs=neqs
dumm='' & stuff='  '

 print, 'Which day (specify by day of year)?'
 read, doy
 openr, 9, 'PP19'+string(doy,'(i6)')+'_GSE.mag'
 readf, 9, dumm
print, dumm
 i=0L
 while i lt 5 do begin
; while not eof(9) do begin
  readf, 9, format='(a15,4f15.3)',stuff, sc, bxs, bys, bzs
; print, stuff, sc, bxs, bys, bzs
  tme(i)=sc/1000./3600
  beqs(i,0)=bxs & beqs(i,1)=bys & beqs(i,2)=bzs & beqstot(i)=bxs*bxs+bys*bys+bzs*bzs
  itot=i & i=i+1
 endwhile
; resize the arrays to remove any zeros at the end
 temptime=tme(0:itot) & Btemp=beqs(0:itot,*) & btottemp=beqstot(0:itot)
 tme=fltarr(itot+1) & beqs=fltarr(itot+1,3) & beqstot=tme
 tme=temptime & beqs=Btemp & beqstot=btottemp
 it1=itot+1
 close,9
 print, stuff, sc, bxs, bys, bzs
 
 print,string(doy,'(i6)')
 
 openr, 7, 'p10_'+strmid(string(doy,'(i6)'),2,4)+'_moments.dat'
 print,'p10_'+strmid(string(doy,'(i6)'),2,4)+'_moments.dat'
 for i=1,11 do begin
   readf, 7, dumm
 endfor
 print, dumm
 i=0L
; while not eof(7) do begin
 while i lt 10 do begin
  readf, 7, format='(a10, a12, a2, 8E13.4)', dumm,stuff, dumm, rhos,$
	    vxs, vys, vzs, txs, tys,tzs, temp
  hr=float(strmid(stuff,0,2)) & mt=float(strmid(stuff,3,2))
  sc=float(strmid(stuff,6,6))
  tm(i)=hr+(mt/60)+(sc/(60*60))
  
  veqs(i,0)=vxs & veqs(i,1)=vys & veqs(i,2)=vzs & tm(i)=hms
  peqs(i)=temp*rhos & neqs(i)=rhos & teqs(i)=temp
  itot2=i & i=i+1
 endwhile
 tempv=veqs(0:itot2,*) & temptm=tm(0:itot2) & temprho=neqs(0:itot2)
 tempp=peqs(0:itot2) & temptemp=teqs(0:itot2)
 veqs=fltarr(itot2+1,3) & tm=fltarr(itot2+1) & neqs=tm & peqs=tm & teqs=tm
 veqs=tempv & tm=temptm & neqs=temprho & peqs=tempp & teqs=temptemp
 close, 7
 it2=itot2+1

;  print, hr, mt, sc
;  print, dumm,stuff, rhos,vxs, vys, vzs, txs, tys,tzs, temps
;  print, format='(5E13.4)', rhos,vxs, vys, vzs, temps
  
return
end

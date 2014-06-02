PRO readeqso2, day
; This program reads the equatore s data. protons and oxygen
; magnetic field is in nT
COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv
COMMON eqso, veqso,neqso,teqso,peqso,tmo,nto

nt=100000	  ; Maximum possible array size (actual data may be smaller)

time = fltarr(nt) & tmb=time & tmv=time & tmo=time & beqstot=time
beqs=fltarr(nt,3) & veqs=beqs & veqso=beqs 
neqs=fltarr(nt) & teqs=neqs & peqs=neqs & pbeqs=neqs
neqso=neqs & teqso=neqs  & peqso=neqs 
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
 
; READ PROTON MOMENTS
 openr, 7, 'ph_'+strmid(string(day,'(i6)'),2,4)+'_moments.dat'
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

; READ O+ MOMENTS
 openr, 7, 'po_'+strmid(string(day,'(i6)'),2,4)+'_moments.dat'
 print,'p16_'+strmid(string(day,'(i6)'),2,4)+'_moments.dat'
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
  tmo(i)=hr+(mt/60)+(sc/(60*60))
  
  veqso(i,0)=vxs & veqso(i,1)=vys & veqso(i,2)=vzs & neqso(i)=rhos 
; T in 10^6 K -> eV
;  teqso(i)=86.2*temp
; T in eV
  teqso(i)=temp
  peqso(i)=0.00016*neqso(i)*teqso(i)
  itot2=i & i=i+1
 endwhile
 tempv=veqso(0:itot2,*) & temptm=tmo(0:itot2) & temprho=neqso(0:itot2)
 tempp=peqso(0:itot2) & temptemp=teqso(0:itot2)
 veqso=fltarr(itot2+1,3) & tmo=fltarr(itot2+1)
 neqso=tmo & peqso=tmo & teqso=tmo
 veqso=tempv & tmo=temptm & neqso=temprho & peqso=tempp & teqso=temptemp
 close, 7
 nto=itot2+1
;  print, hr, mt, sc
;  print, dumm,stuff, rhos,vxs, vys, vzs, txs, tys,tzs, temps
;  print, format='(5E13.4)', rhos,vxs, vys, vzs, temps
  
; COMMON TIMES FOR PROTON AND O+ DATA
; RULES: INTERPOLATE O+ DATA TO PROTONS TO FILL O+ DATA GAPS
    tpmax=max([tmv(ntv-1),tmo(nto-1)]) & tpmin=min([tmv(0),tmo(0)])
    if tpmin ge tpmax then begin
      print, 'Proton and O+ data do not overlap -> Stop'
      stop
    endif
print, 'Tpminmax:',tpmin, tpmax
    io1=0L & io2=ntv-1 
    while tmv(io1) lt tpmin do io1=io1+1 
    while tmv(io2) gt tpmax do io2=io2-1 
print,'!!!!:',io1,io2
print,'!!!!:',neqs(io1),neqs(io2)

    itot2=io2-io1+1
    ntv=itot2
    temptm=tmv(io1:io2) & tmv=fltarr(itot2) & tmv=temptm
    tempv=veqs(io1:io2,*) & temprho=neqs(io1:io2)
    tempp=peqs(io1:io2) & temptemp=teqs(io1:io2)
    veqs=fltarr(itot2+1,3) & tmv=fltarr(itot2) 
    neqs=tmv & peqs=tmv & teqs=tmv
    veqs=tempv & tmv=temptm & neqs=temprho & peqs=tempp & teqs=temptemp
; Interpolation of O+ 
    i1=0 & neqso1=neqs
    for i=0,nto-2 do begin
     iold=i1 & nav=0.0 & v1av=0 & v2av=0 & v3av=0
     ttemp=0.5*(tmo(i)+tmo(i+1))
     while tmv(i1) lt ttemp do begin
      nav=nav+neqs(i1)  
      v1av=v1av+veqs(i1,0) & v2av=v2av+veqs(i1,1) & v3av=v3av+veqs(i1,2)
      i1=i1+1
     endwhile
     i1inter=i1-iold
     nav=nav/float(i1inter)
     v1av=v1av/float(i1inter) & v2av=v2av/float(i1inter) 
     v3av=v3av/float(i1inter)
     for i2=iold,i1-1 do begin
      neqso1(i2)=neqso(i)/nav*neqso1(i2)
     endfor
;     print,'index',iold
;     print,'npav:', nav, '    noav:',neqso(i)
;     print,'v1av:', v1av, '    voav:',veqso(i,0)
;     print,'v2av:', v2av, '    voav:',veqso(i,1)
;     print,'v3av:', v3av, '    voav:',veqso(i,2)
    endfor
    iold=i1 & nav=0.0 & v1av=0 & v2av=0 & v3av=0
    for i1=iold,ntv-1 do begin
      nav=nav+neqs(i1)
      v1av=v1av+veqs(i1,0) & v2av=v2av+veqs(i1,1) & v3av=v3av+veqs(i1,2)
    endfor
    i1inter=ntv-iold
    nav=nav/float(i1inter)
    v1av=v1av/float(i1inter) & v2av=v2av/float(i1inter) 
    v3av=v3av/float(i1inter)   
    for i1=iold,ntv-1 do neqso1(i1)=neqso(nto-1)/nav*neqso1(i1)
    
;    print,'index',iold, ntv
;    print,'npav:', nav, '    noav:',neqso(nto-1)
;     print,'v1av:', v1av, '    voav:',veqso(nto-1,0)
;     print,'v2av:', v2av, '    voav:',veqso(nto-1,1)
;     print,'v3av:', v3av, '    voav:',veqso(nto-1,2)
;    neqs=neqs+16.*neqso1
        
;Correcting number density, velocity and pressure for O+
    temprho=neqs+16.*temprho
;    veqs(*,0)=(neqs(*)*veqs(*,0)+16.*temprho(*)*tempvx(*))/temprho(*)
;    veqs(*,1)=(neqs(*)*veqs(*,1)+16.*temprho(*)*tempvy(*))/temprho(*)
;    veqs(*,2)=(neqs(*)*veqs(*,2)+16.*temprho(*)*tempvz(*))/temprho(*)
;    peqs=peqs+tempp
;    neqs=temprho
    neqs=neqs+16.*neqso1
    neqs=smooth(neqs,3)
    veqs(*,0)=smooth(veqs(*,0),3)
    veqs(*,1)=smooth(veqs(*,1),3)
    veqs(*,2)=smooth(veqs(*,2),3)
    peqs=smooth(peqs,3)
;    for i1=1,ntv-1 do print,i1,neqs(i1),neqso1(i1)
    
return
end

; Esat.pro
; This program finds the maximum, intermediate, and minimum variance direc-
; tions for the electric field from actual satellite data.  It plots these
; as hodograms and gives the angles between the eigenvectors and the solar wind
; magnetic field.  The program also finds the deHoffmann-Teller velocity, 
; checks the Walen relation, and finds the quality ratio.


; Variable initialization
start:
nt=100000	  ; Maximum possible array size (actual data may be smaller)
time = fltarr(nt) & tme=time & tm=time
rho=fltarr(nt) & ptot=rho & btot=rho & p=rho
dumm2=strarr(2) & dumm5=strarr(5) & dumm='' & ftime='' & ltime=''
names = strarr(15) & num=0

B=fltarr(nt,3) & V=B & Efld=B & Eht=B
K0=fltarr(3,3) & Bevec=K0 & Eevec=K0 & Ehtevec=K0 & BMatrix=K0 & EMatrix=K0
  EhtMatrix=K0 & matrix=K0 & prematrix=K0 & K00=K0 & Binit=K0
Bd=fltarr(3) & Ed=Bd & Ehtd=Bd & Be=Bd & Ee=Bd & Ehte=Bd & Beval=Bd & Eeval=Bd
  Ehteval=Bd & vector=Bd & prevector=Bd & Kv0=Bd & vht=Bd & Bvalues=Bd
  Evalues=Bd & Bdirs=Bd & Edirs=Bd & init=Bd
c0 = fltarr(nt) & xprod0=c0 & xprod1=c0 & xprod2=c0 & c1=c0 & c2=c0
  dotprod=c0 & dotprod1=c0
lfit=fltarr(2)
again='y' & withps='n' & angle='y' & w='' & dagain='y' 
satl='U' & ow='y' & q='y' & doy='' & z=0 & small='n'
bgraph=fltarr(nt,3) & vgraph=fltarr(nt,3)
a=0
names=replicate(' ',15)
stuff='          '

; DATA INPUT: Since the Ulysses and ISEE3 Data sets are formatted differently,
;	      each had to be treated separately

 print, 'Which day (specify by day of year)?'
 read, doy
 openr, 9, 'PP19'+string(doy,'(i6)')+'_GSE.mag'
 readf, 9, dumm
print, dumm
 i=0L
; while i lt 5 do begin
 while not eof(9) do begin
  readf, 9, format='(a15,4f15.3)',stuff, sc, bls, bms, bns
; print, stuff, sc, bls, bms, bns
  tme(i)=sc/1000./3600.
  B(i,0)=bls & B(i,1)=bms & B(i,2)=bns & btot(i)=sqrt(bls*bls+bms*bms+bns*bns)
  itot=i & i=i+1
 endwhile
; resize the arrays to remove any zeros at the end
 temptime=tme(0:itot) & Btemp=B(0:itot,*) & btottemp=btot(0:itot)
 tme=fltarr(itot+1) & B=fltarr(itot+1,3) & btot=tme
 tme=temptime & B=Btemp & btot=btottemp
 it1=itot+1
 close,9
 print, stuff, sc, bls, bms, bns
 
 print,string(doy,'(i6)')
 
 openr, 7, 'p10_'+strmid(string(doy,'(i6)'),2,4)+'_moments.dat'
 print,'p10_'+strmid(string(doy,'(i6)'),2,4)+'_moments.dat'
 for i=1,11 do begin
   readf, 7, dumm
   print, dumm
 endfor
 i=0L
 while not eof(7) do begin
; while i lt 1 do begin
  readf, 7, format='(a10, a12, a2, 8E13.4)', dumm,stuff, dumm, rhos,$
	    vxs, vys, vzs, txs, tys,tzs, temp
  hr=float(strmid(stuff,0,2)) & mt=float(strmid(stuff,3,2))
  sc=float(strmid(stuff,6,6))
  tm(i)=hr+(mt/60)+(sc/(60*60))
  
  V(i,0)=vxs & V(i,1)=vys & V(i,2)=vzs 
  p(i)=temp*rhos & rho(i)=rhos
  itot2=i & i=i+1
 endwhile
 tempv=V(0:itot2,*) & temptm=tm(0:itot2) & temprho=rho(0:itot2)
 tempp=p(0:itot2)
 V=fltarr(itot2+1,3) & tm=fltarr(itot2+1) & rho=tm & p=tm
 V=tempv & tm=temptm & rho=temprho & p=tempp
 close, 7
  print, 'Time min and max for B:', tme(0), tme(it1-1)
  print, 'Time min and max for Plasma:', tm(0), tm(itot2)

;  print, hr, mt, sc
;  print, dumm,stuff, rhos,vxs, vys, vzs, txs, tys,tzs, temps
;  print, format='(5E13.4)', rhos,vxs, vys, vzs, temps
 

; TIME INTERVAL:  Since the data spans 24 hours, we only want to worry about 3
;		  hours at a time (which gives 30+ points in the interval). 
input:
print,'beginning and ending times'
print,ftime
print,ltime
print,'choose beginning of 3 hour time interval'
print,'hour'
read, bhr
print,'minute'
read, bmt
i1=bhr+(bmt/60) & i2=i1+3 & ib=0L & ie=itot & il=0L & ig=itot
while tme(ib) lt i1 do ib=ib+1
while tme(ie) gt i2 do ie=ie-1
while tme(il) lt (i1-(1./12)) do il=il+1	;gives extra 5 minutes on
while tme(ig) gt (i2+(1./12)) do ig=ig-1	;beginning and end of array
time=tme(il:ig) & B=B(il:ig,0:2) & btot=btot(il:ig)

 yt1='!8B!dx!n!3 (nT)'
 yt2='!8B!dy!n!3 (nT)'
 yt3='!8B!dz!n!3 (nT)'
 ttl='!17Equator S Data!3'
 stl='isee'
 avg='!17 20 Second Averages !3'


; Subscripting in plot statement plots only requested 3 hour interval, not
; including extra 5 minutes on each end
plot:
!P.REGION=[0.,0.,1.0,1.0]
!p.multi=[0,1,4,0,0] & !p.charsize=2 & !x.charsize=1 & !y.charsize=1
    dpx=0.63  & dpy=0.17
    xa=0.17 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.7 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3
!P.POSITION=[xa,ylo1,xe,yup1]
tempmax=max(B(*,0)) & tempmin=min(B(*,0)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, time(ib-il:ie-il), B(ib-il:ie-il,0), ytitle=yt1, $
yrange=[tempmin-1,tempmax+1], xtickname=names, yticks=4, $
ytickv=yn, title='Magnetic Field', xstyle=1

!P.POSITION=[xa,ylo2,xe,yup2]
tempmax=max(B(*,1)) & tempmin=min(B(*,1)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, time(ib-il:ie-il), B(ib-il:ie-il,1), ytitle=yt2, $
yrange=[tempmin-1,tempmax+1], xtickname=names, yticks=4, $
ytickv=yn, xstyle=1

!P.POSITION=[xa,ylo3,xe,yup3]
tempmax=max(B(*,2)) & tempmin=min(B(*,2)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, time(ib-il:ie-il), B(ib-il:ie-il,2), ytitle=yt3, $
yrange=[tempmin-1,tempmax+1], xtickname=names, yticks=4, $
ytickv=yn, xstyle=1

!P.POSITION=[xa,ylo4,xe,yup4]
tempmax=max(btot) & tempmin=min(btot) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, time(ib-il:ie-il), btot(ib-il:ie-il), ytitle='!8B!dTOT!n!3 (nT)',$
yrange=[tempmin-1,tempmax+1], yticks=4, ytickv=yn, $ 
xtitle='Time (hr)', xstyle=1

if satl eq 'I' then begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
  ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
endif else begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
  ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
endelse
xyouts, .5, 1, ttl, /norm, alignment=0.5
xyouts, .5, .94, '!17 1 Second Averages !3', /norm, alignment=0.5

if z eq 1701 then goto, eigen  ;sends code past averaging if it's already done  

if a le 0. then begin
 print, 'Is this the interval you want?'
 read, q
 if q eq 'n' then begin
  tme=temptime & B=Btemp & btot=btottemp & goto, input
 endif
endif

while a le 0 do begin
 print, 'With postscript?'
 read, withps
 if withps eq 'y' then begin
  print, 'What trial number?'
  read, num
  set_plot, 'ps'
  !p.thick=2
  device, filename='satl_'+stl+string(num,'(i1)')+'.ps'
  device, /inches, xsize=8., scale_factor=1.0, xoffset=0.5
  device, /inches, ysize=4., scale_factor=1.0, yoffset=0.5
  device, /landscape, /times, /bold, font_index=3
  a=a+1 & goto, plot
 endif
 a=a+1
endwhile

; Fill in the appropriate velocity, pressure, etc. arrays
pib=0 & pie=fix(itot2)
while tm(pib) lt i1 do pib=pib+1
while tm(pie) gt i2 do pie=pie-1
ptime=tm(pib:pie) & pV=V(pib:pie,0:2) & pp=1e10*(p(pib:pie))
prho=rho(pib:pie)

; AVERAGING: B-field is averaged over plasma intervals (5 min for ISEE3,
;	     4.0833 min for Ulysses).  The averaging is merely adding up the
;	     data points and dividing by the number of data points.
Bavg=fltarr(ie-ib+1,3) & btotavg=fltarr(ie-ib+1)
if satl eq 'I' then diff=(1./24)
if satl eq 'U' then diff=(4.05/120)
size=fix((pie-pib))
for k=0,2 do begin
 for i=0,size do begin
  avgib=0 & avgie=fix((ig-il))
  while time(avgib) lt ptime(i)-diff do avgib=avgib+1
  while time(avgie) gt ptime(i)+diff do avgie=avgie-1
  Bi=0. & btoti=0.
  for j=avgib,avgie do begin
   Bi=Bi+B(j,k)
   btoti=btoti+btot(j)
  endfor 
  n=float((avgie-avgib))
  Bavg(i,k)=Bi/n & btotavg(i)=btoti/n
 endfor
endfor
tempBavg=Bavg(0:size,0:2) & tempbtotavg=btotavg(0:size)
Bavg=fltarr(size+1,3) & btotavg=fltarr(size+1) & pE=Bavg & pptot=btotavg
Bavg=tempBavg & btotavg=tempbtotavg

if satl eq 'U' then begin
 for i=0,size do pptot(i)=pp(i)+((btotavg(i)*btotavg(i))/(8*!pi))
endif else begin
 pptot=1e10*ptot(pib:pie)
endelse

; Calculating the convection E-field:  Ec = -v x B
for i=0,size do $
 for k=0,2 do begin
   k1=(k+1) mod 3 & k2=(k+2) mod 3      
   pE(i,k)=pV(i,k1)*Bavg(i,k2)-pV(i,k2)*Bavg(i,k1)
   if pE(i,k) lt -1e10 then pE(i,k)=-pE(i,k)	; due to cross product, get
						; some large negative numbers
						; with missing data flags						
endfor

; Creates arrays that don't have the missing data flags
xblip=where(Bavg(*,0) lt 1e10, kb) & yblip=where(Bavg(*,1) lt 1e10)
zblip=where(Bavg(*,2) lt 1e10) & Bnb=fltarr(kb,3)
Bnb(*,0)=Bavg(xblip, 0) & Bnb(*,1)=Bavg(yblip, 1) & Bnb(*,2)=Bavg(zblip, 2) 

xblip=where(pV(*,0) lt 1e10, kv) & yblip=where(pV(*,1) lt 1e10)
zblip=where(pV(*,2) lt 1e10) & Vnb=fltarr(kv,3)
Vnb(*,0)=pV(xblip, 0) & Vnb(*,1)=pV(yblip, 1) & Vnb(*,2)=pV(zblip, 2)

xblip=where(pE(*,0) lt 1e10, ke) & yblip=where(pE(*,1) lt 1e10)
zblip=where(pE(*,2) lt 1e10) & Enb=fltarr(ke,3)
Enb(*,0)=pE(xblip, 0) & Enb(*,1)=pE(yblip, 1) & Enb(*,2)=pE(zblip, 2)

xblip=where(prho lt 1e10) & yblip=where(pp lt 1e10)
zblip=where(pptot lt 1e10)
rhonb=prho(xblip) & pnb=pp(yblip) & ptotnb=pptot(zblip)

!p.multi=[0,1,4,0,0] & !p.charsize=2 & !x.charsize=1 & !y.charsize=1

set_plot, 'x'
a=0
plot2:
; Temp setting are for range of plots
tempmax=max(Bnb) & tempmin=min(Bnb) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f7.2)')] & yn2=[string(temp3,'(f7.2)')]
yn3=[string(tempa,'(f7.2)')] & yn4=[string(temp2,'(f7.2)')]
yn5=[string(tempmax,'(f7.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, Bavg(*,0), $
line=0, ytitle='Magnetic Field', yrange=[tempmin-1,tempmax+1], $
xtickname=names, pos=[.05,.6,.75,.8], yticks=4, ytickv=yn, xstyle=1, $
max_value=1e10
oplot, ptime, Bavg(*,1), line=1  
oplot, ptime, Bavg(*,2), line=2  

tempmax=max(Vnb) & tempmin=min(Vnb) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) & $
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f7.2)')] & yn2=[string(temp3,'(f7.2)')] & $
yn3=[string(tempa,'(f7.2)')] & yn4=[string(temp2,'(f7.2)')] & $
yn5=[string(tempmax,'(f7.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, pV(*,0), $
line=0, ytitle='Velocity', yrange=[tempmin-10, tempmax+10], xtickname=names,$
pos=[.05,.4,.75,.6], yticks=4, ytickv=yn, xstyle=1, max_value=1e10
oplot, ptime, pV(*,1), line=1, max_value=1e10  
oplot, ptime, pV(*,2), line=2, max_value=1e10

tempmax=max(Enb) & tempmin=min(Enb) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) & $
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f8.2)')] & yn2=[string(temp3,'(f8.2)')] & $
yn3=[string(tempa,'(f8.2)')] & yn4=[string(temp2,'(f8.2)')] & $
yn5=[string(tempmax,'(f8.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, pE(*,0), $
line=0, ytitle='Electric Field', yrange=[tempmin-100,tempmax+100], $
xtickname=names, pos=[.05,.2,.75,.4], yticks=4, ytickv=yn, xstyle=1, $
max_value=1e10
oplot, ptime, pE(*,1), line=1, max_value=1e10  
oplot, ptime, pE(*,2), line=2, max_value=1e10

; Plotting of mass density, pressure, and total pressure (inc. EM)
tempmax=max([rhonb,pnb,ptotnb]) & tempmin=min([rhonb,pnb,ptotnb])
tempdif=tempmax-tempmin & temp2=tempmax-(tempdif/4)
temp3=tempmin+(tempdif/4) & tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f7.2)')] & yn2=[string(temp3,'(f7.2)')] & $
yn3=[string(tempa,'(f7.2)')] & yn4=[string(temp2,'(f7.2)')] & $
yn5=[string(tempmax,'(f7.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, prho(*), $
line=0, ytitle='!4q!3, P, P!itot!n', $
yrange=[tempmin-1,tempmax+1], pos=[.05, 0,.75,.2], xtitle='Time (hr)', $
yticks=4, ytickv=yn, xstyle=1, max_value=1e10
oplot, ptime, pp(*), line=1      
oplot, ptime, pptot(*), line=2

xyouts, .8, .8, 'For !8B, V !3and !8E!3:', /norm
xyouts, .8, .15, '____ = !4q!3 (Mass Density)', /norm
xyouts, .8, .1, '........ = Thermal Pressure (x10!e10!n)', /norm
xyouts, .8, .05, '--- = Total Pressure (x10!e10!n)', /norm

if satl eq 'I' then begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
  ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
 xyouts, .8, .75, '____ = X component', /norm
 xyouts, .8, .7, '........ = Y component', /norm
 xyouts, .8, .65, '--- = Z component', /norm
endif else begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
  ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
 xyouts, .8, .75, '____ = R component', /norm
 xyouts, .8, .7, '........ = T component', /norm
 xyouts, .8, .65, '--- = N component', /norm
endelse
xyouts, .5, .94, avg, /norm, alignment=0.5
xyouts, .5, 1, ttl, /norm, alignment=0.5
 
if a le 0. then begin
 print, 'Is this the interval you want?'
 read, q
 if q eq 'n' then begin
  tme=temptime & B=Btemp & btot=btottemp & goto, input
 endif
endif

if withps eq 'y' then begin
 while a le 0 do begin
 a=a+1
 set_plot, 'ps' & !p.thick=2 & goto, plot2
 endwhile
endif 

; HI-RES ANALYSIS: The 3 hour interval of the high-res data is re-plotted to
;		   the screen so the user can narrow the analysis interval down
;		   as far as necessary.  The data is then plotted for the small 
;		   interval, a variance matrix is made, the eigenvectors are
;		   calculated, and min var of B hodograms are plotted.
print, 'Min variance of B will be performed on hi-res data'
z=1701 & set_plot, 'x' & !p.thick=1 & goto, plot
eigen:
print, 'Enter lower time range limit'
print, 'Hour'
read, hlow 
print, 'Minute'
read, mlow
print, 'Second'
read, slow
tlow = hlow + (mlow/60) + (slow/(60*60)) & ibeg=0L
while time(ibeg) le tlow do ibeg=ibeg+1 & print, time(ibeg)
print, 'Upper range limit'
print, 'Hour'
read, hup
print, 'Minute'
read, mup
print, 'Second'
read, sup
tup = hup + (mup/60) + (sup/(60*60)) & iend=ig-il
if tup le time(0) then begin
  print,'upper limit not valid' & goto, eigen & endif
while time(iend) ge tup do iend=iend-1 & print, time(iend)
if time(iend) lt time(ibeg) then goto, eigen

a=0 & l=0
set_plot, 'x' & !p.multi=[0,1,3,0,0]
secondplot:
plot, time(ibeg:iend), B(ibeg:iend,0), xtickname=names, pos=[.1, .55, .9, .8],$
xstyle=1, ytitle=yt1, title='Magnetic Field (for minimum variance)', xticks=4
plot, time(ibeg:iend), B(ibeg:iend,1), xtickname=names, pos=[.1, .3, .9, .55],$
xstyle=1, ytitle=yt2, xticks=4
plot, time(ibeg:iend), B(ibeg:iend,2), pos=[.1, .05, .9, .3],$
xstyle=1, ytitle=yt3, subtitle='Time (hr)', xticks=4
if satl eq 'U' then begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(time(ibeg),'(f8.5)')+'-'+$
  string(time(iend),'(f8.5)')+',  Day of Year: 324, 1990!3', /norm, $
  alignment=0.5
endif else begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(time(ibeg),'(f8.5)')+'-'+$
  string(time(iend),'(f8.5)')+',  Day of Year: '+string(doy, '(i3)')+$
  ', 1978!3', /norm, alignment=0.5
endelse
xyouts, .5, .94, '!17 1 Second Averages !3', /norm, alignment=0.5
xyouts, .5, 1, ttl, /norm, alignment=0.5

while l le 0 do begin
 l=l+1
 print, 'Is this the interval you want?'
 read, small
 if small eq 'n' then goto, eigen
endwhile

if withps eq 'y' then begin
 while a le 0 do begin
 a=a+1
 set_plot, 'ps' & !p.thick=2 & goto, secondplot
 endwhile
endif 

ndata = iend - ibeg + 1
variance, ibeg, iend, ndata, B, BMatrix
 
evecs, BMatrix, Bd, Be, Beval, Bmaxdir, Bmindir, Bintdir, Bevec
Bmx=Bd(Bmaxdir) & Bint=Bd(Bintdir) & Bmn=Bd(Bmindir)

!p.multi=[0,2,2,0,0] & !p.charsize=1 & !x.charsize=1 & !y.charsize=1
chodograms, it1, ibeg, iend, B, Bevec, Bmaxdir, Bmindir, 4

if satl eq 'U' then begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(time(ibeg),'(f8.5)')+'-'+$
  string(time(iend),'(f8.5)')+',  Day of Year: 324, 1990!3', /norm, $
  alignment=0.5
endif else begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(time(ibeg),'(f8.5)')+'-'+$
  string(time(iend),'(f8.5)')+',  Day of Year: '+string(doy, '(i3)')+$
  ', 1978!3', /norm, alignment=0.5
endelse
xyouts, .5, 1, ttl, /norm, alignment=0.5
xyouts, .5, .94, '!17 1 Second Averages !3', /norm, alignment=0.5
plots, .8, .96, psym=1, /norm
xyouts, .88, .95, '!17 = Initial Point!3', /norm, alignment=0.5  
plots, .8, .92, psym=2, /norm
xyouts, .88, .91, '!17 = Final Point!3', /norm, alignment=0.5
xyouts, .7, .85, 'Eigenvectors:', /norm
xyouts, .7, .8, 'i!dB!n = '+string(Bevec(0,Bmaxdir)), /norm
xyouts, .7, .77, '     '+string(Bevec(1,Bmaxdir)), /norm
xyouts, .7, .74, '     '+string(Bevec(2,Bmaxdir)), /norm
xyouts, .7, .7, 'j!dB!n = '+string(Bevec(0,Bintdir)), /norm
xyouts, .7, .67, '     '+string(Bevec(1,Bintdir)), /norm
xyouts, .7, .64, '     '+string(Bevec(2,Bintdir)), /norm
xyouts, .7, .6, 'k!dB!n = '+string(Bevec(0,Bmindir)), /norm
xyouts, .7, .57, '     '+string(Bevec(1,Bmindir)), /norm
xyouts, .7, .54, '     '+string(Bevec(2,Bmindir)), /norm
xyouts, .9, .85, 'Eigenvalues:', /norm
xyouts, .9, .8, 'Max/Int ratio = ', /norm
xyouts, .9, .77, '    '+string((Bmx/Bint)), /norm
xyouts, .9, .73, 'Int/Min ratio = ', /norm
xyouts, .9, .7, '    '+string((Bint/Bmn)), /norm

; LOW-RES ANALYSIS: The 30 or so points in the 3 hour interval (using the
;		    averaged B-field) are analyzed using variance,
;		    eigenvectors, hodograms, deHoffmann-Teller transformations
;		    and Walen plots.
variance, 0, ke-1, ke, Enb, EMatrix
variance, 0, kb-1, kb, Bnb, BAMatrix

evecs, EMatrix, Ed, Ee, Eeval, Emaxdir, Emindir, Eintdir, Eevec
Emx=Ed(Emaxdir) & Eint=Ed(Eintdir) & Emn=Ed(Emindir)
evecs, BAMatrix, BAd, BAe, BAeval, BAmaxdir, BAmindir, BAintdir, BAevec
BAmx=BAd(BAmaxdir) & BAint=BAd(BAintdir) & BAmn=BAd(BAmindir)

!p.multi=[0,2,2,0,0]
chodograms, it1, 0, kb-1, Bnb, BAevec, BAmaxdir, BAmindir, 0
chodograms, it1, 0, kb-1, Bnb, Eevec, Emaxdir, Emindir, 3
if satl eq 'I' then begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
  ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
endif else begin
 xyouts, .5, .97, $
  '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
  ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
endelse
xyouts, .5, 1, ttl, /norm, alignment=0.5
xyouts, .5, .94, avg, /norm, alignment=0.5
plots, .8, .96, psym=1, /norm
xyouts, .88, .95, '!17 = Initial Point!3', /norm, alignment=0.5  
plots, .8, .92, psym=2, /norm
xyouts, .88, .91, '!17 = Final Point!3', /norm, alignment=0.5
xyouts, .7, .85, 'Eigenvectors:', /norm
xyouts, .7, .8, 'i!dB!n = '+string(BAevec(0,BAmaxdir)), /norm
xyouts, .7, .77, '     '+string(BAevec(1,BAmaxdir)), /norm
xyouts, .7, .74, '     '+string(BAevec(2,BAmaxdir)), /norm
xyouts, .7, .7, 'j!dB!n = '+string(BAevec(0,BAintdir)), /norm
xyouts, .7, .67, '     '+string(BAevec(1,BAintdir)), /norm
xyouts, .7, .64, '     '+string(BAevec(2,BAintdir)), /norm
xyouts, .7, .6, 'k!dB!n = '+string(BAevec(0,BAmindir)), /norm
xyouts, .7, .57, '     '+string(BAevec(1,BAmindir)), /norm
xyouts, .7, .54, '     '+string(BAevec(2,BAmindir)), /norm
xyouts, .9, .85, 'Eigenvalues:', /norm
xyouts, .9, .8, 'Max/Int ratio = ', /norm
xyouts, .9, .77, '    '+string((BAmx/BAint)), /norm
xyouts, .9, .73, 'Int/Min ratio = ', /norm
xyouts, .9, .7, '    '+string((BAint/BAmn)), /norm
xyouts, .7, .35, 'Eigenvectors:', /norm
xyouts, .7, .3, 'i!dE!n = '+string(Eevec(0,Emaxdir)), /norm
xyouts, .7, .27, '     '+string(Eevec(1,Emaxdir)), /norm
xyouts, .7, .24, '     '+string(Eevec(2,Emaxdir)), /norm
xyouts, .7, .2, 'j!dE!n = '+string(Eevec(0,Eintdir)), /norm
xyouts, .7, .17, '     '+string(Eevec(1,Eintdir)), /norm
xyouts, .7, .14, '     '+string(Eevec(2,Eintdir)), /norm
xyouts, .7, .1, 'k!dE!n = '+string(Eevec(0,Emindir)), /norm
xyouts, .7, .07, '     '+string(Eevec(1,Emindir)), /norm
xyouts, .7, .04, '     '+string(Eevec(2,Emindir)), /norm
xyouts, .9, .35, 'Eigenvalues:', /norm
xyouts, .9, .3, 'Max/Int ratio = ', /norm
xyouts, .9, .27, '    '+string((Emx/Eint)), /norm
xyouts, .9, .23, 'Int/Min ratio = ', /norm
xyouts, .9, .2, '    '+string((Eint/Emn)), /norm

fname='output_'+stl+string(num,'(i1)')+'.dat'
get_lun, unit
if satl eq 'I' then $
 print, 'Print angle of eigenvectors from X,Y,Z axes'
if satl eq 'U' then $
 print, 'Print angle of eigenvectors from R,T,N axes'
print, 'and the deHoffmann-Teller velocity?'
read, angle
if angle eq 'y' then begin
 print, 'Overwrite previous output.dat file?'
 read, ow
 if ow eq 'y' then openw, unit, fname
 if ow eq 'n' then openu, unit, fname & tmp=fstat(unit) & point_lun, unit, $
  tmp.size
 init=[Bavg(0,0), Bavg(0,1), Bavg(0,2)]
 Evalues=[Emx,Eint,Emn] & Bvalues=[BAmx,BAint,BAmn]
 Edirs=[Emaxdir,Eintdir,Emindir] & Bdirs=[BAmaxdir,BAintdir,BAmindir]
 print, 'ELECTRIC FIELD:'
 if satl eq 'I' then begin
  printf, unit, 'ISEE3 Data'
  printf, unit, ' '
  printf, unit, 'Time Interval '+string(tme(ib),'(f7.4)')+'-'+$
   string(tme(ie),'(f7.4)')+$
   ',  Day of Year: '+string(doy, '(i3)')+', 1978'
  printf, unit, ' '
  printf, unit, '5 Minute Averages'
 endif else begin
  printf, unit, 'Ulysses Data'
  printf, unit, ' '
  printf, unit, 'Time Interval '+string(tme(ib),'(f7.4)')+'-'+$
    string(tme(ie),'(f7.4)')+'  Day of Year: 324, 1990'
  printf, unit, ' '
  printf, unit, '4.0833 Minute Averages'
 endelse
 printf, unit, ' '
 printf, unit, 'ELECTRIC FIELD:'
 close, unit
 if satl eq 'I' then begin
  angles_isee, init, Eevec, Evalues, Edirs, fname, unit
 endif else begin
  angles_ulys, init, Eevec, Evalues, Edirs, fname, unit
 endelse
 print, ''
 print, 'MAGNETIC FIELD:'
 openu, unit, fname
 tmp=fstat(unit)
 point_lun, unit, tmp.size
 printf, unit, ''
 printf, unit, 'MAGNETIC FIELD:'
 close, unit
 if satl eq 'I' then begin
  angles_isee, init, BAevec, Bvalues, Bdirs, fname, unit
 endif else begin
  angles_ulys, init, BAevec, Bvalues, Bdirs, fname, unit
 endelse
;------------------------------------------------------------------------------
; deHoffmann-Teller velocity
; Setting up V(dht) = K^-1 dot (K dot V) where
;
;	Kab = (1/ndata)*sum(over b)[(|B|^2)*k.delta(ab) - Ba*Bb] and
;
;	(K dot V)a = (1/ndata)*sum(over b)([(|B|^2)*k.delta(ab) - Ba*Bb]*Vb)
;
;	k.delta(ab) = 1 if a=b , = 0 otherwise

 for k=0,2 do $
  for n=0,2 do $
   for j=0,kv-1 do begin
    if k eq n then prematrix(k,n)=(Bnb(j,0)^2 + Bnb(j,1)^2 + $
      Bnb(j,2)^2) - Bnb(j,k)^2
    if k eq n then prevector(k)=(Bnb(j,0)^2 + Bnb(j,1)^2 + $
      Bnb(j,2)^2)*Vnb(j,n) - Bnb(j,k)*Bnb(j,n)*Vnb(j,n)
    if k ne n then prematrix(k,n)=-Bnb(j,k)*Bnb(j,n)
    if k ne n then prevector(k)=-Bnb(j,k)*Bnb(j,n)*Vnb(j,n)
    matrix(k,n)=prematrix(k,n) + matrix(k,n)
    vector(k)=prevector(k) + vector(k)
 endfor      
 K0=(1./kv)*matrix
 Kv0=(1./kv)*vector
 K00=invert(K0, status0)
 vht=K00#Kv0
 openu, unit, fname
 tmp=fstat(unit)
 point_lun, unit, tmp.size
 if satl eq 'I' then begin
  printf, unit, 'deHoffmann-Teller velocity components (X,Y,Z):'
  printf, unit, vht
  print, 'deHoffmann-Teller velocity components (X,Y,Z):' 
  print, vht
 endif else begin
  printf, unit, 'deHoffmann-Teller velocity components (R,T,N):'
  printf, unit, vht
  print, 'deHoffmann-Teller velocity components (R,T,N):' 
  print, vht
 endelse
 close, unit
endif
;------------------------------------------------------------------------------
; Walen relation
;
; delta v = +/- delta v(alfven)
; where
; v(alfven) = B/sqrt(mu0*rho)    rho = mass density, mu0 = permeability const.
;
A=findgen(17)*(!pi*2/16)
usersym, cos(A), sin(A)

for i=0,kv-1 do $
 for k=0,2 do begin
  k1=(k+1) mod 3 & k2=(k+2) mod 3      
  Eht(i,k)=vht(k1)*Bnb(i,k2)-vht(k2)*Bnb(i,k1)
endfor
tempEht=Eht(0:kv-1,*) & Eht=fltarr(kv,3) & Eht=tempEht
print, 'Plot to test Walen relation?'
read, w
if (w eq 'y') then begin
 !p.multi=[0,2,1,0,0]
 for j=0,2 do $
  for i=0,kv-1 do begin
   vgraph(i,j)=(Vnb(i,j) - vht(j))
   bgraph(i,j)=(1e-6*Bnb(i,j))/$
    sqrt((rhonb(i)*1e6)*(1.67e-27)*(4*!pi*1e7))
 endfor
 vmin=min(vgraph) & vmax=max(vgraph)
 bmin=min(bgraph) & bmax=max(bgraph)
 bdif=bmax-bmin & vdif=vmax-vmin
 if vdif gt bdif then begin
  xr=[bmin,bmin+vdif] & yr=[vmin,vmax]
 endif else begin
  xr=[bmin,bmax] & yr=[vmin,vmin+vdif]
 endelse
 plot, bgraph(0:kv-1,0), vgraph(0:kv-1,0), xrange=xr, $
  yrange=yr, pos=[.05, .35, .41, .85], psym=2, $
  title='Walen Relation', xtitle='!8V!dAlfven!n!3 (km/s)', $
  ytitle='!8V - V!dht!n!3 (km/s)'
 oplot, bgraph(0:kv-1,1), vgraph(0:kv-1,1), psym=8
 oplot, bgraph(0:kv-1,2), vgraph(0:kv-1,2), psym=5
 lfit=poly_fit(bgraph,vgraph,1,yfit)
 oplot, bgraph, yfit, linestyle=0

 Efdmin=min(Enb(0:ke-1,*)) & Efdmax=max(Enb(0:ke-1,*))
 Ehtmin=min(Eht(0:ke-1,*))
 Efdif=Efdmax-Efdmin
 plot, Enb(0:ke-1,0), Eht(0:ke-1,0), pos=[.59, .35, .95, .85], $
  xtitle = '!8E!3', ytitle = '!8E!dht!n!3', xrange=[Efdmin, Efdmax], $
  yrange=[Ehtmin, Ehtmin+Efdif]
 oplot, Enb(0:ke-1,1), Eht(0:ke-1,1), linestyle=1
 oplot, Enb(0:ke-1,2), Eht(0:ke-1,2), linestyle=2
  
 if satl eq 'I' then begin
  xyouts, .5, .97, $
   '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
   ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
  xyouts, .59, .25, 'XYZ Components of deHoffmann-Teller Velocity:', /norm
  xyouts, .59, .22, 'X: '+string(vht(0)), /norm
  xyouts, .59, .19, 'Y: '+string(vht(1)), /norm
  xyouts, .59, .16, 'Z: '+string(vht(2)), /norm
  xyouts, .59, .11, '____ = X component', /norm
  xyouts, .59, .08, '........ = Y component', /norm
  xyouts, .59, .05, '--- = Z component', /norm
  plots, .05, .26, psym=2, /norm & xyouts, .06, .25, ' = X component', /norm
  plots, .05, .23, psym=8, /norm & xyouts, .06, .22, ' = Y component', /norm
  plots, .05, .20, psym=5, /norm & xyouts, .06, .19, ' = Z component', /norm
 endif else begin
  xyouts, .5, .97, $
   '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
   ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
  xyouts, .59, .25, 'RTN Components of deHoffmann-Teller Velocity:', /norm
  xyouts, .59, .22, 'R: '+string(vht(0)), /norm
  xyouts, .59, .19, 'T: '+string(vht(1)), /norm
  xyouts, .59, .16, 'N: '+string(vht(2)), /norm
  xyouts, .59, .11, '____ = R component', /norm
  xyouts, .59, .08, '........ = T component', /norm
  xyouts, .59, .05, '--- = N component', /norm
  plots, .05, .26, psym=2, /norm & xyouts, .06, .25, ' = R component', /norm
  plots, .05, .23, psym=8, /norm & xyouts, .06, .22, ' = T component', /norm
  plots, .05, .20, psym=5, /norm & xyouts, .06, .19, ' = N component', /norm
 endelse
 xyouts, .5, 1, ttl, /norm, alignment=0.5
 xyouts, .5, .94, avg, /norm, alignment=0.5
 xyouts, .05, .16, '___ = 1st degree least squares fit', /norm
endif

;------------------------------------------------------------------------------
; Quality of fit estimate -- d/d0 ratio
;
; d/d0 is the ratio of the square of the deviation between E(conv) and E(dHT)
;
; i.e.  d = (1/ndata)*sum(over i)( |(Vi - V(dHT)) x Bi|^2 )
;
; when V(dHT) is as calculated above and when V(dHT) = 0

numer = 0. & denom = 0.
for i=0,kv-1 do begin
 xprod0=(Vnb(i,1)-vht(1))*Bnb(i,2) - $
         Bnb(i,1)*(Vnb(i,2)-vht(2))
 xprod1=-(Vnb(i,2)-vht(2))*Bnb(i,0) + $
         Bnb(i,2)*(Vnb(i,0)-vht(0))
 xprod2=(Vnb(i,0)-vht(0))*Bnb(i,1) - $
         Bnb(i,0)*(Vnb(i,1)-vht(1))
 dotprod=(xprod0)^2 + (xprod1)^2 + (xprod2)^2
 numer=dotprod + numer
 c0=Vnb(i,1)*Bnb(i,2) - Bnb(i,1)*Vnb(i,2)
 c1=-Vnb(i,2)*Bnb(i,0) + Bnb(i,2)*Vnb(i,0)
 c2=Vnb(i,0)*Bnb(i,1) - Bnb(i,0)*Vnb(i,1)
 dotprod1=(c0)^2 + (c1)^2 + (c2)^2
 denom=dotprod1 + denom
endfor
dd=(1./kv)*numer
dd0=(1./kv)*denom
epsilon=dd/dd0
epsilon=epsilon(0)
print, ''
print, ''
print, ''
print, epsilon
openu, unit, fname
tmp=fstat(unit)
point_lun, unit, tmp.size
printf, unit, ' '
printf, unit, 'Quality Ratio d/d0 = '
printf, unit, epsilon
close, unit
;------------------------------------------------------------------------------
; Hodograms - E(dHT)
;
; Same as above eigenvector/value and hodogram routines, but this uses the
; deHoffmann-Teller E-field

variance, 0, ke-1, ke, Eht, EhtMatrix
evecs, EhtMatrix, Ehtd, Ehte, Ehteval, Ehtmaxdir, Ehtmindir, Ehtintdir, Ehtevec
Ehtmx=Ehtd(Ehtmaxdir) & Ehtint=Ehtd(Ehtintdir) & Ehtmn=Ehtd(Ehtmindir)

!p.multi=[0,2,2,0,0]
chodograms, it1, 0, ke-1, Enb, Eevec, Emaxdir, Emindir, 1
chodograms, it1, 0, ke-1, Eht, Ehtevec, Ehtmaxdir, Ehtmindir, 2

if satl eq 'I' then begin
 xyouts, .5, .97, $
 '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
 ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
endif else begin
 xyouts, .5, .97, $
 '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
 ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
endelse
xyouts, .5, 1, ttl, /norm, alignment=0.5
xyouts, .5, .94, avg, /norm, alignment=0.5
plots, .8, .96, psym=1, /norm
xyouts, .88, .95, '!17 = Initial Point!3', /norm, alignment=0.5
plots, .8, .92, psym=2, /norm
xyouts, .88, .91, '!17 = Final Point!3', /norm, alignment=0.5
xyouts, .7, .85, 'Eigenvectors:', /norm
xyouts, .7, .8, 'i!dE!n = '+string(Eevec(0,Emaxdir)), /norm
xyouts, .7, .77, '     '+string(Eevec(1,Emaxdir)), /norm
xyouts, .7, .74, '     '+string(Eevec(2,Emaxdir)), /norm
xyouts, .7, .7, 'j!dE!n = '+string(Eevec(0,Eintdir)), /norm
xyouts, .7, .67, '     '+string(Eevec(1,Eintdir)), /norm
xyouts, .7, .64, '     '+string(Eevec(2,Eintdir)), /norm
xyouts, .7, .6, 'k!dE!n = '+string(Eevec(0,Emindir)), /norm
xyouts, .7, .57, '     '+string(Eevec(1,Emindir)), /norm
xyouts, .7, .54, '     '+string(Eevec(2,Emindir)), /norm
xyouts, .9, .85, 'Eigenvalues:', /norm
xyouts, .9, .8, 'Max/Int ratio = ', /norm
xyouts, .9, .77, '    '+string((Emx/Eint)), /norm
xyouts, .9, .73, 'Int/Min ratio = ', /norm
xyouts, .9, .7, '    '+string((Eint/Emn)), /norm
xyouts, .7, .35, 'Eigenvectors:', /norm
xyouts, .7, .3, 'i!dEht!n = '+string(Ehtevec(0,Ehtmaxdir)), /norm
xyouts, .7, .27, '     '+string(Ehtevec(1,Ehtmaxdir)), /norm
xyouts, .7, .24, '     '+string(Ehtevec(2,Ehtmaxdir)), /norm
xyouts, .7, .2, 'j!dEht!n = '+string(Ehtevec(0,Ehtintdir)), /norm
xyouts, .7, .17, '     '+string(Ehtevec(1,Ehtintdir)), /norm
xyouts, .7, .14, '     '+string(Ehtevec(2,Ehtintdir)), /norm
xyouts, .7, .1, 'k!dEht!n = '+string(Ehtevec(0,Ehtmindir)), /norm
xyouts, .7, .07, '     '+string(Ehtevec(1,Ehtmindir)), /norm
xyouts, .7, .04, '     '+string(Ehtevec(2,Ehtmindir)), /norm
xyouts, .9, .35, 'Eigenvalues:', /norm
xyouts, .9, .3, 'Max/Int ratio = ', /norm
xyouts, .9, .27, '    '+string((Ehtmx/Ehtint)), /norm
xyouts, .9, .23, 'Int/Min ratio = ', /norm
xyouts, .9, .2, '    '+string((Ehtint/Ehtmn)), /norm

; ROTATION:  The B and E fields are rotated into their respective eigenvector
;	     bases, and then plotted by component (i.e., max, int, min)
!p.multi=[0,1,3,0,0] & !p.charsize=2 & !x.charsize=1 & !y.charsize=1
Barevec=fltarr(3,3)
Barevec(*,0)=Baevec(*,BAmaxdir) & Barevec(*,1)=Baevec(*,BAintdir)
Barevec(*,2)=Baevec(*,BAmindir)
brot = Bnb#Barevec 
tempmax=max(brot(*,0)) & tempmin=min(brot(*,0)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, brot(*,0), ytitle='B!di!n', $
yrange=[tempmin-1,tempmax+1], $
xtickname=names, pos=[.05,.55,.95,.8], yticks=4, ytickv=yn, $
title='Magnetic Field in Min Variance of !8B!3 Coordinates', xstyle=1, $
xticks=4

tempmax=max(brot(*,1)) & tempmin=min(brot(*,1)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, brot(*,1), ytitle='B!dj!n', $
yrange=[tempmin-1,tempmax+1], $
xtickname=names, pos=[.05,.3,.95,.55], yticks=4, ytickv=yn, xstyle=1, xticks=4

tempmax=max(brot(*,2)) & tempmin=min(brot(*,2)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f5.2)')] & yn2=[string(temp3,'(f5.2)')]
yn3=[string(tempa,'(f5.2)')] & yn4=[string(temp2,'(f5.2)')]
yn5=[string(tempmax,'(f5.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, brot(*,2), ytitle='B!dk!n', $
yrange=[tempmin-1,tempmax+1], xticks=4, xtitle='Time (hr)',$
pos=[.05,.05,.95,.3], yticks=4, ytickv=yn, xstyle=1

if satl eq 'I' then begin
 xyouts, .5, .97, $
 '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
 ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
endif else begin
 xyouts, .5, .97, $
 '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
 ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
endelse
xyouts, .5, 1, ttl, /norm, alignment=0.5
xyouts, .5, .94, avg, /norm, alignment=0.5

!p.multi=[0,1,3,0,0]
Erevec=fltarr(3,3)
Erevec(*,0)=Eevec(*,Emaxdir) & Erevec(*,1)=Eevec(*,Eintdir)
Erevec(*,2)=Eevec(*,Emindir)
erot=Enb#Erevec
tempmax=max(erot(*,0)) & tempmin=min(erot(*,0)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f8.2)')] & yn2=[string(temp3,'(f8.2)')]
yn3=[string(tempa,'(f8.2)')] & yn4=[string(temp2,'(f8.2)')]
yn5=[string(tempmax,'(f8.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, erot(*,0), ytitle='E!di!n', $
yrange=[tempmin-1,tempmax+1], $
xtickname=names, pos=[.05,.55,.95,.8], yticks=4, ytickv=yn, $
title='Electric Field in Max Variance of !8E!3 Coordinates', xstyle=1, $
xticks=4

tempmax=max(erot(*,1)) & tempmin=min(erot(*,1)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f8.2)')] & yn2=[string(temp3,'(f8.2)')]
yn3=[string(tempa,'(f8.2)')] & yn4=[string(temp2,'(f8.2)')]
yn5=[string(tempmax,'(f8.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, erot(*,1), ytitle='E!dj!n', $
yrange=[tempmin-1,tempmax+1], $
xtickname=names, pos=[.05,.3,.95,.55], yticks=4, ytickv=yn, xstyle=1, xticks=4

tempmax=max(erot(*,2)) & tempmin=min(erot(*,2)) & tempdif=tempmax-tempmin
temp2=tempmax-(tempdif/4) & temp3=tempmin+(tempdif/4) 
tempa=tempmax-(2*(tempdif/4))
yn1=[string(tempmin,'(f8.2)')] & yn2=[string(temp3,'(f8.2)')]
yn3=[string(tempa,'(f8.2)')] & yn4=[string(temp2,'(f8.2)')]
yn5=[string(tempmax,'(f8.2)')]
yn=[yn1, yn2, yn3, yn4, yn5]
plot, ptime, erot(*,2), ytitle='E!dk!n', $
yrange=[tempmin-1,tempmax+1], xticks=4, xtitle='Time (hr)',$
pos=[.05,.05,.95,.3], yticks=4, ytickv=yn, xstyle=1

if satl eq 'I' then begin
 xyouts, .5, .97, $
 '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
 ',  Day of Year: '+string(doy, '(i3)')+', 1978!3', /norm, alignment=0.5
endif else begin
 xyouts, .5, .97, $
 '!17Time Interval '+string(tme(ib),'(f7.4)')+'-'+string(tme(ie),'(f7.4)')+$
 ',  Day of Year: 324, 1990!3', /norm, alignment=0.5
endelse
xyouts, .5, 1, ttl, /norm, alignment=0.5
xyouts, .5, .94, avg, /norm, alignment=0.5

print, 'Run for another 3 hour time interval?'
read, again
if again eq 'y' then begin
 tme=temptime & B=Btemp & btot=btottemp
 a=0 & z=0 & set_plot, 'x'
 goto, input
endif

print, 'Run for another day of the year?'
read, dagain
if dagain eq 'y' then begin
 if withps eq 'y' then device, /close
 goto, start
endif

if withps eq 'y' then device,/close
set_plot,'x'
!p.thick=1
!p.multi=0
end

; This program finds the maximum, intermediate, and minimum variance direc-
; tions for the electric field from a satellite data set.  It plots these
; as hodograms and gives the angles between the eigenvectors and the LMN axes,
; XYZ axes, and the solar wind magnetic field.  The program also finds the 
; deHoffmann-Teller velocity, checks the Walen relation, and finds the quality
; ratio.
 

; Variable initialization
time = fltarr(501)
p = fltarr(501,5) & bl=p & bm=p & bn=p & vl=p & vm=p & vn=p & rho=p & ptot=p 
  b2=p
dumm7=strarr(7)
dumm=''
xsat=fltarr(5) & zsat=xsat & bl1=xsat & bm1=xsat & bn1=xsat

s = fltarr(251,5,3) & B=s & V=s & Efld=s & Eht=s
q = fltarr(3,3) & Bevec=q & Eevec=q & Ehtevec=q & BMatrix=q & EMatrix=q
  EhtMatrix=q & matrix=q & prematrix=q & K0=q & K00=q & Binit=q
u = fltarr(3) & Bd=u & Ed=u & Ehtd=u & Be=u & Ee=u & Ehte=u & Beval=u & Eeval=u
  Ehteval=u & vector=u & prevector=u & Kv0=u & vht=u & Bvalues=u & Evalues=u
  Bdirs=u & Edirs=u & init=u
r = fltarr(251) & xprod0=r & xprod1=r & xprod2=r & c0=r & c1=r & c2=r
  dotprod=r & dotprod1=r
g = fltarr(2) & lfit=g & Binitmax=g & Binitmin=g & Ehtdirs=g
again='y' & withps='n' & angles='y' & return='' & w='' & hods=''
bgraph = fltarr(251,3) & vgraph=fltarr(251,3)
;------------------------------------------------------------------------------
; Reading data file

print, 'What filenumber?'
read, num
openr,num,'sat'+string(num,'(i1)')
readf,num,format='(a55,f9.4)',dumm,philm
readf,num,format='(a80)',dumm
readf,num,format='(a80)',dumm
for i=0,4 do begin
  readf, num, format='(a7,f7.2,a3,f7.2)',dumm,xs,dumm,ys
  xsat(i)=xs & zsat(i)=ys
endfor
readf,num,dumm7

i=0
while not eof(num) do begin
  for k=0,4 do begin
    readf, num, times,bls,bms,bns,vls,vms,vns,rhos,ps,bs,ptots 
    time(i)=times & bl(i,k)=bls & bm(i,k)=bms & bn(i,k)=bns
    vl(i,k)=vls & vm(i,k)=vms & vn(i,k)=vns
    rho(i,k)=rhos & p(i,k)=ps & b2(i,k)=bs*bs & ptot(i,k)=ptots
  endfor
  itot=i
  if i lt 499 then i=i+1 else stop, 'Too many records.'
endwhile
close, num
;------------------------------------------------------------------------------
;Creating postscript file

print, 'With postscript?'
read, withps
if withps eq 'y' then begin
  set_plot, 'ps'
  device, filename='efield.ps'
  device, /inches, xsize=8., scale_factor=1.0, xoffset=0.5
  device, /inches, ysize=4., scale_factor=1.0, yoffset=0.5
  device, /landscape, /times, /bold, font_index=3
endif
;------------------------------------------------------------------------------
; Variance matrix

B(*,*,0)=bl(0:250,*) & B(*,*,1)=bm(0:250,*) & B(*,*,2)=bn(0:250,*)
V(*,*,0)=vl(0:250,*) & V(*,*,1)=vm(0:250,*) & V(*,*,2)=vn(0:250,*)
for i=0,250 do $
  for n=0,4 do $
    for k=0,2 do begin
      k1=(k+1) mod 3 & k2=(k+2) mod 3      
      Efld(i,n,k)=V(i,n,k1)*B(i,n,k2)-V(i,n,k2)*B(i,n,k1)
endfor

while again eq 'y' do begin
term1=fltarr(3,3)
vector=fltarr(3) & matrix=fltarr(3,3)
print, 'Enter satellite position (0-4)'
read, pos

set_plot, 'x'
!p.multi=[0,1,3,0,0]
tempmax=max(B(*,pos,0)) & tempmin=min(B(*,pos,0))
if (max(B(*,pos,1)) gt tempmax) then $
  tempmax=max(B(*,pos,1))
if (min(B(*,pos,1)) lt tempmin) then $
  tempmin=min(B(*,pos,1))
if (max(B(*,pos,2)) gt tempmax) then $
  tempmax=max(B(*,pos,2))
if (min(B(*,pos,2)) lt tempmin) then $
  tempmin=min(B(*,pos,2))
plot, B(*,pos,0), line=0, ytitle='Magnetic field', yrange=[tempmin, $
  tempmax]
oplot, B(*,pos,1), line=1 & oplot, B(*,pos,2), line=2
tempmax=max(V(*,pos,0)) & tempmin=min(V(*,pos,0))
if (max(V(*,pos,1)) gt tempmax) then $
  tempmax=max(V(*,pos,1))
if (min(V(*,pos,1)) lt tempmin) then $
  tempmin=min(V(*,pos,1))
if (max(V(*,pos,2)) gt tempmax) then $
  tempmax=max(V(*,pos,2))
if (min(V(*,pos,2)) lt tempmin) then $
  tempmin=min(V(*,pos,2))
plot, V(*,pos,0), line=0, ytitle='Velocity', yrange=[tempmin, tempmax]
oplot, V(*,pos,1), line=1 & oplot, V(*,pos,2), line=2
tempmax=max(Efld(*,pos,0)) & tempmin=min(Efld(*,pos,0))
if (max(Efld(*,pos,1)) gt tempmax) then $
  tempmax=max(Efld(*,pos,1))
if (min(Efld(*,pos,1)) lt tempmin) then $
  tempmin=min(Efld(*,pos,1))
if (max(Efld(*,pos,2)) gt tempmax) then $
  tempmax=max(Efld(*,pos,2))
if (min(Efld(*,pos,2)) lt tempmin) then $
  tempmin=min(Efld(*,pos,2))
plot, Efld(*,pos,0), line=0, ytitle='Electric field', yrange=[tempmin,$
  tempmax]
oplot, Efld(*,pos,1), line=1 & oplot, Efld(*,pos,2), line=2
if withps eq 'y' then set_plot, 'ps'
print, 'Enter lower time range limit'
read, ibeg
print, 'Upper range limit'
read, iend
ndata = iend - ibeg + 1
variance, ibeg, iend, ndata, pos, Efld, EMatrix
variance, ibeg, iend, ndata, pos, B, BMatrix
;------------------------------------------------------------------------------
; Find eigenvectors & eigenvalues

evecs, EMatrix, Ed, Ee, Eeval, Emaxdir, Emindir, Eintdir, Eevec
Emx=Ed(Emaxdir) & Eint=Ed(Eintdir) & Emn=Ed(Emindir)
if (num eq 3) then begin
  if (Eevec(0,Emindir) lt 0) and (B(0,pos,0) gt 0) then begin
    Eevec(*,Emindir)=(-Eevec(*,Emindir))
    Eevec(*,Emaxdir)=(-Eevec(*,Emaxdir))
  endif
  if (Eevec(0,Emindir) gt 0) and (B(0,pos,0) lt 0) then begin
    Eevec(*,Emindir)=(-Eevec(*,Emindir))
    Eevec(*,Emaxdir)=(-Eevec(*,Emaxdir))
  endif
endif
if (num eq 5) then begin
  if (Eevec(2,Emaxdir) lt 0) then begin
    Eevec(*,Emaxdir)=(-Eevec(*,Emaxdir))
    Eevec(*,Eintdir)=(-Eevec(*,Eintdir)) 
  endif
endif 
evecs, BMatrix, Bd, Be, Beval, Bmaxdir, Bmindir, Bintdir, Bevec
Bmx=Bd(Bmaxdir) & Bint=Bd(Bintdir) & Bmn=Bd(Bmindir)
if (num eq 5) then begin
  if (Bevec(2,Bmindir) lt 0) then begin
    Bevec(*,Bintdir)=(-Bevec(*,Bintdir))
    Bevec(*,Bmindir)=(-Bevec(*,Bmindir)) 
  endif
endif
;------------------------------------------------------------------------------
; Hodograms

print, 'Press return for max/intermediate & max/min plots'
read, return
print, 'Enter "f" if this is flux rope data, "c" if current sheet data.'
read, hods
!x.margin=[12,12]
!p.multi=[0,2,2,0,0]
Bdirs=[Bmaxdir, Bmindir]
Edirs=[Emaxdir, Emindir]
Binit=[B(0,pos,0), B(0,pos,1), B(0,pos,2)]
if (hods eq 'f') then begin
  fhodograms, ibeg, iend, pos, B, Bevec, Bdirs, 0, Binit
  fhodograms, ibeg, iend, pos, Efld, Eevec, Edirs, 1, Binit
endif else begin
  chodograms, ibeg, iend, pos, B, Bevec, Bmaxdir, Bmindir, 0
  chodograms, ibeg, iend, pos, B, Eevec, Emaxdir, Emindir, 3
endelse
!x.margin=[10,3]
!P.multi=[0,1,1,0,0] 
;------------------------------------------------------------------------------
; Eigenvector angles from LMN, XYZ, solar wind magnetic field

print, 'Print angle of eigenvectors from L,M,N & X,Y,Z axes' 
print, 'and the deHoffmann-Teller velocity?'
read, angles
if angles eq 'y' then begin
  init=[bl(0,pos), bm(0,pos), bn(0,pos)]
  Evalues=[Emx,Eint,Emn] & Bvalues=[Bmx,Bint,Bmn]
  Edirs=[Emaxdir,Eintdir,Emindir] & Bdirs=[Bmaxdir,Bintdir,Bmindir]
  print, 'ELECTRIC FIELD:'
  angles, philm, init, Eevec, Evalues, Edirs
  print, ''
  print, 'MAGNETIC FIELD:'
  angles, philm, init, Bevec, Bvalues, Bdirs
;------------------------------------------------------------------------------
; deHoffmann-Teller velocity

  for k=0,2 do $
    for n=0,2 do $
      for j=ibeg,iend do begin
        if k eq n then prematrix(k,n)=(B(j,pos,0)^2 + B(j,pos,1)^2 + $
          B(j,pos,2)^2) - B(j,pos,k)^2
        if k eq n then prevector(k)=(B(j,pos,0)^2 + B(j,pos,1)^2 + $
          B(j,pos,2)^2)*V(j,pos,n) - B(j,pos,k)*B(j,pos,n)*V(j,pos,n)
        if k ne n then prematrix(k,n)=-B(j,pos,k)*B(j,pos,n)
        if k ne n then prevector(k)=-B(j,pos,k)*B(j,pos,n)*V(j,pos,n)
        matrix(k,n)=prematrix(k,n) + matrix(k,n)
        vector(k)=prevector(k) + vector(k)
  endfor      
  K0=(1./ndata)*matrix
  Kv0=(1./ndata)*vector
  K00=invert(K0, status0)
  vht=K00#Kv0
  print, 'deHoffmann-Teller velocity components (L,M,N):' 
  print, vht
endif
;------------------------------------------------------------------------------
; Walen relation

print, 'Plot to test Walen relation?'
read, w
if (w eq 'y') then begin
  vgraph=fltarr(251,3) & bgraph=fltarr(251,3)
  for j=0,2 do $
    for i=ibeg,iend do begin
      alpha=0.
      vgraph(i,j)=V(i,pos,j) - vht(j)
      bgraph(i,j)=B(i,pos,j)*(1 - alpha)
  endfor
  vmin=min(vgraph) & vmax=max(vgraph)
  bmin=min(bgraph) & bmax=max(bgraph)
  plot, bgraph(ibeg:iend,0), vgraph(ibeg:iend,0), psym=5, xrange=[bmin,bmax], $
    yrange=[vmin,vmax]
  oplot, bgraph(ibeg:iend,1), vgraph(ibeg:iend,1), psym=6
  oplot, bgraph(ibeg:iend,2), vgraph(ibeg:iend,2), psym=7
  lfit=poly_fit(bgraph,vgraph,1,yfit)
  oplot, bgraph, yfit
endif
;------------------------------------------------------------------------------
; Quality of fit estimate -- d/d0 ratio

numer = 0. & denom = 0.
for i=ibeg,iend do begin
  xprod0=(V(i,pos,1)-vht(1))*B(i,pos,2) - B(i,pos,1)*(V(i,pos,2)-vht(2))
  xprod1=-(V(i,pos,2)-vht(2))*B(i,pos,0) + B(i,pos,2)*(V(i,pos,0)-vht(0))
  xprod2=(V(i,pos,0)-vht(0))*B(i,pos,1) - B(i,pos,0)*(V(i,pos,1)-vht(1))
  dotprod=(xprod0)^2 + (xprod1)^2 + (xprod2)^2
  numer=dotprod + numer
  c0=V(i,pos,1)*B(i,pos,2) - B(i,pos,1)*V(i,pos,2)
  c1=-V(i,pos,2)*B(i,pos,0) + B(i,pos,2)*V(i,pos,0)
  c2=V(i,pos,0)*B(i,pos,1) - B(i,pos,0)*V(i,pos,1)
  dotprod1=(c0)^2 + (c1)^2 + (c2)^2
  denom=dotprod1 + denom
endfor
dd=(1./ndata)*numer
dd0=(1./ndata)*denom
epsilon=dd/dd0
epsilon=epsilon(0)
print, ''
print, ''
print, ''
print, epsilon
;------------------------------------------------------------------------------
;Hodograms - Eht

for i=0,250 do $
  for k=0,2 do begin
    k1=(k+1) mod 3 & k2=(k+2) mod 3      
    Eht(i,pos,k)=vht(k1)*B(i,pos,k2)-vht(k2)*B(i,pos,k1)
endfor
variance, ibeg, iend, ndata, pos, Eht, EhtMatrix
evecs, EhtMatrix, Ehtd, Ehte, Ehteval, Ehtmaxdir, Ehtmindir, Ehtintdir, Ehtevec
Ehtmx=Ehtd(Ehtmaxdir) & Ehtint=Ehtd(Ehtintdir) & Ehtmn=Ehtd(Ehtmindir)
if (num eq 3) then begin
  if (Ehtevec(0,Ehtmindir) lt 0) and (B(0,pos,0) gt 0) then begin
    Ehtevec(*,Ehtmindir)=(-Ehtevec(*,Ehtmindir))
    Ehtevec(*,Ehtmaxdir)=(-Ehtevec(*,Ehtmaxdir))
  endif
  if (Ehtevec(0,Ehtmindir) gt 0) and (B(0,pos,0) lt 0) then begin
    Ehtevec(*,Ehtmindir)=(-Ehtevec(*,Ehtmindir))
    Ehtevec(*,Ehtmaxdir)=(-Ehtevec(*,Ehtmaxdir))
  endif
endif
print, "Press return for Eht hodograms"
read, return
print, 'Enter "f" if this is flux rope data, "c" if current sheet data.'
read, hods
!x.margin=[12,12]
!p.multi=[0,2,2,0,0]
Ehtdirs=[Ehtmaxdir, Ehtmindir]
if (hods eq 'f') then begin
  fhodograms, ibeg, iend, pos, B, Bevec, Bdirs, 0, Binit
  fhodograms, ibeg, iend, pos, Eht, Ehtevec, Ehtdirs, 2, Binit
endif else begin
  chodograms, ibeg, iend, pos, Efld, Eevec, Emaxdir, Emindir, 1
  chodograms, ibeg, iend, pos, Eht, Ehtevec, Ehtmaxdir, Ehtmindir, 2
endelse
!x.margin=[10,3]
!p.multi=[0,1,1,0,0]
;plot, Efld(ibeg:iend,pos,*), Eht(ibeg:iend,pos,*)
set_plot, 'x'

print, 'Run for another satellite position?'
read, again
endwhile


end

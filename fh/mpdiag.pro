; Program mpdiag
; This program generates diagnostic plots of the magnetopause boundary
; as seen by TPMapc.
; It is based on the program mpcross.pro
; Version 2
; This version incorporates the non-uniform grid in rho first 
; instituted in Versions 2 and 2b of Initmapc.  It also utilizes the
; new definition of the magnetopause boundary crossing.
; by Fred Hall, IV
; 19 September 2000


Function rhoindex,rho0

common rzcomm,rhomin,drho

  rhoindex=FIX((rho0-rhomin)/drho)
  return,rhoindex

End


Function phiindex,phi0

common pzcomm,phimin,dphi

  phiindex=FIX((phi0-phimin)/dphi)
  return,phiindex

End



; Begin Main Program
Pro mpdiag,ps=ps

; Initialize some variables
; Note that IDL uses long (l) integers with unformatted data.

common rzcomm,rhomin,drho
common pzcomm,phimin,dphi

rhomin=-40.0
rhomax=15.0
nrho=110l
phimin=-25.0
phimax=25.0
nphi=100l
YEAR=1980l
IDAY=50l
UTHOUR=21l
MINUTE=0l
SECOND=0l
IOPT=1l
totcross=500
psub=0.0
continue='y'
pi=3.141592654
agchoice='d'
xachoice='i'

PARMOD=fltarr(4)

; Open data file and obtain preliminary parameter data.
openr,4,'tpmmcc2.bin',/F77_UNFORMATTED
readu,4,rhomin
readu,4,rhomax
readu,4,nrho
readu,4,phimin
readu,4,phimax
readu,4,nphi
readu,4,YEAR
readu,4,IDAY
readu,4,UTHOUR
readu,4,MINUTE
readu,4,SECOND
for n=1,4 do begin
  readu,4,psub
  PARMOD(n-1)=psub
endfor
readu,4,IOPT
readu,4,totcross

; Define coordinate and data matrices
rho=findgen(nrho+1)
kappa=alog(rhomax/rhomin)/nrho
rho=rhomin*exp(kappa*rho)
rho(0)=rhomin
rho(nrho)=rhomax
phi=findgen(nphi+1)
dphi=(phimax-phimin)/nphi
phi=phi*dphi+phimin

; Define additional data matrices
stcross=lonarr(nphi+1)
mpccrho=fltarr(totcross)
mpccphi=mpccrho

; Read in the data
readu,4,stcross

close,4


; Generate the actual magnetopause crossings
mpcounter=0
; print,'nrho = ',nrho
; print,'nphi = ',nphi
for kmpc=0,nphi-1 do $
  if (stcross(kmpc) ne nrho) then begin $
;    print,'stcross = ',stcross(kmpc),$
;          '(',kmpc,'):   mpcounter = ',mpcounter         
    mpccrho(mpcounter)=rho(stcross(kmpc))
    mpccphi(mpcounter)=phi(kmpc)
    mpcounter=mpcounter+1
  endif
; print
; print
; print
; print,'Magnetopause crossings:'
; print,'mpccrho:'
; print,mpccrho
; print
; print,'mpccphi:'
; print,mpccphi
; print
; print
; print


; Obtain some information from the user
print,'Do you want to see the radial distance of the magnetopause'
print,'plotted against the azimuthal angle phi or the'
print,'azimuthal index jphi?'
print,'Please enter:'
print,'a for the angle phi'
print,'i for the index jphi'
repeat begin
  read,xachoice
endrep until ((xachoice EQ 'A') OR (xachoice EQ 'a') OR $
              (xachoice EQ 'I') OR (xachoice EQ 'i'))
print
print


; Establish the plot labels
mtl='Magnetopause Radial Distance (in the Equatorial Plane, z = 0)'
xtl='Azimuthal angle !7u!3 (radians)'
ytl='!7q!3!Dmp!N (R!DE!N)'



; Enable printer if specified
if keyword_set(ps) then begin
  set_plot,'ps'
  device,/portrait
  device,/inches,xsize=6.0,ysize=5.5,xoffset=1.0,yoffset=4.0
  device,filename='mpdiag.ps'
endif



; Generate the plot
if ((xachoice EQ 'A') OR (xachoice EQ 'a')) then begin
; The radial position of the magnetopause is to be plotted against
; the azimuthal angle phi.
  print,'You have specified that the radial position of the'
  print,'magnetopause is to be plotted against the azimuthal angle'
  print,'phi.'
  print,'Would you like the horizontal axis to be specified in'
  print,'degrees or radians?'  
  print,'Please enter:'
  print,'d for degrees'
  print,'r for radians'
  repeat begin
    read,agchoice
  endrep until ((agchoice EQ 'D') OR (agchoice EQ 'd') OR $
                (agchoice EQ 'R') OR (agchoice EQ 'r'))
  print
  print
  if ((agchoice EQ 'D') OR (agchoice EQ 'd')) then begin
    mpccphi=180.0*mpccphi/pi
    xtl='Azimuthal angle !7u!3 (degrees)'
  endif
  plot,mpccphi,mpccrho,PSYM=-2, $
       TITLE=mtl,XTITLE=xtl,YTITLE=ytl
endif $
else begin
; The radial position of the magnetopause is to be plotted against
; the azimuthal index jphi'
  print,'You have specified that the radial position of the'
  print,'magnetopause is to be plotted against the azimuthal index'
  print,'jphi.'
  jmp=intarr(totcross+1)
  jmp=phiindex(mpccphi)
  xtl='Azimuthal index jphi'
  plot,jmp,mpccrho,PSYM=-2, $
       TITLE=mtl,XTITLE=xtl,YTITLE=ytl
endelse


; Close printer device if opened and reset normal plotting
if keyword_set(ps) then begin
  device,/close
  set_plot,'x'
endif



print
print
print
print,'mpccrho:'
print,mpccrho
print
print
print,'mpccphi:'
print,mpccphi
print
print
print
print,'MAGNETOPAUSE CROSSINGS'
if ((xachoice EQ 'A') OR (xachoice EQ 'a')) then begin
  print,'     (crossing number, rho, phi)       '
  for n=1,totcross do begin
    print,'(',n,',',mpccrho(n),', ',mpccphi(n),')'
  endfor
endif $
else begin
  print,'     (crossing number, rho, jphi)       '
  for n=1,totcross do begin
    print,'(',n,',',mpccrho(n),', ',jmp(n),')'
  endfor
endelse
print
print
print
if ((agchoice EQ 'd') AND (xachoice EQ 'i')) then $
  print,'Maximum value of mpccphi: ',180.0*MAX(mpccphi)/pi,' (degrees)' $
else $
  print,'Maximum value of mpccphi: ',MAX(mpccphi)

print
print
print


end

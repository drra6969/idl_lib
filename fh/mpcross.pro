Pro mpcross,ps=ps
; Program mpcross
; This program generates a plot of the magnetopause boundary as
; seen by TPMapc.
; Version 2
; Version 2 differs only slightly from Version 1.  The plot title
; and axis labels are done properly in this version.  Nothing else
; is changed.
; by Fred Hall, IV
; 19 August 2000

; Initialize some variables
; Note that IDL uses long (l) integers with unformatted data.
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

PARMOD=fltarr(4)

; Open data file and obtain preliminary parameter data.
openr,4,'tpmmcc.bin',/F77_UNFORMATTED
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
drho=(rhomax-rhomin)/nrho
rho=rho*drho+rhomin
phi=findgen(nphi+1)
dphi=(phimax-phimin)/nphi
phi=phi*dphi+phimin
rhocross=fltarr(totcross+1)
phicross=rhocross

; Read in the data
readu,4,rhocross
readu,4,phicross

close,4


; Enable printer if specified
if keyword_set(ps) then begin
  set_plot,'ps'
  device,/portrait
  device,/inches,xsize=6.0,ysize=5.5,xoffset=1.0,yoffset=4.0
  device,filename='mpcross.ps'
endif



; Generate the plot
mtl='Magnetopause Crossings (in the Equatorial Plane, z = 0)'
plot,rhocross(1:*),phicross(1:*),/POLAR,PSYM=2, $
     TITLE=mtl,XTITLE='x (R!DE!N)',YTITLE='y (R!DE!N)'

print,'rhocross:'
print,rhocross
print
print
print,'phicross:'
print,phicross
print
print
print
print,'MAGNETOPAUSE CROSSINGS'
print,'     (rho, phi)       '
for n=1,totcross do begin
  print,'(',rhocross(n),', ',180.0*phicross(n)/pi,')'
endfor
print
print
print
print,'Maximum value of phicross: ',180.0*MAX(phicross)/pi


; Close printer device if opened and reset normal plotting
if keyword_set(ps) then begin
  device,/close
  set_plot,'x'
endif

end

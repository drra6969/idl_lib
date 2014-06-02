Pro hclwalendhtbound2,itest,smo,trmin,trmax,delt,fdelt,$
       slwal0,slwal1,slht0,slht1,t1a,t2a,totnumint,wps,deltab,$
                absmin,absmax,isc,bntest,satraj,datestrn

COMMON test, timeqs,nnn,neqs,veqs,bave

COMMON sc, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild, $
           re, rsc,dxsc,dysc,dzsc,volsc,volrat

COMMON minis, dscmin2,dscmax2,dscmin3,dscmax3,scdp,scdp1,scdp2,scdp3,scdp4

  iscorg=isc
  absmin=0.0 & absmax=0.0
  iv1a=intarr(4000) & iv2a=iv1a & ib1a=iv1a & ib2a=iv1a
  t1a=fltarr(4000) & t2a=t1a
  numint=0 
  succes=0
  iiter = 0
  iterror='n'
  testi=0
  tmin = trmin & tmax=tmin+delt
  print,'HT and Walen Analysis:', trmin, trmax
  if wps eq 'y' then printf,7,  'HT and Walen Analysis:', tmin, tmax
   while tmax le trmax do begin 
     hindbd,tmin,tmax,nnn,timeqs,iv1,iv2
     if iv2-iv1 lt 3 then  goto, nextiter
     nv=iv2-iv1+1  
  
; Determine the actual data set to be analyzed and give it physical 
;     (or normalized) units 
     vxv = fltarr(nv) & vyv = vxv  & vzv = vxv
     baxv = vxv       & bayv = vxv & bazv = vxv
     exv = vxv        & eyv = vxv  & ezv = vxv
     nnv = vxv
     bxv = fltarr(nv) & byv=bxv & bzv=bxv
     diffbxi=fltarr(nv)
     diffbyi=fltarr(nv)
     diffbzi=fltarr(nv)
     vxv=veqs(0,iv1:iv2) & vyv=veqs(1,iv1:iv2) & vzv=veqs(2,iv1:iv2)
     nnv=neqs(iv1:iv2)
     baxv=bave(0,iv1:iv2) & bayv=bave(1,iv1:iv2) & bazv=bave(2,iv1:iv2)
     if smo eq 'y' then begin
         vxv=smooth(vxv,3) & vyv=smooth(vyv,3) & vzv=smooth(vzv,3)
         nnv=smooth(nnv,3) 
         baxv=smooth(vxv,3) & bayv=smooth(vyv,3) & bazv=smooth(vzv,3)         
     endif

; Hoffmann Teller velocity and variance for Eht
     efieldeqs, vxv,vyv,vzv, baxv,bayv,bazv, exv,eyv,ezv
     
     htcoor, nv, vxv,vyv,vzv, baxv,bayv,bazv, vht
     ehtfieldeqs, 'p',vht,baxv,bayv,bazv,ehxv,ehyv,ehzv
; Some stuff for plots testing the walen relation and the HT frame
     vmhtx=vxv-vht(0)  & vmhty=vyv-vht(1)  & vmhtz=vzv-vht(2)
     vmhp=fltarr(nv,3) & vmhp(*,0)=vmhtx & vmhp(*,1)=vmhty  & vmhp(*,2)=vmhtz
     valfv=vmhp 
      valfv(*,0)=baxv/sqrt(nnv)
      valfv(*,1)=bayv/sqrt(nnv)
      valfv(*,2)=bazv/sqrt(nnv)
      valfv=21.8*valfv
     ewh=vmhp & ewh(*,0)=exv & ewh(*,1)=eyv &  ewh(*,2)=ezv
     ewht=vmhp & ewht(*,0)=ehxv & ewht(*,1)=ehyv &  ewht(*,2)=ehzv
; Test the walen relation and the HT frame
     lfit0=poly_fit(valfv,vmhp,1,yfit,yband,sigma,a0)
     lfit1=poly_fit(ewh,ewht,1,yfit,yband,sigma,a1)

     ccoef0=findgen(2,2) & cstd=findgen(2,2) & creg=findgen(2)
     ccoef0(*,0)=lfit0(*) & ccoef0(*,1)=lfit1(*)
     cstd(0,0)=sqrt(a0(0,0)) & cstd(1,0)=sqrt(a0(1,1)) 
     cstd(0,1)=sqrt(a1(0,0)) & cstd(1,1)=sqrt(a1(1,1)) 
     creg(0)=correlate(valfv,vmhp) & creg(1)=correlate(ewh,ewht)


    
; Test that magnetic field changes at least amount of testdb (we only want
; reconnection intervals)=
; variation coefficient: how much variance of measured magnetic field values 
;differs from the average of the measured magnetic field values. 
;the value is between 0.0-1.0. When strong field reversals and strong current
; testbd--->1.

baxv2=(total(baxv))/nv
bayv2=(total(bayv))/nv
bazv2=(total(bazv))/nv

baxv3=(total(baxv*baxv))/nv
bayv3=(total(bayv*bayv))/nv
bazv3=(total(bazv*bazv))/nv

bav3=sqrt(baxv3+bayv3+bazv3) 

for i=0,nv-1 do begin
diffbxi(i)=baxv(i)-baxv2
diffbyi(i)=bayv(i)-bayv2
diffbzi(i)=bazv(i)-bazv2
endfor

varcoefx=total(diffbxi*diffbxi)/nv 
varcoefy=total(diffbyi*diffbyi)/nv
varcoefz=total(diffbzi*diffbzi)/nv
testdb=sqrt(varcoefx+varcoefy+varcoefz)/bav3

if testdb gt 0.5 then print,'testdb',testdb

     case itest of
      0: if ( (abs(ccoef0(1,0)) gt slwal0) and (abs(ccoef0(1,0)) lt slwal1)$ 
            and (abs(ccoef0(1,1)) gt slht0) and $
                (abs(ccoef0(1,1)) lt slht1) )then begin

       if testdb gt deltab then begin

           print, 'Interval: ',cvhms(tmin/60.),' - ',cvhms(tmax/60.),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
           print, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
           print, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
           print, '  dHT velocity:', vht
           if wps eq 'y' then begin
             printf,7, 'Interval: ',cvhms(tmin/60.),' - ',cvhms(tmax/60.),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
             printf,7, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
             printf,7, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
             printf,7, '  dHT velocity:', vht
           endif
           succes = 1
       endif
      endif

      1: if ( (abs(ccoef0(1,0)) gt slwal0) and $
            (abs(ccoef0(1,0)) lt slwal1) ) then begin

       if testdb gt deltab then begin

          print, 'Interval: ',cvhms(tmin/60.),' - ',cvhms(tmax/60.),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
           print, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
           print, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
           print, '  dHT velocity:', vht
           if wps eq 'y' then begin
             printf,7, 'Interval: ',cvhms(tmin/60.),' - ',cvhms(tmax/60.),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
             printf,7, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
             printf,7, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
             printf,7, '  dHT velocity:', vht
           endif
           succes = 1          
       endif        
      endif
      2: if ( (abs(ccoef0(1,1)) gt slht0) and $
            (abs(ccoef0(1,1)) lt slht1) ) then begin
           print, 'Interval: ',cvhms(tmin/60.),' - ',cvhms(tmax/60.),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
           print, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
           print, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
           print, '  dHT velocity:', vht
           if wps eq 'y' then begin
             printf,7, 'Interval: ',cvhms(tmin/60.),' - ',cvhms(tmax/60.),$
              '  iteration=',iiter,$
              '  Number of data points:', nv
             printf,7, '  Walen coef: ', ccoef0(1,0),' +-',cstd(1,0),'   ',$
               ccoef0(0,0),' +-',cstd(0,0)
             printf,7, '  HT    coef: ', ccoef0(1,1),' +-',cstd(1,1),'   ',$
               ccoef0(0,1),' +-',cstd(0,1)
             printf,7, '  dHT velocity:', vht
           endif
           succes = 1
       endif
     endcase

     if succes eq 1 then begin
         iv1a(numint) = iv1 & iv2a(numint) = iv2 
;         ib1a(numint) = ib1 & ib2a(numint) = ib2
         t1a(numint) = tmin & t2a(numint) = tmax
         numint=numint+1
     endif
     succes=0

nextiter:
     tmin = tmin+fdelt*delt  &  tmax=tmin+delt
     iiter = iiter+1
 endwhile

 ii=0
 for i=1,numint-1 do begin
     if t1a(i) lt t2a(ii) then begin
         t2a(ii)=t2a(i) ; iv2a(ii)=iv2a(i) & ib2a(ii)=ib2a(i)
         endif else begin
         ii=ii+1
         t1a(ii)=t1a(i) & t2a(ii)=t2a(i)
;         iv1a(ii)=iv1a(i) & iv2a(ii)=iv2a(i)
;         ib1a(ii)=ib1a(i) & ib2a(ii)=ib2a(i)
     endelse
 endfor
 totnumint=ii+1 & t1a=t1a(0:(totnumint-1)) & t2a=t2a(0:(totnumint-1)) 
 for i=0,totnumint-1 do print, 'Intervals:', cvhms(t1a(i)/60.),' - ',$
                                cvhms(t2a(i)/60.)

for i=0,totnumint-1 do print, 'Int. in decimal hours:', t1a(i)/60.,' - ',$
t2a(i)/60.

 if wps eq 'y' then $
   for i=0,totnumint-1 do printf,7, 'Intervals:', cvhms(t1a(i)/60.),' - ',$
                                cvhms(t2a(i)/60.)
if wps eq 'y' then $
for i=0,totnumint-1 do printf,7, 'Int. in decimal hours:', t1a(i)/60.,' - ',$
t2a(i)/60.



absmin=t1a(0)
absmax=t2a(totnumint-1)
print,'absmin absmax',absmin,absmax
print,'size traj',size(scdr1)

; SATELLITE LOCATIONS



hindbd,absmin,absmax,naux,time,its,ite

;dscmin2=1.1*(min([scdr1(*,its:ite),scdr2(*,its:ite),scdr3(*,its:ite),$
;           scdr4(*,its:ite)])) & dscmax2=1.1*(max([scdr1(*,its:ite),$
;           scdr2(*,its:ite),$
;           scdr3(*,its:ite),scdr4(*,its:ite)])) 

if satraj eq 1 then begin
dscmin3=min(re*rsc(*,its:ite)+scdr1(*,its:ite))
dscmax3=max(re*rsc(*,its:ite)+scdr1(*,its:ite))
endif

if satraj eq 3 then begin
dscmin3=min(re*rsc(*,its:ite)+scdr3(*,its:ite))
dscmax3=max(re*rsc(*,its:ite)+scdr3(*,its:ite))
endif

if satraj eq 4 then begin
dscmin3=min(re*rsc(*,its:ite)+scdr4(*,its:ite))
dscmax3=max(re*rsc(*,its:ite)+scdr4(*,its:ite))
endif

;print,'relsc3traj',scdr3(*,its:ite)
;print,'sc3traj',re*scdr3(*,its:ite)
;print,'rsc',rsc(*,its:ite)
;print,'re*rsc',re*rsc(*,its:ite)

;plotcoordinates for B line plots
    dpx=0.41  & dpy=0.17
    xab=0.545 & xeb=xab+dpx 
    ylo1=0.58 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3

;plotcoordinates for P line plots
    dpx=0.41  & dpy=0.17
    xap=0.055 & xep=xap+dpx 
    ylo0=0.75 & yup0=ylo0+dpy
    ylo1=ylo0-dpy & yup1=ylo0
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3

print,'its ite',its,ite

; colors
      tvlct,[0,255,0,100,0,255,230],[0,0,255,100,255,0,230],$
                                    [0,0,0,255,255,255,0]
      red   = 1
      green = 2
      blue  = 3
      yebb  = 4
      grbl  = 5
      yell  = 6

print,'scdr1x',scdr1(0,its:ite)
scdp1=scdr1 & scdp2=scdr2 & scdp3=scdr3 & scdp4=scdr4
sat1:
wset,1
   scdp1(0,its:ite)=rsc(0,its:ite)+scdr1(0,its:ite)/re
   scdp1(1,its:ite)=rsc(1,its:ite)+scdr1(1,its:ite)/re
   scdp1(2,its:ite)=rsc(2,its:ite)+scdr1(2,its:ite)/re
print,'scdr1x2',scdp1(*,its:ite)
!P.POSITION=[0.1,0.1,0.9,0.9]


sat2:
wset,1
   scdp2(0,its:ite)=rsc(0,its:ite)+scdr2(0,its:ite)/re
   scdp2(1,its:ite)=rsc(1,its:ite)+scdr2(1,its:ite)/re
   scdp2(2,its:ite)=rsc(2,its:ite)+scdr2(2,its:ite)/re
print,'scdr1x2',scdp2(*,its:ite)
!P.POSITION=[0.1,0.1,0.9,0.9]


sat3:
wset,1
   scdp3(0,its:ite)=rsc(0,its:ite)+scdr3(0,its:ite)/re
   scdp3(1,its:ite)=rsc(1,its:ite)+scdr3(1,its:ite)/re
   scdp3(2,its:ite)=rsc(2,its:ite)+scdr3(2,its:ite)/re
print,'scdr1x2',scdp3(*,its:ite)
!P.POSITION=[0.1,0.1,0.9,0.9]


sat4:
wset,1
   scdp4(0,its:ite)=rsc(0,its:ite)+scdr4(0,its:ite)/re
   scdp4(1,its:ite)=rsc(1,its:ite)+scdr4(1,its:ite)/re
   scdp4(2,its:ite)=rsc(2,its:ite)+scdr4(2,its:ite)/re
print,'scdr1x2',scdp4(*,its:ite)
!P.POSITION=[0.1,0.1,0.9,0.9]



if satraj eq 1 then scdp=scdp1
if satraj eq 2 then scdp=scdp2
if satraj eq 3 then scdp=scdp3
if satraj eq 4 then scdp=scdp4


scminx=min(scdp(0,its:ite))
scmaxx=max(scdp(0,its:ite))
scminy=min(scdp(1,its:ite))
scmaxy=max(scdp(1,its:ite))
scminz=min(scdp(2,its:ite))
scmaxz=max(scdp(2,its:ite))

!P.CHARSIZE=2.0
!P.FONT=-1
!P.CHARTHICK=1.
!P.POSITION=[0.2,0.15,0.8,0.85]
  ttvar='nb' & if bntest eq 1 then ttvar='ne'
  ttwal='r'  & if slwal1 lt 0.5 then ttwal='t'
  ih1=fix(trmin/60.) & ih2=fix(trmax/60.)
  im1=fix(trmin-ih1*60) & im2=fix(trmax-ih2*60)
  psname2='cl'+datestrn+':'+string(ih1,'(i2.2)')+string(im1,'(i2.2)')$
                     +'_'+string(ih2,'(i2.2)')+string(im2,'(i2.2)')$
                         +'sc'+string(isc,'(i1.1)')+ttvar+ttwal
if (wps eq 'y') then begin
  !P.FONT=-1
  !P.THICK=2.
  !X.THICK=1.5
  !Y.THICK=1.5
  !P.CHARTHICK=1.5
  !P.CHARSIZE=2.
  set_plot,'ps'
  device,/color,bits=8
  device,filename=psname2+'.ps'
  device, /landscape, /times, /bold, font_index=3
  device, /inches, xsize=10., ysize=8., scale_factor=0.95, $
                      xoffset=0.5,yoffset=9.8
endif
textvar='Min variance B' & if bntest eq 1 then textvar='Max variance E'

plot_3dbox,scdp(0,its:ite),scdp(1,its:ite),scdp(2,its:ite),$
   /XY_PLANE,/YZ_PLANE, /XZ_PLANE, $
   GRIDSTYLE=1, XYSTYLE=3, XZSTYLE=4,YZSTYLE=5, $
   AZ=55, TITLE='', $
   XTITLE='X ', YTITLE='Y ', $
   ZTITLE='Z ', SUBTITLE='', $
   XRANGE=[scminx,scmaxx],YRANGE=[scminy,scmaxy],$
   ZRANGE=[scminz,scmaxz],style=4,$
   PSYM=-4, /save
plots,scdp(0,its),scdp(1,its),scdp(2,its),PSYM=2,SYMSIZE=3,/T3D
xyouts, 0.3, 0.92, 'Boundary Normal Directions',charsize=1.4,font=0,/normal
xyouts, 0.5, 0.88, 'Day: '+datestrn,charsize=1.1,font=0,/normal
xyouts, 0.65, 0.88, 'Time: '+string(ih1,'(i2.2)')+':'+string(im1,'(i2.2)')$
        +'-'+string(ih2,'(i2.2)')+':'+string(im2,'(i2.2)'),$
        charsize=1.1,font=0,/normal
xyouts, 0.55, 0.85, 'SC '+string(isc,'(i1.1)'),charsize=1.1,font=0,/normal
xyouts, 0.65, 0.85, textvar,charsize=1.1,font=0,/normal

xyouts, 0.8,0.7, 'search t and dt : '+string(delt,'(f5.2)')$
       +' ; '+string(fdelt,'(f5.2)'),charsize=0.7,/normal
xyouts,0.8,0.68, 'delta b='+string(deltab,'(f5.2)'),charsize=0.7,/normal
if itest ne 2 then $
  xyouts, 0.8,0.66, 'slope range for W:'+string(slwal0,'(f5.2)')$
                      +' ; '+string(slwal1,'(f5.2)'),charsize=0.7,/normal
if itest ne 1 then $
  xyouts, 0.8,0.64, 'slope range for HT:'+string(slht0,'(f5.2)')$
                      +' ; '+string(slht1,'(f5.2)'),charsize=0.7,/normal
isc=satraj
for i=0,totnumint-1 do begin
tvmin=t1a(i)
tvmax=t2a(i)
hclvarbbound2b, tvmin,tvmax,wps,smo,eb1,eb2,eb3,ee1,ee2,ee3,strnn,$
              absmin,absmax,isc,bntest,iterror,satraj,scminx,scmaxx,$
              scminy,scmaxy,scminz,scmaxz

endfor
print,'isc after hclvarbbound endfor',isc
print,'isc before hclwalendhtbound return',isc
isc=iscorg

if (wps eq 'y') then begin
device,/close & set_plot,'x'
endif

return
end


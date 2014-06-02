Pro hclwalendht,itest,smo,trmin,trmax,delt,fdelt,$
                slwal0,slwal1,slht0,slht1,t1a,t2a,totnumint,wps,deltab

COMMON test, timeqs,nnn,neqs,veqs,bave

  iv1a=intarr(4000) & iv2a=iv1a & ib1a=iv1a & ib2a=iv1a
  t1a=fltarr(4000) & t2a=t1a
  numint=0 
  succes=0
  iiter = 0
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
for i=0,totnumint-1 do printf,7, 'Int. in decimal hours:', $
                         t1a(i)/60.,' - ',t2a(i)/60.
return
end


     

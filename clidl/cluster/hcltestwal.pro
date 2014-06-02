Pro hcltestwal, tmin,tmax,withps,smo,strnn, vht


COMMON test, timeqs,nnn,neqs,veqs,bave

  print,'Carry out ht and walen analysis:', tmin, tmax  
  vxv = fltarr(nnn) & vyv = vxv  & vzv = vxv
  baxv = vxv       & bayv = vxv & bazv = vxv
  exv = vxv        & eyv = vxv  & ezv = vxv
  nnv = vxv
  bxv = fltarr(nnn) & byv=bxv & bzv=bxv

  vxv=veqs(0,*) & vyv=veqs(1,*) & vzv=veqs(2,*)
  nnv=neqs(*)
  baxv=bave(0,*) & bayv=bave(1,*) & bazv=bave(2,*)
  if smo eq 'y' then begin
      vxv=smooth(vxv,3) & vyv=smooth(vyv,3) & vzv=smooth(vzv,3)
      nnv=smooth(nnv,3) 
      baxv=smooth(vxv,3) & bayv=smooth(vyv,3) & bazv=smooth(vzv,3)         
  endif

; Hoffmann Teller velocity and variance for Eht
     efieldeqs, vxv,vyv,vzv, baxv,bayv,bazv, exv,eyv,ezv
;     print,'vxv',vxv
;     print,'vyv',vyv
;     print,'vzv',vzv
     
     htcoor, nnn, vxv,vyv,vzv, baxv,bayv,bazv, vht
     ehtfieldeqs, 'p',vht,baxv,bayv,bazv,ehxv,ehyv,ehzv
; Some stuff for plots testing the walen relation and the HT frame
     vmhtx=vxv-vht(0)  & vmhty=vyv-vht(1)  & vmhtz=vzv-vht(2)
     vmhp=fltarr(nnn,3) & vmhp(*,0)=vmhtx & vmhp(*,1)=vmhty  & vmhp(*,2)=vmhtz
     valfv=vmhp 
      valfv(*,0)=baxv/sqrt(nnv)
      valfv(*,1)=bayv/sqrt(nnv)
      valfv(*,2)=bazv/sqrt(nnv)
      valfv=21.8*valfv
     ewh=vmhp & ewh(*,0)=exv & ewh(*,1)=eyv &  ewh(*,2)=ezv
     ewht=vmhp & ewht(*,0)=ehxv & ewht(*,1)=ehyv &  ewht(*,2)=ehzv
; Test the walen relation and the HT frame
;     lfit0=poly_fit(valfv,vmhp,1,yfit,yband,sigma,a0)
;     lfit1=poly_fit(ewh,ewht,1,yfit,yband,sigma,a1)

;     ccoef0=findgen(2,2) & cstd=findgen(2,2) & creg=findgen(2)
;     ccoef0(*,0)=lfit0(*) & ccoef0(*,1)=lfit1(*)
;     cstd(0,0)=sqrt(a0(0,0)) & cstd(1,0)=sqrt(a0(1,1)) 
;     cstd(0,1)=sqrt(a1(0,0)) & cstd(1,1)=sqrt(a1(1,1)) 
;     creg(0)=correlate(valfv,vmhp) & creg(1)=correlate(ewh,ewht)

;      wset,1
      hwalenplot, tv,vmhp,valfv,ewh,ewht,vht,timeqs(0),timeqs(nnn-1),$
                  ccoef0,cstd,creg,strnn


return
end


     

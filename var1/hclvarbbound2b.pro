Pro hclvarbbound2b, tvmin,tvmax,withps,smo,eb1,eb2,eb3,ee1,ee2,ee3,strnn,$
                  absmin,absmax,isc,bntest,iterror,satraj,scminx,scmaxx,$
                  scminy,scmaxy,scminz,scmaxz


COMMON test, timeqs,nnn,neqs,veqs,bave

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4

COMMON pfield,ncis1,timep1,statsp1,np1,no1,nhe1,nhia1,vp1,vhia1,tp1,thia1, $
              ncis3,timep3,statsp3,np3,no3,nhe3,nhia3,vp3,vhia3,tp3,thia3, $
              ncis4,timep4,statsp4,np4,no4,nhe4,nhia4,vp4,vhia4,tp4,thia4 

COMMON sc,naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild, $
           re, rsc,dxsc,dysc,dzsc,volsc,volrat
COMMON minis, dscmin2,dscmax2,dscmin3,dscmax3,scdp,scdp1,scdp2,scdp3,scdp4

  print, 'Variance Analysis for the Time Range:', tvmin, tvmax
  print, '  Number of data points:', nnn



print,'isc',isc
if isc eq 2 then goto, ending
hcltestvar,timep1,timep3,timep4,ncis1,ncis3,ncis4,$
                tvmin,tvmax,isc,smo,iterror

if iterror eq 'y' then goto,ending


hindbd,tvmin,tvmax,naux,time,its,ite



print,'its ite sc',its,ite,isc

if bntest eq 0 then begin
   bxv=fltarr(nnn) & byv=bxv & bzv=bxv
   bxv(*)=bave(0,*) & byv(*)=bave(1,*) & bzv(*)=bave(2,*)
   vxv=fltarr(nnn) & vyv=bxv & vzv=bxv
   vxv(*)=veqs(0,*) & vyv(*)=veqs(1,*) & vzv(*)=veqs(2,*)
   efieldeqs, vxv,vyv,vzv, bxv,byv,bzv, exv,eyv,ezv
   if smo eq 'y' then begin
         vxv=smooth(vxv,3) & vyv=smooth(vyv,3) & vzv=smooth(vzv,3)
         bxv=smooth(vxv,3) & byv=smooth(vyv,3) & bzv=smooth(vzv,3)         
   endif
   
   varmat,nnn,bxv,byv,bzv,bav,bmat
   eigen, bmat,ef1,ef2,ef3,ewf
   sorteigencl, ef1,ef2,ef3
   eb1=ef1 & eb2=ef2 & eb3=ef3
   ebst='B' 


   !P.CHARSIZE=1.0
   !P.FONT=0
   !P.CHARTHICK=1.
   ps1=[.06, .55, .32, .9]
   ps2=[.40, .55, .66, .9]
   post=0.93  & dpo=0.55

   xtit1='j!dB!n'& xtit2='k!dB!n' & ytit='i!dB!n'
   titl='Magnetic Field Variance'
   ft=bave 
   ft(0,*)=bave(0,*)*ef1(0)+bave(1,*)*ef1(1)+bave(2,*)*ef1(2)
   ft(1,*)=bave(0,*)*ef2(0)+bave(1,*)*ef2(1)+bave(2,*)*ef2(2)
   ft(2,*)=bave(0,*)*ef3(0)+bave(1,*)*ef3(1)+bave(2,*)*ef3(2)

   low=fltarr(3) & high=low 
   for i=0,2 do   low(i)=min(ft(i,*))
   for i=0,2 do   high(i)=max(ft(i,*))
   dif=0.5*(high-low)  & aver=0.5*(high+low)  & diff=1.02*max(dif)
   bot0=aver(0)-diff & top0=aver(0)+diff
   bot1=aver(1)-diff & top1=aver(1)+diff
   bot2=aver(2)-diff & top2=aver(2)+diff

   A=findgen(17)*(!pi*2/16)
   usersym, cos(A), sin(A)

endif



if bntest eq 1 then begin
   ; ELECTRIC FIELD


   bxv=fltarr(nnn) & byv=bxv & bzv=bxv
   bxv(*)=bave(0,*) & byv(*)=bave(1,*) & bzv(*)=bave(2,*)
   vxv=fltarr(nnn) & vyv=bxv & vzv=bxv
   vxv(*)=veqs(0,*) & vyv(*)=veqs(1,*) & vzv(*)=veqs(2,*)
   efieldeqs, vxv,vyv,vzv, bxv,byv,bzv, exv,eyv,ezv

   varmat,nnn,exv,eyv,ezv,eav,emat
   eigen, emat,ef1,ef2,ef3,ewf
   print
   sorteigencle, ef1,ef2,ef3
   ee1=ef1 & ee2=ef2 & ee3=ef3
   ebst='E' 
   ef3=ef1
     ps1=[.06, 0.05, .32, .4]
     ps2=[.40, 0.05, .66, .4]
     post=0.43  & dpo=0.05

   xtit1='j!dE!n'& xtit2='k!dE!n' & ytit='i!dE!n'
   titl='Electric Field Variance'
   ee=fltarr(3,nnn) 
   ee(0,*)=exv(*) & ee(1,*)=eyv(*) & ee(2,*)=ezv(*) 
   ft=ee
   ft(0,*)=ee(0,*)*ef1(0)+ee(1,*)*ef1(1)+ee(2,*)*ef1(2)
   ft(1,*)=ee(0,*)*ef2(0)+ee(1,*)*ef2(1)+ee(2,*)*ef2(2)
   ft(2,*)=ee(0,*)*ef3(0)+ee(1,*)*ef3(1)+ee(2,*)*ef3(2)

   low=fltarr(3) & high=low 
   for i=0,2 do   low(i)=min(ft(i,*))
   for i=0,2 do   high(i)=max(ft(i,*))
   dif=0.5*(high-low)  & aver=0.5*(high+low)  & diff=1.02*max(dif)
   bot0=aver(0)-diff & top0=aver(0)+diff
   bot1=aver(1)-diff & top1=aver(1)+diff
   bot2=aver(2)-diff & top2=aver(2)+diff

   A=findgen(17)*(!pi*2/16)
   usersym, cos(A), sin(A)

endif

nef=0.15 & ddscx=scmaxx-scminx & ddscy=scmaxy-scminy & ddscz=scmaxz-scminz
nefx=nef*ddscx & nefy=nef*ddscy & nefz=nef*ddscz

if isc eq 1 then begin
if satraj eq 1 then wset,1
if satraj eq 0 then wset,0
if satraj ne 1 and satraj ne 0 then goto,ending
!P.POSITION=[0.1,0.1,0.9,0.9]
plot_3dbox,[scdp1(0,its),scdp1(0,its)+nefx*ef3(0)],[scdp1(1,its),$
            scdp1(1,its)+nefy*ef3(1)],[scdp1(2,its),scdp1(2,its)+nefz*ef3(2)],$
   /XY_PLANE,XYSTYLE=1,/YZ_PLANE,YZSTYLE=1,$
   AZ=55,XRANGE=[scminx,scmaxx],YRANGE=[scminy,scmaxy],$
   ZRANGE=[scminz,scmaxz],style=4,$
   CHARSIZE=2,/noerase
endif


if isc eq 2 then begin
if satraj eq 2 then wset,1
if satraj eq 0 then wset,0
if satraj ne 2 and satraj ne 0 then goto,ending
!P.POSITION=[0.1,0.1,0.9,0.9]
plot_3dbox,[scdp2(0,its),scdp2(0,its)+nefx*ef3(0)],[scdp2(1,its),$
            scdp2(1,its)+nefy*ef3(1)],[scdp2(2,its),scdp2(2,its)+nefz*ef3(2)],$
   /XY_PLANE,XYSTYLE=1,/YZ_PLANE,YZSTYLE=1,$
    AZ=55,XRANGE=[scminx,scmaxx],YRANGE=[scminy,scmaxy],$
    ZRANGE=[scminz,scmaxz],style=4,$
    CHARSIZE=2,/noerase
endif

if isc eq 3 then begin
if satraj eq 3 then wset,1
if satraj eq 0 then wset,0
if satraj ne 3 and satraj ne 0 then goto,ending
!P.POSITION=[0.1,0.1,0.9,0.9]
plot_3dbox,[scdp3(0,its),scdp3(0,its)+nefx*ef3(0)],[scdp3(1,its),$
            scdp3(1,its)+nefy*ef3(1)],[scdp3(2,its),scdp3(2,its)+nefz*ef3(2)],$
   /XY_PLANE,XYSTYLE=1,/YZ_PLANE,YZSTYLE=1,$
   AZ=55,XRANGE=[scminx,scmaxx],YRANGE=[scminy,scmaxy],$
   ZRANGE=[scminz,scmaxz],style=4,$
   CHARSIZE=2,/noerase
endif

if isc eq 4 then begin
if satraj eq 4 then wset,1
if satraj eq 0 then wset,0
if satraj ne 4 and satraj ne 0 then goto,ending
!P.POSITION=[0.1,0.1,0.9,0.9]
plot_3dbox,[scdp4(0,its),scdp4(0,its)+nefx*ef3(0)],[scdp4(1,its),$
            scdp4(1,its)+nefy*ef3(1)],[scdp4(2,its),$
            scdp4(2,its)+nefz*ef3(2)],$
   /XY_PLANE,XYSTYLE=1,/YZ_PLANE,YZSTYLE=1,$
   AZ=55,XRANGE=[scminx,scmaxx],YRANGE=[scminy,scmaxy],$
   ZRANGE=[scminz,scmaxz],style=4,$
   CHARSIZE=2,/noerase
endif

ending:  
if iterror eq 'y' then print,'no boundary drawn, not enough data'



return
end

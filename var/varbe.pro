Pro varbe, tvmin,tvmax,withps,smo,eb1,eb2,eb3,ee1,ee2,ee3,strnn

COMMON test, timeqs,nnn,neqs,veqs,bave,eeqs

  print, 'Variance Analysis for the Time Range:', tvmin, tvmax
  print, '  Number of data points:', nnn

   bxv=fltarr(nnn) & byv=bxv & bzv=bxv
   bxv(*)=bave(0,*) & byv(*)=bave(1,*) & bzv(*)=bave(2,*)
   vxv=bxv & vyv=bxv & vzv=bxv
   exv=bxv & eyv=bxv & ezv=bxv
   vxv(*)=veqs(0,*) & vyv(*)=veqs(1,*) & vzv(*)=veqs(2,*)
   exv(*)=eeqs(0,*) & eyv(*)=eeqs(1,*) & ezv(*)=eeqs(2,*)
   if smo eq 'y' then begin
         vxv=smooth(vxv,3) & vyv=smooth(vyv,3) & vzv=smooth(vzv,3)
         bxv=smooth(bxv,3) & byv=smooth(byv,3) & bzv=smooth(bzv,3)         
   endif
   
   varmat,nnn,bxv,byv,bzv,bav,bmat
   eigen, bmat,ef1,ef2,ef3,ewf
   sorteigenb, ef1,ef2,ef3
   eb1=ef1 & eb2=ef2 & eb3=ef3
   ebst='B' 

   !P.CHARSIZE=1.0
   !P.FONT=0
   !P.CHARTHICK=1.
   if withps eq 'y' then begin
     !P.FONT=2
     !P.CHARTHICK=2.
     !P.CHARSIZE=1.
     !P.THICK=4.
     !X.THICK=3
     !Y.THICK=3
   endif
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

   plot, ft(1,*), ft(0,*), $
     xrange=[bot1,top1], xtitle=xtit1, ytitle=ytit, $
     xtick_get= xmark, ytick_get=ymark, yrange=[bot0, top0], xstyle=1, $
     ystyle=1, pos=ps1,/noerase
   plots, ft(1,0), ft(0,0), psym=6
   plots, ft(1,nnn-1), ft(0,nnn-1), psym=2

   plot, ft(2,*), ft(0,*), $
     xrange=[bot2,top2], xtitle=xtit2, ytitle=ytit, xtick_get= $
     xmark, ytick_get=ymark, yrange=[bot0, top0], xstyle=1, ystyle=1, $
     pos=ps2,/noerase
   plots, ft(2,0), ft(0,0), psym=6
   plots, ft(2,nnn-1), ft(0,nnn-1), psym=2

    xyouts, .7, .98, strnn, /norm
    xyouts, .7, .94, 'Variance Interval '+$
      string(tvmin,'(f5.1)')+' - '+string(tvmax,'(f5.1)')+'', /norm
    plots, .75, .525, psym=6, /norm
    xyouts, .76, .52, ' - Initial Point', /norm
    plots, .75, .495, psym=2, /norm
    xyouts, .76, .49, ' - Final Point', /norm
    plots, .75, .465, psym=8, /norm
    xyouts, .76, .46, ' - Reference Period', /norm
    
    
   xyouts, .36, 0.38+dpo, titl, /norm, charthick=1.5,alignment=0.5
  
   xyouts, .70, .35+dpo, 'Eigenvectors:', /norm
   xyouts, .70, .3+dpo, ytit+' = '+string(ef1(0),'(f5.2)'), /norm
   xyouts, .70, .27+dpo, '     '+string(ef1(1),'(f5.2)'), /norm
   xyouts, .70, .24+dpo, '     '+string(ef1(2),'(f5.2)'), /norm
   xyouts, .70, .2+dpo, xtit1+' = '+string(ef2(0),'(f5.2)'), /norm
   xyouts, .70, .17+dpo, '     '+string(ef2(1),'(f5.2)'), /norm
   xyouts, .70, .14+dpo, '     '+string(ef2(2),'(f5.2)'), /norm
   xyouts, .70, .1+dpo, xtit2+' = '+string(ef3(0),'(f5.2)'), /norm
   xyouts, .70, .07+dpo, '     '+string(ef3(1),'(f5.2)'), /norm
   xyouts, .70, .04+dpo, '     '+string(ef3(2),'(f5.2)'), /norm
   xyouts, .85, .35+dpo, 'Eigenvalue Ratios:', /norm
   if abs(ewf(0)/ewf(1)) lt 9999.9 then $
     xyouts, .85, .31+dpo, 'Max/Int: '+string((ewf(0)/ewf(1)),'(f7.2)'), /norm $
   else    xyouts, .85, .31+dpo, 'Max/Int: > 10000', /norm
   if abs(ewf(1)/ewf(2)) lt 9999.9 then $
     xyouts, .85, .28+dpo, 'Int/Min: '+string((ewf(1)/ewf(2)),'(f7.2)'), /norm $
   else    xyouts, .85, .28+dpo, 'Int/Min: > 10000', /norm
  
   iphix=180./!pi*acos(abs(ef1(0))) & jphix=180./!pi*acos(abs(ef2(0)))
   kphix=180./!pi*acos(abs(ef3(0))) 
   iphiy=180./!pi*acos(abs(ef1(1))) & jphiy=180./!pi*acos(abs(ef2(1)))
   kphiy=180./!pi*acos(abs(ef3(1))) 
   xyouts, .85, .24+dpo,'Angles with X:', /norm
   xyouts, .85, .21+dpo,'ix: '+string(iphix,'(f4.1)'), /norm
   xyouts, .85, .18+dpo,'jx: '+string(jphix,'(f4.1)'), /norm
   xyouts, .85, .15+dpo,'kx: '+string(kphix,'(f4.1)'), /norm
   xyouts, .85, .11+dpo,'Angles with Y:', /norm
   xyouts, .85, .08+dpo,'iy: '+string(iphiy,'(f4.1)'), /norm
   xyouts, .85, .05+dpo,'jy: '+string(jphiy,'(f4.1)'), /norm
   xyouts, .85, .02+dpo,'ky: '+string(kphiy,'(f4.1)'), /norm


; ELECTRIC FIELD

   vxv=fltarr(nnn) & vyv=bxv & vzv=bxv
   vxv(*)=veqs(0,*) & vyv(*)=veqs(1,*) & vzv(*)=veqs(2,*)
   exv(*)=eeqs(0,*) & eyv(*)=eeqs(1,*) & ezv(*)=eeqs(2,*)

   varmat,nnn,exv,eyv,ezv,eav,emat
   eigen, emat,ef1,ef2,ef3,ewf
   print
   sorteigenb, ef1,ef2,ef3
   ee1=ef1 & ee2=ef2 & ee3=ef3
   ebst='E' 

   !P.CHARSIZE=1.0
   !P.FONT=0
   !P.CHARTHICK=1.
   if withps eq 'y' then begin
     !P.FONT=3
     !P.CHARTHICK=2.5
     !P.CHARSIZE=1.
     !P.THICK=4.
     !X.THICK=4
     !Y.THICK=4
   endif
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

   plot, ft(1,*), ft(0,*), $
     xrange=[bot1,top1], xtitle=xtit1, ytitle=ytit, $
     xtick_get= xmark, ytick_get=ymark, yrange=[bot0, top0], xstyle=1, $
     ystyle=1, pos=ps1,/noerase
   plots, ft(1,0), ft(0,0), psym=6
   plots, ft(1,nnn-1), ft(0,nnn-1), psym=2

   plot, ft(2,*), ft(0,*), $
     xrange=[bot2,top2], xtitle=xtit2, ytitle=ytit, xtick_get= $
     xmark, ytick_get=ymark, yrange=[bot0, top0], xstyle=1, ystyle=1, $
     pos=ps2,/noerase
   plots, ft(2,0), ft(0,0), psym=6
   plots, ft(2,nnn-1), ft(0,nnn-1), psym=2
    
    
   xyouts, .36, 0.38+dpo, titl, /norm, charthick=1.5,alignment=0.5
  
   xyouts, .70, .35+dpo, 'Eigenvectors:', /norm
   xyouts, .70, .3+dpo, ytit+' = '+string(ef1(0),'(f5.2)'), /norm
   xyouts, .70, .27+dpo, '     '+string(ef1(1),'(f5.2)'), /norm
   xyouts, .70, .24+dpo, '     '+string(ef1(2),'(f5.2)'), /norm
   xyouts, .70, .2+dpo, xtit1+' = '+string(ef2(0),'(f5.2)'), /norm
   xyouts, .70, .17+dpo, '     '+string(ef2(1),'(f5.2)'), /norm
   xyouts, .70, .14+dpo, '     '+string(ef2(2),'(f5.2)'), /norm
   xyouts, .70, .1+dpo, xtit2+' = '+string(ef3(0),'(f5.2)'), /norm
   xyouts, .70, .07+dpo, '     '+string(ef3(1),'(f5.2)'), /norm
   xyouts, .70, .04+dpo, '     '+string(ef3(2),'(f5.2)'), /norm
   xyouts, .85, .35+dpo, 'Eigenvalue Ratios:', /norm
   if abs(ewf(0)/ewf(1)) lt 9999.9 then $
     xyouts, .85, .31+dpo, 'Max/Int: '+string((ewf(0)/ewf(1)),'(f7.2)'), /norm $
   else    xyouts, .85, .31+dpo, 'Max/Int: > 10000', /norm
   if abs(ewf(1)/ewf(2)) lt 9999.9 then $
     xyouts, .85, .28+dpo, 'Int/Min: '+string((ewf(1)/ewf(2)),'(f7.2)'), /norm $
   else    xyouts, .85, .28+dpo, 'Int/Min: > 10000', /norm
  
   iphix=180./!pi*acos(abs(ef1(0))) & jphix=180./!pi*acos(abs(ef2(0)))
   kphix=180./!pi*acos(abs(ef3(0))) 
   iphiy=180./!pi*acos(abs(ef1(1))) & jphiy=180./!pi*acos(abs(ef2(1)))
   kphiy=180./!pi*acos(abs(ef3(1))) 
   xyouts, .85, .24+dpo,'Angles with X:', /norm
   xyouts, .85, .21+dpo,'ix: '+string(iphix,'(f4.1)'), /norm
   xyouts, .85, .18+dpo,'jx: '+string(jphix,'(f4.1)'), /norm
   xyouts, .85, .15+dpo,'kx: '+string(kphix,'(f4.1)'), /norm
   xyouts, .85, .11+dpo,'Angles with Y:', /norm
   xyouts, .85, .08+dpo,'iy: '+string(iphiy,'(f4.1)'), /norm
   xyouts, .85, .05+dpo,'jy: '+string(jphiy,'(f4.1)'), /norm
   xyouts, .85, .02+dpo,'ky: '+string(kphiy,'(f4.1)'), /norm



  !P.FONT=-1
  
return
end

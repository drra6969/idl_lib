PRO HODO, nt, t, tv1,tv2,itv1,itv2,f,ef1,ef2,ef3,ewf,st1,ind,strnn,pt,vht
; Hodograms - current sheet crossing

  !P.CHARSIZE=1.0
  !P.FONT=0
  !P.CHARTHICK=1.
  if pt eq 0 then begin
     ps1=[.08, .55, .34, .9]
     ps2=[.42, .55, .68, .9]
     post=0.93  & dpo=0.55
  endif
  if pt eq 1 then begin
     ps1=[.08, 0.05, .34, .4]
     ps2=[.42, 0.05, .68, .4]
     post=0.43  & dpo=0.05
  endif
  if itv1 lt 0 then itv1=0
  if itv2 ge nt then itv2=nt-1
  
  jvec='j!d'+ind+'!n'   & xtit1=jvec
  kvec='k!d'+ind+'!n'   & xtit2=kvec
  ivec='i!d'+ind+'!n'   & ytit =ivec
  titl='!17'+st1+' in Variance of !18'+ind+'!17 Coordinates!3'

  ft=f 
  ft(*,0)=f(*,0)*ef1(0)+f(*,1)*ef1(1)+f(*,2)*ef1(2)
  ft(*,1)=f(*,0)*ef2(0)+f(*,1)*ef2(1)+f(*,2)*ef2(2)
  ft(*,2)=f(*,0)*ef3(0)+f(*,1)*ef3(1)+f(*,2)*ef3(2)
  vhtt=vht
  vhtt(0)=total(vht(*)*ef1(*))
  vhtt(1)=total(vht(*)*ef2(*))
  vhtt(2)=total(vht(*)*ef3(*))

  low=fltarr(3) & high=low 
  for i=0,2 do   low(i)=min(ft(*,i))
  for i=0,2 do   high(i)=max(ft(*,i))
  dif=0.5*(high-low)  & aver=0.5*(high+low)  & diff=1.02*max(dif)
  bot0=aver(0)-diff & top0=aver(0)+diff
  bot1=aver(1)-diff & top1=aver(1)+diff
  bot2=aver(2)-diff & top2=aver(2)+diff

  A=findgen(17)*(!pi*2/16)
  usersym, cos(A), sin(A)

  plot, ft(*,1), ft(*,0), $
    xrange=[bot1,top1], xtitle=xtit1, ytitle=ytit, $
    xtick_get= xmark, ytick_get=ymark, yrange=[bot0, top0], xstyle=1, $
    ystyle=1, pos=ps1
  if itv2 gt itv1 then oplot, ft(itv1:itv2,1), ft(itv1:itv2,0), psym=8 
  plots, ft(0,1), ft(0,0), psym=6
  plots, ft(nt-1,1), ft(nt-1,0), psym=2
  if st1 eq 'Velocity' then plots, vhtt(1),vhtt(0), psym=5, symsize=2
;  print,'x tick values:'
;  print, xmark
;  print,'y tick values:'
;  print, ymark

  plot, ft(*,2), ft(*,0), $
    xrange=[bot2,top2], xtitle=xtit2, ytitle=ytit, xtick_get= $
    xmark, ytick_get=ymark, yrange=[bot0, top0], xstyle=1, ystyle=1, $
    pos=ps2; xticks=3
  if itv2 gt itv1 then oplot, ft(itv1:itv2,2), ft(itv1:itv2,0), psym=8 
  plots, ft(0,2), ft(0,0), psym=6
  plots, ft(nt-1,2), ft(nt-1,0), psym=2
  if st1 eq 'Velocity' then  plots, vhtt(2),vhtt(0), psym=5, symsize=2
;  print,'x tick values:'
;  print, xmark
;  print,'y tick values:'
;  print, ymark

  if pt eq 0 then begin
    xyouts, .4, .98, '!17Interval '+$
      string(t(0),'(f5.1)')+' - '+string(t(nt-1),'(f5.1)')+'!3', $
      /norm, alignment=0.5
    xyouts, .7, .98, strnn, /norm
    xyouts, .7, .94, '!17Variance Interval '+$
      string(tv1,'(f5.1)')+' - '+string(tv2,'(f5.1)')+'!3', /norm
      
    plots, .75, .525, psym=6, /norm
    xyouts, .76, .52, '!17 - Initial Point!3', /norm
    plots, .75, .495, psym=2, /norm
    xyouts, .76, .49, '!17 - Final Point!3', /norm
    plots, .75, .465, psym=8, /norm
    xyouts, .76, .46, '!17 - Reference Period!3', /norm
  endif
  if st1 eq 'Velocity' then  begin
    plots, .75, 0.01, psym=5, /norm
    xyouts, .76, 0.01, '!17 - Walen velocity!3', /norm
  endif
    
    
  xyouts, .36, 0.38+dpo, titl, /norm, charthick=1.5,alignment=0.5
  
  xyouts, .72, .35+dpo, 'Eigenvectors:', /norm
  xyouts, .72, .3+dpo, ivec+' = '+string(ef1(0),'(f7.4)'), /norm
  xyouts, .72, .27+dpo, '     '+string(ef1(1),'(f7.4)'), /norm
  xyouts, .72, .24+dpo, '     '+string(ef1(2),'(f7.4)'), /norm
  xyouts, .72, .2+dpo, jvec+' = '+string(ef2(0),'(f7.4)'), /norm
  xyouts, .72, .17+dpo, '     '+string(ef2(1),'(f7.4)'), /norm
  xyouts, .72, .14+dpo, '     '+string(ef2(2),'(f7.4)'), /norm
  xyouts, .72, .1+dpo, kvec+' = '+string(ef3(0),'(f7.4)'), /norm
  xyouts, .72, .07+dpo, '     '+string(ef3(1),'(f7.4)'), /norm
  xyouts, .72, .04+dpo, '     '+string(ef3(2),'(f7.4)'), /norm
  xyouts, .87, .35+dpo, 'Eigenvalue Ratios:', /norm
  if abs(ewf(0)/ewf(1)) lt 9999.9 then $
    xyouts, .87, .31+dpo, 'Max/Int: '+string((ewf(0)/ewf(1)),'(f7.2)'), /norm $
  else    xyouts, .87, .31+dpo, 'Max/Int: > 10000', /norm
  if abs(ewf(1)/ewf(2)) lt 9999.9 then $
    xyouts, .87, .28+dpo, 'Int/Min: '+string((ewf(1)/ewf(2)),'(f7.2)'), /norm $
  else    xyouts, .87, .28+dpo, 'Int/Min: > 10000', /norm
  
  iphix=180./!pi*acos(abs(ef1(0))) & jphix=180./!pi*acos(abs(ef2(0)))
  kphix=180./!pi*acos(abs(ef3(0))) 
  iphiy=180./!pi*acos(abs(ef1(1))) & jphiy=180./!pi*acos(abs(ef2(1)))
  kphiy=180./!pi*acos(abs(ef3(1))) 
  xyouts, .87, .24+dpo,'Angles with X:', /norm
  xyouts, .87, .21+dpo,'ix: '+string(iphix,'(f4.1)'), /norm
  xyouts, .87, .18+dpo,'jx: '+string(jphix,'(f4.1)'), /norm
  xyouts, .87, .15+dpo,'kx: '+string(kphix,'(f4.1)'), /norm
  xyouts, .87, .11+dpo,'Angles with Y:', /norm
  xyouts, .87, .08+dpo,'iy: '+string(iphiy,'(f4.1)'), /norm
  xyouts, .87, .05+dpo,'jy: '+string(jphiy,'(f4.1)'), /norm
  xyouts, .87, .02+dpo,'ky: '+string(kphiy,'(f4.1)'), /norm

  !P.FONT=-1
  
return
end

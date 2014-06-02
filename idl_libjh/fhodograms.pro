; Hodograms

PRO FHODOGRAMS, IBEG, IEND, POS, X, EVEC, DIRS, PT, BINIT
r = fltarr(3,251) & proj0=r & proj1=r &proj2=r & add=r
intdir = 0
xmark = fltarr(10) & ymark=fltarr(10)
maxdir=dirs(0) & mindir=dirs(1)
for i=ibeg, iend do $
  for k=0,2 do begin
    proj0(k,i)=X(i,pos,0)*evec(0,k)
    proj1(k,i)=X(i,pos,1)*evec(1,k)
    proj2(k,i)=X(i,pos,2)*evec(2,k)
    add(k,i)=proj0(k,i)+proj1(k,i)+proj2(k,i)
endfor
for i=0,2 do begin
  if (maxdir eq i or mindir eq i) then intdir = (i+1) mod 3
  if (intdir eq maxdir or intdir eq mindir) then intdir = (i+2) mod 3
endfor 
Binitmax=[0,(Binit(0)*evec(0,maxdir) + Binit(1)*evec(1,maxdir) + $
  Binit(2)*evec(2,maxdir))]
Binitmin=[0,(Binit(0)*evec(0,mindir) + Binit(1)*evec(1,mindir) + $
  Binit(2)*evec(2,mindir))]
Binitint=[0,(Binit(0)*evec(0,intdir) + Binit(1)*evec(1,intdir) + $
  Binit(2)*evec(2,intdir))]
low1=min(add(mindir,ibeg:iend))
high1=max(add(mindir,ibeg:iend))
low2=min(add(intdir,ibeg:iend))
high2=max(add(intdir,ibeg:iend))
lowmax=1.05*min(add(maxdir,ibeg:iend))
highmax=1.05*max(add(maxdir,ibeg:iend))
diff1=(high1-low1)/2
diff2=(high2-low2)/2
diffmax=(highmax-lowmax)/2
if (abs(diffmax) ge abs(diff1)) then begin
  avg1=(high1-low1)/2 + low1
  top1=avg1+diffmax & bot1=avg1-diffmax
endif else begin
  top1=high1 & bot1=low1
  avgmax=(highmax-lowmax)/2 + lowmax
  highmax=avgmax+diff1 & lowmax=avgmax-diff1
endelse
if (abs(diffmax) ge abs(diff2)) then begin
  avg2=(high2-low2)/2 + low2
  top2=avg2+diffmax & bot2=avg2-diffmax
endif else begin
  top2=high2 & bot2=low2
  avgmax=(highmax-lowmax)/2 + lowmax
  highmax=avgmax+diff2 & lowmax=avgmax-diff2
endelse
if (pt eq 0) then plot, add(intdir,ibeg:iend), add(maxdir,ibeg:iend), $
  xrange=[bot2,top2], psym=1,  xtitle='B int', ytitle='B max', xtick_get= $
  xmark, ytick_get=ymark, yrange=[lowmax, highmax], xstyle=1, ystyle=1
if (pt eq 1) then plot, add(intdir,ibeg:iend), add(maxdir,ibeg:iend), $
  xrange=[bot2,top2], psym=1,  xtitle='E int', ytitle='E max', xtick_get= $
  xmark, ytick_get=ymark, yrange=[lowmax, highmax], xstyle=1, ystyle=1
if (pt eq 2) then plot, add(intdir,ibeg:iend), add(maxdir,ibeg:iend), $
  xrange=[bot2,top2], psym=1,  xtitle='Eht int', ytitle='Eht max', xtick_get= $
  xmark, ytick_get=ymark, yrange=[lowmax, highmax], xstyle=1, ystyle=1
oplot, add(intdir,ibeg:iend), add(maxdir,ibeg:iend), xrange=[bot2,top2]
if ((pt eq 1) or (pt eq 2)) then oplot, Binitint, Binitmax
xyouts, [(add(intdir,ibeg) + .06*top2), (add(intdir,iend) + .06*top2)],  $
  [(add(maxdir,ibeg) + .1*diffmax), (add(maxdir,iend) - .1*diffmax)], $
  [ibeg, iend]
xyouts, [(add(intdir,ibeg) + .06*top2), (add(intdir,iend) + .06*top2)], $
  [(add(maxdir,ibeg) + .1*diffmax), (add(maxdir,iend) - .1*diffmax)], $
  ['t = ', 't = ']
if (pt eq 0) then xyouts, [.5*(xmark(1)-xmark(0))+xmark(0)], [.5*(ymark(1)- $
  ymark(0))+ymark(0)], ['B']
if (pt eq 1) then xyouts, [.5*(xmark(1)-xmark(0))+xmark(0)], [.5*(ymark(1)- $
  ymark(0))+ymark(0)], ['E']
if (pt eq 2) then xyouts, [.5*(xmark(1)-xmark(0))+xmark(0)], [.5*(ymark(1)- $
  ymark(0))+ymark(0)], ['Eht']
if (pt eq 0) then plot, add(mindir,ibeg:iend), add(maxdir,ibeg:iend), $
  xrange=[bot1,top1], psym=1, xtitle='B min', ytitle='B max', xtick_get= $
  xmark, ytick_get=ymark, yrange=[lowmax, highmax], xstyle=1, ystyle=1
if (pt eq 1) then plot, add(mindir,ibeg:iend), add(maxdir,ibeg:iend), $
  xrange=[bot1,top1], psym=1, xtitle='E min', ytitle='E max', xtick_get= $
  xmark, ytick_get=ymark, yrange=[lowmax, highmax], xstyle=1, ystyle=1
if (pt eq 2) then plot, add(mindir,ibeg:iend), add(maxdir,ibeg:iend), $
  xrange=[bot1,top1], psym=1, xtitle='Eht min', ytitle='Eht max', xtick_get= $
  xmark, ytick_get=ymark, yrange=[lowmax, highmax], xstyle=1, ystyle=1
oplot, add(mindir,ibeg:iend), add(maxdir,ibeg:iend), xrange=[bot1,top1]
if ((pt eq 1) or (pt eq 2)) then oplot, Binitmin, Binitmax
xyouts, [(add(mindir,ibeg) + .1*(top1-bot1)), (add(mindir,iend) + .1*(top1 - $
  bot1))],  [(add(maxdir,ibeg) + .1*diffmax),(add(maxdir,iend) - .1*diffmax)],$
  [ibeg, iend]
xyouts, [(add(mindir,ibeg) + .1*(top1-bot1)), (add(mindir,iend) + .1* $
  (top1-bot1))], [(add(maxdir,ibeg) + .1*diffmax), (add(maxdir,iend) - .1* $
  diffmax)], ['t = ', 't = ']
if (pt eq 0) then xyouts, [.5*(xmark(1)-xmark(0))+xmark(0)], [.5*(ymark(1)- $
  ymark(0))+ymark(0)], ['B']
if (pt eq 1) then xyouts, [.5*(xmark(1)-xmark(0))+xmark(0)], [.5*(ymark(1)- $
  ymark(0))+ymark(0)], ['E']
if (pt eq 2) then xyouts, [.5*(xmark(1)-xmark(0))+xmark(0)], [.5*(ymark(1)- $
  ymark(0))+ymark(0)], ['Eht']
return
end

































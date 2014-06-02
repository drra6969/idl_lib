PRO hwalenplot, t,vh,va,e,eh,vht,tmin,tmax, ccoef0,cstd,creg,strnn

;------------------------------------------------------------------------------
; Walen relation
;
; delta v = +/- delta v(alfven)
; where
; va(lfven) = B/sqrt(rho)    rho = mass density
; vh=v-vht

  !p.multi=[0,2,1,0,0]
  !P.CHARSIZE=1.0
  !P.FONT=0
  !P.CHARTHICK=1.
  
  A=findgen(17)*(!pi*2/16)
  usersym, cos(A), sin(A)
  
  vmin=min(vh) & vmax=max(vh) & avev=0.5*(vmin+vmax) & vdif=0.5*(vmax-vmin)
  bmin=min(va) & bmax=max(va) & aveb=0.5*(bmin+bmax) & bdif=0.5*(bmax-bmin)
  diff=1.02*max([vdif,bdif])
  xr=[aveb-diff,aveb+diff] &  yr=[avev-diff,avev+diff]

  plot, va(*,0), vh(*,0), pos=[.10, .38, .46, .88], $
    xrange=xr, yrange=yr, xstyle=1, ystyle=1, $
    title='Walen Relation',  $
    xtitle='V!dAlfven!n', ytitle='V - V!dht!n', psym=2
  oplot, va(*,1), vh(*,1), psym=8
  oplot, va(*,2), vh(*,2), psym=5
  lfit0=poly_fit(va,vh,1,yfit,yband,sigma,a0)
  oplot, va, yfit, linestyle=0
  
  bmin=min(e) & bmax=max(e) & aveb=0.5*(bmin+bmax) & bdif=0.5*(bmax-bmin)
  vmin=min(eh) & vmax=max(eh) & avev=0.5*(vmin+vmax) & vdif=0.5*(vmax-vmin)
  diff=1.02*max([vdif,bdif])
  xr=[aveb-diff,aveb+diff] &  yr=[avev-diff,avev+diff]
  plot, e(*,0), eh(*,0), pos=[.54, .38, .90, .88], $
    xrange=xr, yrange=yr, xstyle=1, ystyle=1,  $
    xtitle = 'E!dht!n', ytitle = 'E', psym=2
  oplot, e(*,1), eh(*,1), psym=8
  oplot, e(*,2), eh(*,2), psym=5
  lfit1=poly_fit(e,eh,1,yfit,yband,sigma,a1)
  oplot, e, yfit, linestyle=0

  ccoef0=findgen(2,2) & cstd=findgen(2,2) & creg=findgen(2)
  ccoef0(*,0)=lfit0(*) & ccoef0(*,1)=lfit1(*)
  cstd(0,0)=sqrt(a0(0,0)) & cstd(1,0)=sqrt(a0(1,1)) 
  cstd(0,1)=sqrt(a1(0,0)) & cstd(1,1)=sqrt(a1(1,1)) 
  creg(0)=correlate(va,vh) & creg(1)=correlate(e,eh)
  
  xyouts, .4, .95, '!17Variance Interval '+$
    string(tmin,'(f5.1)')+'-'+string(tmax,'(f5.1)')+'!3', $
    /norm, alignment=0.5
  xyouts, .8, .95, strnn, /norm, alignment=0.5
  
  plots, .05, .31, psym=2, /norm & xyouts, .06, .3, ' = X component', /norm
  plots, .05, .28, psym=8, /norm & xyouts, .06, .27, ' = Y component', /norm
  plots, .05, .25, psym=5, /norm & xyouts, .06, .24, ' = Z component', /norm
  xyouts, .05, .21, '___ = linear least squares fit, f=ax+b', /norm
  xyouts, .05, .17, 'Coefficients: ', /norm
  xyouts, .20, .17, 'a ='+string(ccoef0(1,0),'(f7.3)')$
                      +' +-'+string(cstd(1,0),'(f7.3)'), /norm
  xyouts, .20, .14, 'b =' +string(ccoef0(0,0),'(f7.1)')$
                      +' +-'+string(cstd(0,0),'(f7.1)'), /norm
  xyouts, .05, .11, 'Correlation coefficient: '+string(creg(0),'(f6.3)'), /norm
  
  plots, .75, .31, psym=2, /norm  & xyouts, .76, .3, ' = X component', /norm
  plots, .75, .28, psym=8, /norm & xyouts, .76, .27, ' = Y component', /norm
  plots, .75, .25, psym=5, /norm & xyouts, .76, .24, ' = Z component', /norm
  xyouts, .5, .21, '___ = linear least squares fit, f=ax+b', /norm
  xyouts, .5, .17, 'Coefficients: ',/norm
  xyouts, .65, .17, 'a ='+string(ccoef0(1,1),'(f7.3)')$
                      +' +-'+string(cstd(1,1),'(f7.3)'), /norm
  xyouts, .65, .14, 'b ='+string(ccoef0(0,1),'(f7.1)')$
                      +' +-'+string(cstd(0,1),'(f7.1)'), /norm
  xyouts, .5, .11, 'Correlation coefficient: '+string(creg(1),'(f6.3)'), /norm
  
  xyouts, .5, .07, 'Vht:', /norm
  xyouts, .6, .07, string(vht(0),'(f8.2)'), /norm
  xyouts, .7, .07, string(vht(1),'(f8.2)'), /norm
  xyouts, .8, .07, string(vht(2),'(f8.2)'), /norm

  !P.FONT=-1

return
end


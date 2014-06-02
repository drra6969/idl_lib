Pro powerspectrum, tvmin,tvmax,withps,smo,withunits,strnn

COMMON test, timeqs,nnn,neqs,veqs,bave,eeqs
COMMON units, nnorm,bnorm,vnorm,pnorm,lnorm,tnorm,tempnorm,jnorm,$
              nfac,bfac,vfac,pfac,lfac,tfac,tempfac,jfac

  print, 'Power spectrum for the Time Range:', tvmin, tvmax
  print, '  Number of data points:', nnn

  if nnn lt 10 then begin
    print, 'Not enough data; nnn should be > 9'
    return
endif

   bxp=fltarr(nnn) & byp=bxp & bzp=bxp
   bxp(*)=bave(0,*) & byp(*)=bave(1,*) & bzp(*)=bave(2,*)
   vxp=bxp & vyp=bxp & vzp=bxp
   eexp=bxp & eyp=bxp & ezp=bxp
   vxp(*)=veqs(0,*) & vyp(*)=veqs(1,*) & vzp(*)=veqs(2,*)
   eexp(*)=eeqs(0,*) & eyp(*)=eeqs(1,*) & ezp(*)=eeqs(2,*)
   bbp=sqrt(bxp*bxp+byp*byp+bzp*bzp)
   vvp=sqrt(vxp*vxp+vyp*vyp+vzp*vzp)
   eep=sqrt(eexp*eexp+eyp*eyp+ezp*ezp)

   if smo eq 'y' then begin
         bxp=smooth(bxp,3) & byp=smooth(byp,3) & bzp=smooth(bzp,3)         
         vxp=smooth(vxp,3) & vyp=smooth(vyp,3) & vzp=smooth(vzp,3)
         eexp=smooth(eexp,3) & eyp=smooth(eyp,3) & ezp=smooth(ezp,3)         
         bbp=smooth(bbp,3) & eep=smooth(eep,3) & eep=smooth(eep,3)         
   endif
   
   detrend,timeqs,bxp,bxd & detrend,timeqs,byp,byd & detrend,timeqs,bzp,bzd 
   detrend,timeqs,vxp,vxd & detrend,timeqs,vyp,vyd & detrend,timeqs,vzp,vzd 
   detrend,timeqs,eexp,exd & detrend,timeqs,eyp,eyd & detrend,timeqs,ezp,ezd 
   detrend,timeqs,bbp,bbd & detrend,timeqs,vvp,vvd & detrend,timeqs,eep,eed 

   fbx=FFT(HANNING(nnn)*bxd) & fby=FFT(HANNING(nnn)*byd)
   fbz=FFT(HANNING(nnn)*bzd) & fbb=FFT(HANNING(nnn)*bbd)
   fvx=FFT(HANNING(nnn)*vxd) & fvy=FFT(HANNING(nnn)*vyd)
   fvz=FFT(HANNING(nnn)*vzd) & fvv=FFT(HANNING(nnn)*vvd)
   fex=FFT(HANNING(nnn)*exd) & fey=FFT(HANNING(nnn)*eyd)
   fez=FFT(HANNING(nnn)*ezd) & fee=FFT(HANNING(nnn)*eed)

   pbx=abs(fbx(0:nnn/2.))^2 & pby=abs(fby(0:nnn/2.))^2 
   pbz=abs(fbz(0:nnn/2.))^2 & pbb=abs(fbb(0:nnn/2.))^2 
   pvx=abs(fvx(0:nnn/2.))^2 & pvy=abs(fvy(0:nnn/2.))^2 
   pvz=abs(fvz(0:nnn/2.))^2 & pvv=abs(fvv(0:nnn/2.))^2 
   pex=abs(fex(0:nnn/2.))^2 & pey=abs(fey(0:nnn/2.))^2 
   pez=abs(fez(0:nnn/2.))^2 & pee=abs(fee(0:nnn/2.))^2 
   pbb0=pbx+pby+pbz  & pbpert=pbb0-pbb
   pvv0=pvx+pvy+pvz  & pvpert=pvv0-pvv
   pee0=pex+pey+pez  & pepert=pee0-pee

   delft=(timeqs(nnn-1)-timeqs(0))/nnn
   m2=FINDGEN(nnn/2.+1)
   freq2=m2/(nnn*delft)
;   indeces=where(freq2 ge 0.5)
;   indeces2=where(freq2 ge 5.0)
;   ind=indeces(0)
;   ind2=indeces2(0)
;   ind3=N_ELEMENTS(freq2)-1
;ind2=N_ELEMENTS(freq2)-1
;normalization factor
   factor=2*nnn*delft
   fsig= freq2(0:nnn/2)
;   if ind2 ne -1 then begin
;   noise=total((factor*hpperp(ind2:ind3))*$
;                    (freq2(ind2:ind3)))/(ind3-ind2+1)
;   endif

    names=strarr(15) & names=replicate(' ',15)

    if withunits eq 'n' then begin
      tempustr= '' & densustr= '' & pustr   = ''
      bustr   = '' & vustr   = '' & justr   = '' & eustr   = ''
   endif
    if withunits eq 'y' then begin
      tempustr= 'keV' & densustr= 'cm!U-3!N' & pustr   = 'nPa'
      bustr   = 'nT' & vustr   = 'km/s' & justr   = 'nA/m!U2!N'
      eustr   = 'mV/m'
   endif


;plotcoordinates for 2nd column
    dpx=0.41  & dpy=0.165
    xab=0.545 & xeb=xab+dpx 
    ylo1=0.55 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3

;plotcoordinates for 1st column
    dpx=0.9  & dpy=0.41
    xap=0.085 & xep=xap+dpx 
    ylo0=0.53 & yup0=ylo0+dpy
    ylo1=ylo0-dpy & yup1=ylo0
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3


      tvlct,[0,255,0,100,0,255,230],[0,0,255,100,255,0,230],$
                                    [0,0,0,255,255,255,0]
      red   = 1
      green = 2
      blue  = 3
      yebb  = 4
      grbl  = 5
      yell  = 6
    col1=0 & col2=yell & col3=green & col4=red
    
    erase
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,1,1]
    !P.CHARSIZE=1.
    !P.CHARTHICK=1.
    !P.FONT=-1
;    !X.TICKS=0
;    !Y.TICKS=0
    !X.TICKlen=0.04
    !Y.TICKlen=0.03
    !X.THICK=1
    !Y.THICK=1
    if withps eq 'y' then begin
      !P.CHARSIZE=1.3
      !P.CHARTHICK=3.
      !P.THICK=6.
      !P.FONT=2
      !X.THICK=5
      !Y.THICK=5
    endif
         

;   Magnetic field
    nmin=1 & nmax=nnn/2
    fmin=freq2(nmin)   & fmax=freq2(nmax)
    !P.POSITION=[xap,ylo0,xep,yup0]
    bmax=max([pbb,pbb0]) & bmin=min([pbb,pbb0]) 
    plot, [fmin,fmax],[bmin,bmax],/nodata,$
          xrange=[fmin,fmax],yrange=[bmin,bmax],$
	  title='Magnetic Power Spectrum',/xlog,/ylog,$
	  xstyle=1,ystyle=1,xtickname=names,/noerase
    oplot, freq2(nmin:nmax), pbb(nmin:nmax),line=0,color=red
    oplot, freq2(nmin:nmax), pbb0(nmin:nmax),line=2,color=blue
;    xt0=min-0.10*del  &   yt0=bmin+0.62*delb    
;    xt1=min-0.12*del  &   yt1=bmin+0.45*delb    
;    xyouts, xt0, yt0, 'N',charsize=1
;    xyouts, xt1, yt1,densustr,charsize=0.9
;    xt0=max+0.08*del  &   yt0=bmin+0.62*delb    
;    xt1=max+0.07*del  &   yt1=bmin+0.45*delb    
;    xyouts, xt0, yt0, 'T',charsize=1
;    xyouts, xt1, yt1,tempustr,charsize=0.9

;   V
    !P.POSITION=[xap,ylo1,xep,yup1]
    bmax=max([pvv,pvv0]) & bmin=min([pvv,pvv0]) 
    plot, [fmin,fmax],[bmin,bmax],/nodata,$
          xrange=[fmin,fmax],yrange=[bmin,bmax],$
	  title='Velocity Power Spectrum',/xlog,/ylog,$
	  xstyle=1,ystyle=1,xtitle='freq',/noerase
    oplot, freq2(nmin:nmax), pvv(nmin:nmax),line=0,color=red
    oplot, freq2(nmin:nmax), pvv0(nmin:nmax),line=2,color=blue

;   E
;    !P.POSITION=[xap,ylo2,xep,yup2]
;    bmax=max([pee,pee0]) & bmin=min([pee,pee0]) 
;    plot, [fmin,fmax],[bmin,bmax],/nodata,$
;          xrange=[fmin,fmax],yrange=[bmin,bmax],$
;	  title='Electric Field Powers Spectrum',/xlog,/ylog,$
;	  xstyle=1,ystyle=1,/noerase
;    oplot, freq2(nmin:nmax), pee(nmin:nmax),line=0,color=red
;    oplot, freq2(nmin:nmax), pee0(nmin:nmax),line=2,color=blue
  !P.FONT=-1

  
return
end

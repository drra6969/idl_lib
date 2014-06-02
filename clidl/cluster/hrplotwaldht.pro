PRO hrplotwaldht, min, max, strnn,totnumint,t1a,t2a, withps,coordstrn,$
                  delt,fdelt,isc,slht0,slht1,slwal0,slwal1,itest,deltab

COMMON bfield, nfgm1,timem1,statsm1,b1,varbt1,varbb1, $
               nfgm2,timem2,statsm2,b2,varbt2,varbb2, $
               nfgm3,timem3,statsm3,b3,varbt3,varbb3, $
               nfgm4,timem4,statsm4,b4,varbt4,varbb4
COMMON pfield,ncis1,timep1,statsp1,np1,no1,nhe1,nhia1,vp1,vhia1,tp1,thia1, $
              ncis3,timep3,statsp3,np3,no3,nhe3,nhia3,vp3,vhia3,tp3,thia3, $
              ncis4,timep4,statsp4,np4,no4,nhe4,nhia4,vp4,vhia4,tp4,thia4 
COMMON sc, naux,time,sccoor,scvel,scdr1,scdr2,scdr3,scdr4,gsegsm,diptild, $
           re, rsc,dxsc,dysc,dzsc,volsc,volrat
COMMON bulk, n1,v1,t1,p1,n3,v3,t3,p3,n4,v4,t4,p4
COMMON current, njc,timec,jc,divb
COMMON plaxis,tax



    tax='m'
    names=strarr(15) & names=replicate(' ',15)

;plotcoordinates for SAT COORDINATES
    ddr=20.
    dqx=0.075 & dqy=0.12 & ddx=0.03
    xl1=0.77 & xu1=xl1+dqx
    xl2=xu1+ddx & xu2=xl2+dqx
    yl1=0.84 & yu1=yl1+dqy
;plotcoordinates for SAT SEPARATION
    dqx=0.075 & dqy=0.12 & ddx=0.03
    xsl1=0.54 & xsu1=xsl1+dqx
    xsl2=xsu1+ddx & xsu2=xsl2+dqx
    yl1=0.84 & yu1=yl1+dqy

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
    !P.CHARSIZE=0.7
    !P.FONT=-1
    !X.TICKS=0
    !Y.TICKS=0
    !X.TICKlen=0.04
    !Y.TICKlen=0.02
    del = max-min   
    
     colpol=190
     if withps ne 'y' then colpol=120 
     for inds=0,totnumint-1 do begin
;         print,'times:',t1a(inds),t2a(inds)
       xaa=(t1a(inds)-min)*(xep-xap)/del+xap
       xee=(t2a(inds)-min)*(xep-xap)/del+xap
       xs=[xaa,xee,xee,xaa]
       ys=[ylo4,ylo4,yup0,yup0]
;       print,xaa,xee,ylo4,yup1
       POLYFILL, xs, ys, COLOR = colpol, /norm
       xaa=(t1a(inds)-min)*(xeb-xab)/del+xab
       xee=(t2a(inds)-min)*(xeb-xab)/del+xab
       xs=[xaa,xee,xee,xaa]
       ys=[ylo4,ylo4,yup1,yup1]
;       print,xaa,xee,ylo4,yup1
       POLYFILL, xs, ys, COLOR = colpol, /norm
     endfor
     

; PLASMA DATA
    hindbd,min,max,ncis1,timep1,ip1s,ip1e
    hindbd,min,max,ncis3,timep3,ip3s,ip3e
    hindbd,min,max,ncis4,timep4,ip4s,ip4e    

;   DENSITY
    !P.POSITION=[xap,ylo0,xep,yup0]
    dum=0.
    if ip1e ge ip1s then begin
      tt1=fltarr(ip1e-ip1s+1) & dum=[dum,n1(ip1s:ip1e)] & endif
    if ip3e ge ip3s then begin
      tt3=fltarr(ip3e-ip3s+1) & dum=[dum,n3(ip3s:ip3e)] & endif
    if ip4e ge ip4s then begin
      tt4=fltarr(ip4e-ip4s+1) & dum=[dum,n4(ip4s:ip4e)] & endif
    bmax=max(dum) & if bmax eq 0.0 then bmax=1.
    bmin=0.01 & delb=bmax-bmin
    plot, [min,max],[bmin,bmax],/nodata,/ylog,$
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  title=strnn+' - '+coordstrn,$
	  xstyle=1,ystyle=2,xtickname=names,/noerase
    if ip1e ge ip1s then $
      oplot, timep1(ip1s:ip1e), n1(ip1s:ip1e),line=0,thick=1
    if ip3e ge ip3s then $
      oplot, timep3(ip3s:ip3e), n3(ip3s:ip3e),line=0,color=col3,thick=1
    if ip4e ge ip4s then $
      oplot, timep4(ip4s:ip4e), n4(ip4s:ip4e),line=0,color=col4,thick=1
    xt0=min-0.10*del  &   yt0=0.85*delb    
    xyouts, xt0, yt0, 'N',charsize=0.8

;   VX
    !P.POSITION=[xap,ylo1,xep,yup1]
    dum=0.
    if ip1e ge ip1s then begin
      tt1(*)=v1(0,ip1s:ip1e) & dum=[dum,tt1] & endif
    if ip3e ge ip3s then begin
      tt3(*)=v3(0,ip3s:ip3e) & dum=[dum,tt3] & endif
    if ip4e ge ip4s then begin 
      tt4(*)=v4(0,ip4s:ip4e) & dum=[dum,tt4] & endif
    bmax=max(dum) & bmin=min(dum) 
    if bmax le bmin then begin & bmax=1. & bmin=-1. & endif
    delb=bmax-bmin  
    plot, [min,max],[bmin,bmax],/nodata, $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if ip1e ge ip1s then $
      oplot, timep1(ip1s:ip1e), v1(0,ip1s:ip1e),line=0,thick=1
    if ip3e ge ip3s then $
      oplot, timep3(ip3s:ip3e), v3(0,ip3s:ip3e),line=0,color=col3,thick=1
    if ip4e ge ip4s then $
      oplot, timep4(ip4s:ip4e), v4(0,ip4s:ip4e),line=0,color=col4,thick=1
    xt0=min-0.12*del  &   yt0=bmin+0.85*delb    
    xyouts, xt0, yt0, 'V!DX!N',charsize=0.8

;   VY
    !P.POSITION=[xap,ylo2,xep,yup2]
    dum=0.
    if ip1e ge ip1s then begin
      tt1(*)=v1(1,ip1s:ip1e) & dum=[dum,tt1] & endif
    if ip3e ge ip3s then begin
      tt3(*)=v3(1,ip3s:ip3e) & dum=[dum,tt3] & endif
    if ip4e ge ip4s then begin 
      tt4(*)=v4(1,ip4s:ip4e) & dum=[dum,tt4] & endif
    bmax=max(dum) & bmin=min(dum) 
    if bmax le bmin then begin & bmax=1. & bmin=-1. & endif
    delb=bmax-bmin  
    plot, [min,max],[bmin,bmax],/nodata, $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if ip1e ge ip1s then $
      oplot, timep1(ip1s:ip1e), v1(1,ip1s:ip1e),line=0,thick=1
    if ip3e ge ip3s then $
      oplot, timep3(ip3s:ip3e), v3(1,ip3s:ip3e),line=0,color=col3,thick=1
    if ip4e ge ip4s then $
      oplot, timep4(ip4s:ip4e), v4(1,ip4s:ip4e),line=0,color=col4,thick=1
    xt0=min-0.12*del  &   yt0=bmin+0.85*delb    
    xyouts, xt0, yt0, 'V!DY!N',charsize=0.8

;   VZ
    !P.POSITION=[xap,ylo3,xep,yup3]
    dum=0.
    if ip1e ge ip1s then begin
      tt1(*)=v1(2,ip1s:ip1e) & dum=[dum,tt1] & endif
    if ip3e ge ip3s then begin
      tt3(*)=v3(2,ip3s:ip3e) & dum=[dum,tt3] & endif
    if ip4e ge ip4s then begin 
      tt4(*)=v4(2,ip4s:ip4e) & dum=[dum,tt4] & endif
    bmax=max(dum) & bmin=min(dum) 
    if bmax le bmin then begin & bmax=1. & bmin=-1. & endif
    delb=bmax-bmin  
    plot, [min,max],[bmin,bmax],/nodata, $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if ip1e ge ip1s then $
      oplot, timep1(ip1s:ip1e), v1(2,ip1s:ip1e),line=0,thick=1
    if ip3e ge ip3s then $
      oplot, timep3(ip3s:ip3e), v3(2,ip3s:ip3e),line=0,color=col3,thick=1
    if ip4e ge ip4s then $
      oplot, timep4(ip4s:ip4e), v4(2,ip4s:ip4e),line=0,color=col4,thick=1
    xt0=min-0.12*del  &   yt0=bmin+0.85*delb    
    xyouts, xt0, yt0, 'V!DZ!N',charsize=0.8

;   TEMPERATURE
    !P.POSITION=[xap,ylo4,xep,yup4]
    dum=0.
    if ip1e ge ip1s then dum=[dum,t1(ip1s:ip1e)]
    if ip3e ge ip3s then dum=[dum,t3(ip3s:ip3e)]
    if ip4e ge ip4s then dum=[dum,t4(ip4s:ip4e)]
    bmax=max(dum) & if bmax eq 0.0 then bmax=1.
    bmin=0.0 & delb=bmax-bmin 
    plot, [min,max],[bmin,bmax],/nodata, $
          xtickformat='XTICKS',xminor=xminor, $
          xrange=[min,max],yrange=[0,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if ip1e ge ip1s then $
      oplot, timep1(ip1s:ip1e), t1(ip1s:ip1e),line=0,thick=1
    if ip3e ge ip3s then $
      oplot, timep3(ip3s:ip3e), t3(ip3s:ip3e),line=0,color=col3,thick=1
    if ip4e ge ip4s then $
      oplot, timep4(ip4s:ip4e), t4(ip4s:ip4e),line=0,color=col4,thick=1
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xt1=min-0.12*del  &   yt1=bmin+0.74*delb    
    xyouts, xt0, yt0, 'T',charsize=0.8
    xyouts, xt1, yt1, '10!U6!NK',charsize=0.7
    xyouts, min, bmin-0.3*(bmax-bmin), 'search t and dt : '$
     +string(delt,'(f5.2)')$
    +' ; '+string(fdelt,'(f5.2)'),charsize=0.6
    xyouts, min, bmin-0.4*(bmax-bmin), 'spacecraft='+string(isc,'(i1.1)')$
                             ,charsize=0.6
    xyouts,(min+max)/2., bmin-0.4*(bmax-bmin), 'delta b='+string(deltab,$
                   '(f5.2)'),charsize=0.6


    if itest ne 2 then begin
    xyouts, max, bmin-0.3*(bmax-bmin), 'slope range for W:'$
                 +string(slwal0,'(f5.2)')$
                            +' ; '+string(slwal1,'(f5.2)'),charsize=0.6
    endif

    if itest ne 1 then begin
    xyouts, max, bmin-0.4*(bmax-bmin), 'slope range for HT:'$
    +string(slht0,'(f5.2)')$
                            +';'+string(slht1,'(f5.2)'),charsize=0.6
    endif


; SATELLITE LOCATION
    aa0=14. & bb0=9./400.
    aa1=11. & bb1=22./400.
    yy0=findgen(41)-20. & xx0=-bb0*yy0*yy0+aa0
    yy1=yy0             & xx1=-bb1*yy0*yy0+aa1
    hindbd,min,max,naux,time,its,ite

   print,'P6'
    !P.POSITION=[xl1,yl1,xu1,yu1]
    plot, xx0,yy0,line=1,$
	  xrange=[ddr,-ddr], yrange=[ddr,-ddr], xstyle=1,ystyle=1,$
	  xtitle='X',charsize=0.6,/noerase 
    oplot, rsc(0,its:ite), rsc(1,its:ite),line=0,color=red, thick=2
    oplot, xx1,yy1,line=1
    oplot,[0.,0.],[-ddr,ddr],line=2
    oplot,[-ddr,ddr],[0.,0.],line=2
    plots, rsc(0,its), rsc(1,its),psym=2
    plots, rsc(0,ite), rsc(1,ite),psym=5
    xt0=1.6*ddr  &   yt0=0.08*ddr  
    xyouts, xt0, yt0, 'Y',charsize=0.7

    !P.POSITION=[xl2,yl1,xu2,yu1]
    plot, xx0,yy0,line=1, $
	  xrange=[ddr,-ddr], yrange=[-ddr,ddr], xstyle=1,ystyle=1,$
	  xtitle='X',charsize=0.6,/noerase 
    oplot, rsc(0,its:ite), rsc(2,its:ite),line=0,color=red, thick=2
    oplot, xx1,yy1,line=1
    oplot,[0.,0.],[-ddr,ddr],line=2
    oplot,[-ddr,ddr],[0.,0.],line=2
    plots, rsc(0,its), rsc(2,its),psym=2
    plots, rsc(0,ite), rsc(2,ite),psym=4
    xt0=1.6*ddr  &   yt0=-0.08*ddr  
    xyouts, xt0, yt0, 'Z',charsize=0.7

; SATELLITE SEPARATION
    dscmin=min([scdr1,scdr2,scdr3,scdr4]) & dscmax=max([scdr1,scdr2,scdr3,scdr4])
    l0=0.001 & dscmin=l0*dscmin & dscmax=l0*dscmax
    !P.POSITION=[xsl1,yl1,xsu1,yu1]
    plot, [0.,0.],[dscmin,dscmax],line=1,$
	  xrange=[dscmax,dscmin], yrange=[dscmax,dscmin], xstyle=1,ystyle=1,$
	  xtitle='X',charsize=0.6,/noerase 
    oplot, [dscmin,dscmax],[0.,0.],line=1
    oplot, l0*scdr1(0,its:ite), l0*scdr1(1,its:ite),thick=1
    oplot, l0*scdr2(0,its:ite), l0*scdr2(1,its:ite),color=col2, thick=2
    oplot, l0*scdr4(0,its:ite), l0*scdr4(1,its:ite),color=col4, thick=2
    plots, l0*scdr1(0,its), l0*scdr1(1,its),psym=2
    plots, l0*scdr2(0,its), l0*scdr2(1,its),psym=2,color=col2
    plots, l0*scdr4(0,its), l0*scdr4(1,its),psym=2,color=col4
    plots, l0*scdr1(0,ite), l0*scdr1(1,ite),psym=5
    plots, l0*scdr2(0,ite), l0*scdr2(1,ite),psym=5,color=col2
    plots, l0*scdr4(0,ite), l0*scdr4(1,ite),psym=5,color=col4
    plots, 0., 0.,psym=4,color=col3
    del=dscmax-dscmin
    xt0=dscmax+0.33*del  &   yt0=0.4*(dscmax+dscmin) 
    xyouts, l0*scdr1(0,its), l0*scdr1(1,its),'1'
    xyouts, l0*scdr2(0,its), l0*scdr2(1,its),'2'
    xyouts, l0*scdr4(0,its), l0*scdr4(1,its),'4'
    xyouts, 0.,0.,'3'
    xyouts, xt0, yt0, 'Y',charsize=0.7

    !P.POSITION=[xsl2,yl1,xsu2,yu1]
    plot, [0.,0.],[dscmin,dscmax],line=1, $
	  xrange=[dscmax,dscmin], yrange=[dscmin,dscmax], xstyle=1,ystyle=1,$
	  xtitle='X',charsize=0.6,/noerase 
    oplot, [dscmin,dscmax],[0.,0.],line=1
    oplot, l0*scdr1(0,its:ite),l0*scdr1(2,its:ite),thick=1
    oplot, l0*scdr2(0,its:ite),l0*scdr2(2,its:ite),color=col2, thick=2
    oplot, l0*scdr4(0,its:ite),l0*scdr4(2,its:ite),color=col4, thick=2
    plots, l0*scdr1(0,its), l0*scdr1(2,its),psym=2
    plots, l0*scdr2(0,its), l0*scdr2(2,its),psym=2,color=col2
    plots, l0*scdr4(0,its), l0*scdr4(2,its),psym=2,color=col4
    plots, l0*scdr1(0,ite), l0*scdr1(2,ite),psym=5
    plots, l0*scdr2(0,ite), l0*scdr2(2,ite),psym=5,color=col2
    plots, l0*scdr4(0,ite), l0*scdr4(2,ite),psym=5,color=col4
    plots, 0., 0.,psym=4,color=col3
    xt0=dscmax+0.33*del  &   yt0=0.6*(dscmax+dscmin) 
    xyouts, l0*scdr1(0,its), l0*scdr1(2,its),'1'
    xyouts, l0*scdr2(0,its), l0*scdr2(2,its),'2'
    xyouts, l0*scdr4(0,its), l0*scdr4(2,its),'4'
    xyouts, 0.,0.,'3'
    xyouts, xt0, yt0, 'Z',charsize=0.7
    

; MAGNETIC DATA
    hindbd,min,max,nfgm1,timem1,im1s,im1e
    hindbd,min,max,nfgm2,timem2,im2s,im2e
    hindbd,min,max,nfgm3,timem3,im3s,im3e
    hindbd,min,max,nfgm4,timem4,im4s,im4e    
    del = max-min    

;   BX
    !P.POSITION=[xab,ylo1,xeb,yup1]
    dum=0.
    if im1e ge im1s then begin
      tt1=fltarr(im1e-im1s+1) & tt1(*)=b1(0,im1s:im1e) & dum=[dum,tt1] & endif
    if im2e ge im2s then begin
      tt2=fltarr(im2e-im2s+1) & tt2(*)=b2(0,im2s:im2e) & dum=[dum,tt2] & endif
    if im3e ge im3s then begin
      tt3=fltarr(im3e-im3s+1) & tt3(*)=b3(0,im3s:im3e) & dum=[dum,tt3] & endif
    if im4e ge im4s then begin
      tt4=fltarr(im4e-im4s+1) & tt4(*)=b4(0,im4s:im4e) & dum=[dum,tt4] & endif
    bmax=max(dum) & bmin=min(dum) 
    if bmax le bmin then begin & bmax=1. & bmin=-1. & endif
    delb=bmax-bmin  
    plot, [min,max],[bmin,bmax],/nodata, $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  title=strnn+' - '+coordstrn,$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if im1e ge im1s then $
      oplot, timem1(im1s:im1e), b1(0,im1s:im1e),line=0,thick=1
    if im2e ge im2s then $
      oplot, timem2(im2s:im2e), b2(0,im2s:im2e),line=0,color=col2,thick=1
    if im3e ge im3s then $
      oplot, timem3(im3s:im3e), b3(0,im3s:im3e),line=0,color=col3,thick=1
    if im4e ge im4s then $
      oplot, timem4(im4s:im4e), b4(0,im4s:im4e),line=0,color=col4,thick=1
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xt1=min-0.10*del  &   yt1=bmin+0.73*delb    
    xyouts, xt0, yt0, 'B!DX!N',charsize=0.8
    xyouts, xt1, yt1, 'nT',charsize=0.7

;   BY
    !P.POSITION=[xab,ylo2,xeb,yup2]
    dum=0.
    if im1e ge im1s then begin
      tt1(*)=b1(1,im1s:im1e) & dum=[dum,tt1] & endif
    if im2e ge im2s then begin
      tt2(*)=b2(1,im2s:im2e) & dum=[dum,tt2] & endif
    if im3e ge im3s then begin
      tt3(*)=b3(1,im3s:im3e) & dum=[dum,tt3] & endif
    if im4e ge im4s then begin
      tt4(*)=b4(1,im4s:im4e) & dum=[dum,tt4] & endif
    bmax=max(dum) & bmin=min(dum) 
    if bmax le bmin then begin & bmax=1. & bmin=-1. & endif
    delb=bmax-bmin  
    plot, [min,max],[bmin,bmax],/nodata, $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if im1e ge im1s then $
      oplot, timem1(im1s:im1e), b1(1,im1s:im1e),line=0,thick=1
    if im2e ge im2s then $
      oplot, timem2(im2s:im2e), b2(1,im2s:im2e),line=0,color=col2,thick=1
    if im3e ge im3s then $
      oplot, timem3(im3s:im3e), b3(1,im3s:im3e),line=0,color=col3,thick=1
    if im4e ge im4s then $
      oplot, timem4(im4s:im4e), b4(1,im4s:im4e),line=0,color=col4,thick=1
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xt1=min-0.10*del  &   yt1=bmin+0.73*delb    
    xyouts, xt0, yt0, 'B!DY!N',charsize=0.8
    xyouts, xt1, yt1, 'nT',charsize=0.7

;   BZ
    !P.POSITION=[xab,ylo3,xeb,yup3]
    dum=0.
    if im1e ge im1s then begin
      tt1(*)=b1(2,im1s:im1e) & dum=[dum,tt1] & endif
    if im2e ge im2s then begin
      tt2(*)=b2(2,im2s:im2e) & dum=[dum,tt2] & endif
    if im3e ge im3s then begin
      tt3(*)=b3(2,im3s:im3e) & dum=[dum,tt3] & endif
    if im4e ge im4s then begin
      tt4(*)=b4(2,im4s:im4e) & dum=[dum,tt4] & endif
    bmax=max(dum) & bmin=min(dum) 
    if bmax le bmin then begin & bmax=1. & bmin=-1. & endif
    delb=bmax-bmin  
    plot, [min,max],[bmin,bmax],/nodata, $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    if im1e ge im1s then $
      oplot, timem1(im1s:im1e), b1(2,im1s:im1e),line=0,thick=1
    if im2e ge im2s then $
      oplot, timem2(im2s:im2e), b2(2,im2s:im2e),line=0,color=col2,thick=1
    if im3e ge im3s then $
      oplot, timem3(im3s:im3e), b3(2,im3s:im3e),line=0,color=col3,thick=1
    if im4e ge im4s then $
      oplot, timem4(im4s:im4e), b4(2,im4s:im4e),line=0,color=col4,thick=1
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xt1=min-0.10*del  &   yt1=bmin+0.73*delb    
    xyouts, xt0, yt0, 'B!DZ!N',charsize=0.8
    xyouts, xt1, yt1, 'nT',charsize=0.7

;   CURRENT DENSITY
    hindbd,min,max,njc,timec,ics,ice    
    !P.POSITION=[xab,ylo4,xeb,yup4]
    if im1e ge im1s then begin
      bmax=max(jc(*,ics:ice)) & bmin=min(jc(*,ics:ice))
    endif else begin
      bmax=1.e-9 & bmin=-1.e-9
    endelse 
    j0=1.e9 & bmax=j0*bmax & bmin=j0*bmin
    delb=bmax-bmin 
    plot, [min,max],[bmin,bmax],/nodata, $
          xtickformat='XTICKS',xminor=xminor, $
	  xrange=[min,max], yrange=[bmin,bmax], $
	  xstyle=1,ystyle=2,$;xtickname=names, $
	  xtitle='time',/noerase
    if im1e ge im1s then begin
      oplot, timec(ics:ice), j0*jc(0,ics:ice)
      oplot, timec(ics:ice), j0*jc(1,ics:ice),line=2,color=blue,thick=1
      oplot, timec(ics:ice), j0*jc(2,ics:ice),line=3,color=red,thick=1
    endif
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xt1=min-0.13*del  &   yt1=bmin+0.74*delb    
    xyouts, xt0, yt0, 'J',charsize=0.8
    xyouts, xt1, yt1, 'nA/m!U2!N',charsize=0.7


  return
end


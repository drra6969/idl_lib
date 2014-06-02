PRO plotsum, nn, time, min, max, its, ite, strnn, withps

COMMON fields, b,varbt,varbb,n,v,t,np,no,nhe,nhia,vp,vhia,tp,thia
COMMON sc, rsc,dxsc,dysc,dzsc,volsc,volrat
COMMON plaxis,tax

    tax='m'
    names=strarr(15) & names=replicate(' ',15)

;plotcoordinates for sat coordinates
    ddr=20.
    dqx=0.15 & dqy=0.12 & ddx=0.06
    xl1=0.5 & xu1=xl1+dqx
    xl2=xu1+ddx & xu2=xl2+dqx
    yl1=0.84 & yu1=yl1+dqy
;plotcoordinates for line plots
    dpx=0.7  & dpy=0.17
    xa=0.15 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.58 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3

    if (withps eq 'n') then begin
      red   = 255
      green = 256l*255
      blue  = 256l*256l*255+256l*100 +100
      yebb  = 256l*255+256l*256l*255
      grbl  = 255+256l*256l*255
      yell  = 255+256l*255
    endif
    if (withps eq 'y') then begin
      tvlct,[0,255,0,100,0,255,255],[0,0,255,100,255,0,255],$
                                    [0,0,0,255,255,255,0]
      red   = 1
      green = 2
      blue  = 3
      yebb  = 4
      grbl  = 5
      yell  = 6
    endif
    
    
    erase
    !P.REGION=[0.,0.,1.0,1.0]
    !P.MULTI=[0,1,1]
    !P.CHARSIZE=1.0
    !P.FONT=-1
    !X.TICKS=0
    !Y.TICKS=0
    !X.TICKlen=0.04
    !Y.TICKlen=0.02
    del = max-min
    aa0=14. & bb0=9./400.
    aa1=11. & bb1=22./400.
    yy0=findgen(41)-20. & xx0=-bb0*yy0*yy0+aa0
    yy1=yy0             & xx1=-bb1*yy0*yy0+aa1
    
    !P.POSITION=[xl1,yl1,xu1,yu1]
    plot, xx0,yy0,line=1,$
          xminor=xminor, $
	  xrange=[ddr,-ddr], yrange=[ddr,-ddr], xstyle=1,ystyle=1,$
	  xtitle='X',charsize=0.8,/noerase 
    oplot, rsc(0,its:ite), rsc(1,its:ite),line=0,color=red, thick=2
    oplot, xx1,yy1,line=1
    oplot,[0.,0.],[-ddr,ddr],line=2
    oplot,[-ddr,ddr],[0.,0.],line=2
    plots, rsc(0,its), rsc(1,its),psym=2
    plots, rsc(0,ite), rsc(1,ite),psym=5
    xt0=1.6*ddr  &   yt0=0.08*ddr  
    xyouts, xt0, yt0, 'Y',charsize=0.9


    !P.POSITION=[xl2,yl1,xu2,yu1]
    plot, xx0,yy0,line=1, $
          xminor=xminor, $
	  xrange=[ddr,-ddr], yrange=[-ddr,ddr], xstyle=1,ystyle=1,$
	  xtitle='X',charsize=0.8,/noerase 
    oplot, rsc(0,its:ite), rsc(2,its:ite),line=0,color=red, thick=2
    oplot, xx1,yy1,line=1
    oplot,[0.,0.],[-ddr,ddr],line=2
    oplot,[-ddr,ddr],[0.,0.],line=2
    plots, rsc(0,its), rsc(2,its),psym=2
    plots, rsc(0,ite), rsc(2,ite),psym=4
    xt0=1.6*ddr  &   yt0=-0.08*ddr  
    xyouts, xt0, yt0, 'Z',charsize=0.9
    
;   Satellite separation and ratio of actual to ideal tetrahedron volume
    !P.POSITION=[xa,ylo1,xe,yup1]
    bmax=max([dxsc(its:ite),dysc(its:ite),dzsc(its:ite)])
    delb=bmax  & bmax=1.1*bmax
    plot, time(its:ite), dxsc(its:ite), $
          xminor=xminor, xrange=[min,max],yrange=[0,bmax], $
	  xstyle=1,ystyle=9,xtickname=names,$
	  title=strnn,/noerase 
    oplot, time(its:ite), dysc(its:ite),line=1
    oplot, time(its:ite), dzsc(its:ite),line=2
    axis,yaxis=1,yrange=[0,1.],ystyle=1   
    oplot, time(its:ite), volrat(its:ite)*bmax, line=3,color=green, thick=2
    xt0=min+0.23*del  &   yt0=0.85*delb    
    xt1=max+0.07*del  &   yt1=0.85*delb    
    xyouts, xt0, yt0, 'SC!Dseparation!N - x,y,z',charsize=1.2
    xyouts, xt1, yt1, 'T!Dvol!N',charsize=1.2
    
; Density and Total Pressure
    kb=1.38e-23
    mu0=4.*!pi*1.e-7   & ptot=n & pb=n
    pb(*) = 0.5*(b(0,*)^2+b(1,*)^2+b(2,*))*1.e-18/mu0
    ptot  = n*T*kb*1.e12 + pb

    !P.POSITION=[xa,ylo2,xe,yup2]
    bmax=max(n(its:ite))
    delb=bmax  & bmax=1.1*bmax  
    plot, time(its:ite), n(its:ite), $
          xminor=xminor, xrange=[min,max],yrange=[0,bmax],$
	  xstyle=1,ystyle=9,xtickname=names,/noerase 
    pmax=max(1e9*ptot(its:ite))
    axis,yaxis=1,yrange=[0,pmax],ystyle=1   
    oplot, time(its:ite), 1e9*ptot(its:ite)/pmax*bmax, $
                                   line=2,color=green,thick=2
    oplot, time(its:ite), 1e9*pb(its:ite)/pmax*bmax, $
                                   line=2,color=blue,thick=3
    xt0=min-0.10*del  &   yt0=0.85*delb    
    xt1=max+0.07*del  &   yt1=0.85*delb 
    xt2=max+0.09*del  &   yt2=0.7*delb   
    xyouts, xt0, yt0, 'N',charsize=1.2
    xyouts, xt1, yt1, 'P!Dtot!N, P!DB!N',charsize=1.2    
    xyouts, xt2, yt2, 'nPa',charsize=1.0   
    
    !P.POSITION=[xa,ylo3,xe,yup3]
    bmax=max([b(*,its:ite)]) & bmin=min([b(*,its:ite)])
    print,'Vmin, Vmax:',bmin,bmax
    delb=bmax-bmin
    plot, time(its:ite), b(0,its:ite), $
          xminor=xminor, xrange=[min,max],yrange=[bmin,bmax],$
	  xstyle=1,ystyle=2,xtickname=names,/noerase 
    oplot, time(its:ite), b(1,its:ite),line=2,color=blue,thick=2
    oplot, time(its:ite), b(2,its:ite),line=3,color=red,thick=2
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xyouts, xt0, yt0, 'B',charsize=1.2

    
    !P.POSITION=[xa,ylo4,xe,yup4]
    bmax=max([v(*,its:ite)]) & bmin=min([v(*,its:ite)])
    print,'Bmin, Bmax:',bmin,bmax
    delb=bmax-bmin
    plot, time(its:ite), v(0,its:ite), $
          xtickformat='XTICKS',xminor=xminor, $
	  xrange=[min,max], yrange=[bmin,bmax], $
	  xstyle=1,ystyle=2,$;xtickname=names, $
	  xtitle='time',/noerase
    oplot, time(its:ite), v(1,its:ite),line=2,color=blue,thick=2
    oplot, time(its:ite), v(2,its:ite),line=3,color=red,thick=2
    xt0=min-0.10*del  &   yt0=bmin+0.85*delb    
    xyouts, xt0, yt0, 'V',charsize=1.2
    

  return
end

PRO plotvarsum, tmin,tmax,timeb,timev,baxr,bayr,bazr,$
                strnn,totnumint,t1a,t2a,wps
COMMON ref, bxr,byr,bzr,vxr,vyr,vzr, $
            rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            xsat1,ysat1,zsat1,vxsat1,vysat1,vzsat1, $
            xsat2,ysat2,zsat2,vxsat2,vysat2,vzsat2, $
            index, starttime, phi, xtit, withps
COMMON plaxis,tax


    names=strarr(15) & names=replicate(' ',15)
    dpx=0.7  & dpy=0.2
    xa=0.12 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.7 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3
    min = tmin &  max=tmax
       
       erase
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,1,5,0,0]
       !P.CHARSIZE=2.5
       !P.FONT=-1
       !X.TICKS=0
       !Y.TICKS=0
       !X.TICKlen=0.04
       !Y.TICKlen=0.02
;       !Y.RANGE=[ymin,ymax]
       if (max-min) gt 1.5 then tax = 'h' else tax = 'm'
       if 60.*(max-min) le 7. then tax = 'ms'
       if 60.*(max-min) le 1.5 then tax = 's'
       case tax of
         'h': begin tplb=timeb & tplv=timev & xminor=6 & end
         'm': begin min=60.*min    & max=60.*max
                    tplb=60.*timeb & tplv=60.*timev &  & xminor=0 & end
         'ms': begin min=60.*min    & max=60.*max
                    tplb=60.*timeb & tplv=60.*timev &  & xminor=6 & end
         's': begin min=3600.*min    & max=3600.*max
                    tplb=3600.*timeb & tplv=3600.*timev &  & xminor=0 & end
       endcase
       del = max-min
       !X.RANGE=[min,max]
;       print,'trange:', min,max
;       print,'xrange:', xa,xe

     colpol=240
     if wps ne 'y' then colpol=120 
     for inds=0,totnumint-1 do begin
;         print,'times:',t1a(inds),t2a(inds)
       xaa=(t1a(inds)-tmin)*(xe-xa)/(tmax-tmin)+xa
       xee=(t2a(inds)-tmin)*(xe-xa)/(tmax-tmin)+xa
       xs=[xaa,xee,xee,xaa]
       ys=[ylo4,ylo4,yup1,yup1]
;       print,xaa,xee,ylo4,yup1
       POLYFILL, xs, ys, COLOR = colpol, /norm
     endfor
       
	!P.POSITION=[xa,ylo1,xe,yup1]
        if max(4.*rhor) le max(babr) then bmax=max([10.*rhor,babr])  $
          else bmax=max([rhor,babr])
;        print, 'B max: ',bmax
        bmin=min([10.*rhor,babr]) ;&   print, 'B min: ',bmin
        bmin=0.0
;        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.1*delb  & delb=bmax
	if max(4.*rhor) le max(babr) then $
           plot, tplv, 100.*rhor, $
           xminor=xminor, $
	   yrange=[0,bmax], xstyle=1,ystyle=9,xtickname=names,/noerase $
        else plot, tplv, rhor, $
           xminor=xminor, $
	   yrange=[0,bmax], xstyle=1,ystyle=9,xtickname=names,/noerase
	oplot, tplb, babr, line=1
;	ytick_get=vv

        tempmax=max(tempr) ;& print, 'Temp, max: ',tempmax
        temprange = 10000.   & tempscale = bmax/temprange
	axis,yaxis=1,yrange=[0,temprange],ystyle=1
	oplot, tplv, tempscale*tempr, line=2
        xt0=min-0.1*del  &   yt0=bmin+0.85*delb    
        xt0a=min-0.2*del  &  yt0a=bmin+0.6*delb 
        xt1=max+0.16*del  &   yt1=bmin+0.9*delb    
        xt2=max+0.09*del  &   yt2=bmin+0.68*delb 
        yt3=bmin+0.44*delb  & yt4=bmin+0.20*delb 
        xt2a=max+0.12*del   & yt2a=bmin+0.57*delb 
        yt3a=bmin+0.33*delb & yt4a=bmin+0.09*delb
        xyouts, xt0, yt0, 'B',charsize=1.8
        xyouts, xt1, yt1,'T',charsize=1.8
        xyouts, xt2, yt2,'Dens. (cm!U-3!N)',charsize=1.5
        if max(4.*rhor) le max(babr) then begin
           xyouts, xt0, yt0a, '10N',charsize=1.8
           xyouts, xt2a, yt2a,'10 N ___',charsize=1.5
        endif else begin
          xyouts, xt0, yt0a, 'N',charsize=1.8
          xyouts, xt2a, yt2a,'N ___',charsize=1.5
        endelse
        xyouts, xt2, yt3, 'Magn.F. (nT)',charsize=1.5
        xyouts, xt2a, yt3a, 'B .....',charsize=1.5
        xyouts, xt2, yt4, 'Temp. (eV)',charsize=1.5
        xyouts, xt2a, yt4a, 'T _ _',charsize=1.5

  titl='!17 Plasma and Field Data,  !3'+strnn
  xyouts, .45, 0.93, titl, charsize=1.55,charthick=2.0,/norm, alignment=0.5

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([pr,pbr,ptotr]) ;& print, 'p max: ',bmax
        bmin=min([pr,pbr,ptotr]) ;& print, 'p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        bmin=0.0
        delb=bmax
        bmax=bmax+0.1*delb  & delb=bmax
	plot, tplv, ptotr, yrange=[0,bmax], $
           xminor=xminor, $
	   xstyle=1,ystyle=1,xtickname=names,/noerase
	oplot, tplv, pr, line=1
	oplot, tplv, pbr, line=2
        xt1=max+0.02*del   &  yt1=bmin+0.73*delb    
        xt1a=max+0.04*del  &  yt1a=bmin+0.60*delb    
        xt2=max+0.04*del   &  yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Pressure',charsize=1.5
        xyouts, xt1a, yt1a,'(nPascal)',charsize=1.5
        xyouts, xt2, yt2,'P!Dtot!N ___',charsize=1.5
        xyouts, xt2, yt3, 'P!Dth!N ......',charsize=1.5
        xyouts, xt2, yt4, 'P!DB!N _ _',charsize=1.5
        
	!P.POSITION=[xa,ylo3,xe,yup3]
        bmax=max([vxr,vyr,vzr]) ;&   print, 'V max: ',bmax
        bmin=min([vxr,vyr,vzr]) ;&   print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=bmax-bmin
	plot, tplv, vxr, yrange=[bmin,bmax], $
           xminor=xminor, $
	   xstyle=1,ystyle=1,xtickname=names, line=2,/noerase
	oplot, tplv, vyr, line=1
	oplot, tplv, vzr, line=0
        yt1=bmin+0.73*delb    
        yt1a=bmin+0.60*delb    
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Velocity',charsize=1.5
        xyouts, xt1a, yt1a,'(km s!U-1!N)',charsize=1.5
        xyouts, xt2, yt2,'V!Dx!N _ _',charsize=1.5
        xyouts, xt2, yt3, 'V!Dy!N ......',charsize=1.5
        xyouts, xt2, yt4, 'V!Dz!N ___',charsize=1.5
        
	!P.POSITION=[xa,ylo4,xe,yup4]
        bmax=max([bxr,byr,bzr]) ;&   print, 'B max: ',bmax
        bmin=min([bxr,byr,bzr]) ;;&   print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=bmax-bmin
	plot, tplb, bxr, yrange=[bmin,bmax], $
           xtickformat='XTICKS', xminor=xminor, $
	   xstyle=1,ystyle=1,xtitle=xtit, line=2,/noerase
	oplot, tplb, byr, line=1
	oplot, tplb, bzr, line=0
        yt1=bmin+0.73*delb   
        yt1a=bmin+0.60*delb   
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Magn. Field',charsize=1.5
        xyouts, xt1a, yt1a,'(nT)',charsize=1.5
        xyouts, xt2, yt2,'B!Dx!N _ _',charsize=1.5
        xyouts, xt2, yt3, 'B!Dy!N ......',charsize=1.5
        xyouts, xt2, yt4, 'B!Dz!N ___',charsize=1.5

    min = tmin &  max = tmax
        

;  if satchoice eq '2' then begin
;    xyouts, 0.05, 0.2, 'Data represents a cut throuth system for const y.', $
;       charsize=1.2, /norm
;    xyouts, 0.05, 0.17, 'Location is in simulation units and '$
;                         +'velocities are in magnetospheric frame', $
;       charsize=1.2, /norm 
;    xyouts, 0.05, 0.14, strnn, charsize=1.2, /norm
;  endif
        
return
end

PRO pltrvar, itot,min,max,time2d,satchoice,strnn
COMMON ref, bxr,byr,bzr,vxr,vyr,vzr, $
            rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            xsat1,ysat1,zsat1,vxsat1,vysat1,vzsat1, $
            xsat2,ysat2,zsat2,vxsat2,vysat2,vzsat2, $
            index, starttime, phi, xtit, withps


    names=strarr(15) & names=replicate(' ',15)
    dpx=0.63  & dpy=0.17
    xa=0.07 & xe=xa+dpx 
    hopp=0.03     ;to seperate plots if desired
    ylo1=0.8 & yup1=ylo1+dpy
    ylo2=ylo1-dpy & yup2=ylo1
    ylo3=ylo2-dpy & yup3=ylo2
    ylo4=ylo3-dpy & yup4=ylo3
    
       del = max-min
       
       !P.REGION=[0.,0.,1.0,1.0]
       !P.MULTI=[0,1,4,0,0]
       !P.CHARSIZE=2.5
       !P.FONT=-1
       !X.TICKS=0
       !Y.TICKS=0
       !X.TICKlen=0.04
       !Y.TICKlen=0.02
       !X.RANGE=[min,max]
;       !Y.RANGE=[ymin,ymax]

	!P.POSITION=[xa,ylo1,xe,yup1]
        bmax=max([10.*rhor,babr]) &   print, 'B max: ',bmax
        bmin=min([10.*rhor,babr]) &   print, 'B min: ',bmin
        bmin=0.0
;        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, 10.*rhor, $
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=9,xtickname=names
	oplot, t, babr, line=1
;	ytick_get=vv

        tempmax=max(tempr) & print, 'Temp, max: ',tempmax
        temprange = 2000.   & tempscale = bmax/temprange
	axis,yaxis=1,yrange=[0,temprange],ystyle=1
	oplot, t, tempscale*tempr, line=2
        xt0=min-0.1*del  &   yt0=bmin+0.9*delb    
        yt0a=bmin+0.68*delb 
        xt1=max+0.12*del  &   yt1=bmin+0.9*delb    
        xt2=max+0.09*del  &   yt2=bmin+0.68*delb 
        yt3=bmin+0.44*delb  & yt4=bmin+0.20*delb 
        xt2a=max+0.12*del   & yt2a=bmin+0.57*delb 
        yt3a=bmin+0.33*delb & yt4a=bmin+0.09*delb
        xyouts, xt0, yt0, 'B',charsize=1.8
        xyouts, xt0, yt0a, '10 N',charsize=1.8
        xyouts, xt1, yt1,'T',charsize=1.8
        xyouts, xt2, yt2,'Dens. (cm!U-3!N)',charsize=1.5
        xyouts, xt2a, yt2a,'N ___',charsize=1.5
        xyouts, xt2, yt3, 'Magn.F. (nT)',charsize=1.5
        xyouts, xt2a, yt3a, 'B .....',charsize=1.5
        xyouts, xt2, yt4, 'Temp. (eV)',charsize=1.5
        xyouts, xt2a, yt4a, 'T _ _',charsize=1.5

	!P.POSITION=[xa,ylo2,xe,yup2]
        bmax=max([pr,pbr,ptotr]) & print, 'p max: ',bmax
        bmin=min([pr,pbr,ptotr]) & print, 'p min: ',bmin
        if (bmax-bmin) lt 0.0001 then bmax=bmin+1.0
        bmin=0.0
        delb=bmax
        bmax=bmax+0.1*delb  & delb=bmax
	plot, t, ptotr,$
	   yrange=[0,bmax], $
	   xstyle=1,ystyle=1,xtickname=names
	oplot, t, pr, line=1
	oplot, t, pbr, line=2
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
        bmax=max([vxr,vyr,vzr]) &   print, 'V max: ',bmax
        bmin=min([vxr,vyr,vzr]) &   print, 'V min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=bmax-bmin
	plot, t, vxr, yrange=[bmin,bmax], $
	   xstyle=1,ystyle=1,xtickname=names, line=2
	oplot, t, vyr, line=1
	oplot, t, vzr, line=0
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
        bmax=max([bxr,byr,bzr]) &   print, 'B max: ',bmax
        bmin=min([bxr,byr,bzr]) &   print, 'B min: ',bmin
        delb=bmax-bmin
        if bmin eq bmax then bmax=bmin+1.0
        delb=bmax-bmin
        bmax=bmax+0.05*delb & bmin=bmin-0.05*delb & delb=bmax-bmin
	plot, t, 2*bxr, yrange=[bmin,bmax], $
	   xstyle=1,ystyle=1,xtitle=xtit, line=2
	oplot, t, byr, line=1
	oplot, t, bzr, line=0
        yt1=bmin+0.73*delb   
        yt1a=bmin+0.60*delb   
        yt2=bmin+0.45*delb    
        yt3=bmin+0.3*delb    
        yt4=bmin+0.15*delb    
        xyouts, xt1, yt1,'Magn. Field',charsize=1.5
        xyouts, xt1a, yt1a,'(nT)',charsize=1.5
        xyouts, xt2, yt2,'2B!Dx!N _ _',charsize=1.5
        xyouts, xt2, yt3, 'B!Dy!N ......',charsize=1.5
        xyouts, xt2, yt4, 'B!Dz!N ___',charsize=1.5
        

  if satchoice ne '2' then begin
     xyouts, 0.05, 0.2, 'Probe index: '+string(index,'(i3)'), $
       charsize=1.2, /norm
     xyouts, 0.25, 0.2,'Rotation angle for y,z comp.:'+string(phi,'(i3)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.17, $
       'Initial probe location and vel. (simulation frame., normalized):',$
       charsize=1.2, /norm
     xyouts, 0.2, 0.145, 'x = '+string(xsat1,'(f7.1)')$
                     +'   y = '+string(ysat1,'(f7.1)')$
                     +'   z = '+string(zsat1,'(f7.1)'),$
       charsize=1.2, /norm
     xyouts, 0.2, 0.12, 'V!Dx!N = '+string(vxsat1,'(f7.3)')$
                    +'   V!Dy!N = '+string(vysat1,'(f7.3)')$
                    +'   V!Dz!N = '+string(vzsat1,'(f7.3)'),$
       charsize=1.2, /norm
       
     xyouts, 0.05, 0.09, $
       'Initial probe location and vel. (MSP frame, in km and km/s):',$
       charsize=1.2, /norm
     xyouts, 0.2, 0.065, 'x = '+string(xsat2,'(i5)')$
                     +'   y = '+string(ysat2,'(i5)')$
                     +'   z = '+string(zsat2,'(i5)'),$
       charsize=1.2, /norm
     xyouts, 0.2, 0.04, 'V!Dx!N = '+string(vxsat2,'(i4)')$
                    +'   V!Dy!N = '+string(vysat2,'(i4)')$
                    +'   V!Dz!N = '+string(vzsat2,'(i4)'),$
       charsize=1.2, /norm
     xyouts, 0.05, 0.01, $
      'DATA is plotted in the probe rest frame in rotated (MSP) coord.',$
       charsize=1.2, /norm
  endif
  if satchoice eq '2' then begin
    xyouts, 0.05, 0.2, 'Data represents a cut throuth system for const y.', $
       charsize=1.2, /norm
    xyouts, 0.05, 0.17, 'Location is in simulation units and '$
                         +'velocities are in magnetospheric frame', $
       charsize=1.2, /norm 
    xyouts, 0.05, 0.14, strnn, charsize=1.2, /norm
  endif
        
return
end

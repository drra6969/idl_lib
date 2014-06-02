PRO ctfvec, f2,fv1,fv2,pos1,xpos,ypos,nl1,nl2,names, $
             titl,xtitl,ytitl,glatt,aval,xt,yt

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

;--- velocities on either side and average velocities---

        fb=f2 & fn1=fv1 & fn2=fv2
        if glatt eq 'y' then  fb=smooth(fb,3)
        fb=interpolate(fb,ioxf,ioyf,/grid)
        fn1=interpolate(fv1,iox,ioy,/grid)
        fn2=interpolate(fv2,iox,ioy,/grid)
	
;       plot vz,magnetic field, and vel vectors       
        !P.POSITION=pos1

        thickarr=fltarr(nl1) & thickarr(*)=1.0 & thickarr(nl1-1)=4.0
        if glatt eq 'y' then  fb=smooth(fb,3)
        bmax=max(fb) & bmin=min(fb) & del=(bmax-bmin)/(nl1-1.)
        bav=0.5*(bmax+bmin) 
        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(nl1)*del+bmin+del & lvec(nl1-1)=aval
        indsort=sort(lvec)
        lvec=lvec(indsort) & thickarr=thickarr(indsort)
        contour,fb,xf,yf,levels=lvec,$
          c_linestyle=0, c_thick=thickarr,$ 
          xstyle=1, ystyle=1,$
          xtitle=xtitl, thick=1.0
        oplot,xt,yt,psym=2

        fmax=sqrt(max(fn1^2+fn2^2))
        vect, fn1, fn2, xn, yn, length=1.0,$
        title=titl,/noerase
;	xyouts,charsize=1.1,xpos(2),ypos(8),'Max='+string(fmax,'(f4.2)')
;	xyouts,charsize=0.8,xpos(0),ypos(4),' '+string(fmax,'(f5.2)')
;	xyouts,charsize=0.8,xpos(0),ypos(0),'time' 
;	xyouts,charsize=0.8,xpos(0),ypos(1),' '+string(time,'(i3)') 
	xyouts,charsize=0.8,xpos(0),ypos(2),run 
	xyouts,charsize=1.0,xpos(1),ypos(7),ytitl
;	xyouts,charsize=1.0,xpos(2),ypos(9),'time = '+string(time,'(i3)') 



  
return
end


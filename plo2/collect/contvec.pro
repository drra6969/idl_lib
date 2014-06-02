PRO contvec, fb,fn1,fn2,pos1,xpos,ypos,nl1,nl2,names, $
             titl,xtitl,ytitl,glatt

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

;--- velocities on either side and average velocities---

        if glatt eq 'y' then  fb=smooth(fb,3)
	
;       plot vz,magnetic field, and vel vectors       
	!P.POSITION=pos1
        	
    if glatt eq 'y' then  fb=smooth(fb,3)
	bmax=max(fb) & bmin=min(fb) & del=(bmax-bmin)/(nl1-1.)
        bav=0.5*(bmax+bmin) 
;        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(nl1)*del+bmin & lvec(nl1-1)=bav-1.2*del
        lvec=lvec(sort(lvec))
        contour,fb,xf,yf,levels=lvec,$
        c_linestyle=0,$ 
        xstyle=1,ystyle=1,$
        xtitle=xtitl,thick=1.0

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


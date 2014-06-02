PRO covec, pos1,xpos,ypos,nl1,nl2,names, $
             titl,xtitl,ytitl,smo,plane

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
COMMON program_var, fp1,fp2,xsu,ysu,f0,xchoice,ychoice,far1,far2,xar,yar,$
                    cutata,cutatb

;--- velocities on either side and average velocities---
	
;       plot vz,magnetic field, and vel vectors       
	!P.POSITION=pos1
        	
	bmax=max(fp2) & bmin=min(fp2) & del=(bmax-bmin)/(nl1-1.)
        if del lt 0.000001 then del=0.1
        bav=0.5*(bmax+bmin) 
;        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(nl1)*del+bmin & lvec(nl1-1)=bav-1.2*del
        lvec=lvec(sort(lvec))
        contour,fp2,xsu,ysu,levels=lvec,$
        c_linestyle=0,$ 
        xstyle=1,ystyle=1,$
        xtitle=xtitl,thick=1.0

        fmax=sqrt(max(far1^2+far2^2))
        vect3, far1, far2, xar, yar, length=1.0,$
        title=titl,/noerase

	xyouts,charsize=0.8,xpos(0),ypos(1),cutata 
	xyouts,charsize=0.8,xpos(0),ypos(2),cutatb       
	xyouts,charsize=1.0,xpos(1),ypos(7),ytitl
	xyouts,charsize=1.0,xpos(2),ypos(8),'time = '+string(time,'(i3)') 

;    xyouts,charsize=0.8,xpos(0),ypos(2),run 


  
return
end


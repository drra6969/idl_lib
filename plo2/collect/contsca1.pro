PRO contsca1, fa,pos1,xpos,ypos,nl1,nl2,names,titl,xtitl,ytitl,glatt


COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

;--- velocities on either side and average velocities---

        if glatt eq 'y' then  fa=smooth(fa,3)
	fmax=max(fa) & fmin=min(fa)

	!P.POSITION=pos1
	bmax=max(fa) & bmin=min(fa) &  bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/(nl2-1.)
        if (bmax*bmin ge 0.) or (abs(bmin) lt 0.2*abs(bmax)) then $
          cb=bav else cb=0.
	contour,fa,xf,yf,levels=findgen(nl2)*del+bmin,$
        c_linestyle=findgen(nl2)*del+bmin lt cb,$
        title=titl,xstyle=1,ystyle=1, xtitle=xtitl
	xyouts,charsize=0.8,xpos(0),ypos(3),'Max' 
	xyouts,charsize=0.8,xpos(0),ypos(4),''+string(bmax,'(f5.2)') 
	xyouts,charsize=0.8,xpos(0),ypos(5),'Min' 
	xyouts,charsize=0.8,xpos(0),ypos(6),''+string(bmin,'(f5.2)') 
;	xyouts,charsize=0.8,xpos(0),ypos(0),'time' 
;	xyouts,charsize=0.8,xpos(0),ypos(1),' '+string(time,'(i3)') 
	xyouts,charsize=0.8,xpos(0),ypos(2),run 
	xyouts,charsize=1.0,xpos(1),ypos(7), ytitl

  
return
end


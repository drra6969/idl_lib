PRO cont2, pos1,xpos,ypos,nl1,nl2,names,titl,xtitl,ytitl,smo,plane

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
COMMON program_var, fp1,fp2,xsu,ysu,f0,xchoice,ychoice,far1,far2,xar,yar,$
                    cutata,cutatb

;--- velocities on either side and average velocities---

	fmax=max(fp2) & fmin=min(fp2)

;       plot vzproj_msp     
	!P.POSITION=pos1
	bmax=max(fp2) & bmin=min(fp2) &  bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/(nl1-1.) 
        if del lt 0.000001 then del=0.1
        if (bmax*bmin ge 0.) or (abs(bmin) lt 0.2*abs(bmax)) then $
          cb=bav else cb=0.
	contour,fp2,xsu,ysu,levels=findgen(nl1)*del+bmin,$
        c_linestyle=0,$
;        c_linestyle=findgen(nl1)*del+bmin lt cb,$
        title=titl,xstyle=1,ystyle=1,xtitle=xtitl
	xyouts,charsize=0.8,xpos(0),ypos(1),cutata 
	xyouts,charsize=0.8,xpos(0),ypos(2),cutatb       
	xyouts,charsize=0.8,xpos(0),ypos(3),'Max' 
	xyouts,charsize=0.8,xpos(0),ypos(4),''+string(bmax,'(f5.2)') 
	xyouts,charsize=0.8,xpos(0),ypos(5),'Min' 
	xyouts,charsize=0.8,xpos(0),ypos(6),''+string(bmin,'(f5.2)') 
	xyouts,charsize=1.1,xpos(0),ypos(8),'time = '+string(time,'(i3)') 
	xyouts,charsize=1.0,xpos(1),ypos(7), ytitl

;	xyouts,charsize=0.8,xpos(0),ypos(2), run 


return
end


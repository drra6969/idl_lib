PRO contplot, f,xa,xe,ylo,yup,format,xtit,tit,rtime,dl,m
COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, $
                    xn,yn, iox,ioy,ioxf,ioyf, run, time, $
                    xpos,ypos0,ypos1,ypos2,ypos3,ypos4, $
                    ypos5,ypos6,xpmin,xpmax,ymin,ymax

; Subroutine to make contour plot
; 

       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ymin,ymax]

        fa=interpolate(f,ioxf,ioyf,/grid)
	bmax=max(fa,indmax) & bmin=min(fa,indmin)
	bmax0=bmax
        if (bmax-bmin) lt 0.00001 then bmax=bmin+0.5
        del=(bmax-bmin)/dl
        lvec=findgen(dl+1)*del + bmin
        lvec=lvec(sort(lvec))
        bdiv=0.5*(bmax0+bmin)
        if ( (bmax gt 0.0) and (bmin lt 0.0) ) then bdiv=0.0
        ytit='z'

	!P.POSITION=[xa,ylo,xe,yup]
        contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt bdiv,$
        title=tit,xstyle=1,ystyle=1,$
        xtitle='x (km)',ytitle=ytit
	xyouts,charsize=1,xpos,ypos0,'time'
	xyouts,charsize=1,xpos,ypos1,' '+string(rtime,'(f6.2)')+' s'
	xyouts,charsize=1,xpos,ypos2,run
	if (m eq 1) then begin
	  xyouts,charsize=1,xpos,ypos3,'Max='
 	  xyouts,charsize=1,xpos,ypos4,' '+string(bmax0,format)+xtit
	  xyouts,charsize=1,xpos,ypos5,'Min='
	  xyouts,charsize=1,xpos,ypos6,' '+string(bmin,format)+xtit
	endif
return
end

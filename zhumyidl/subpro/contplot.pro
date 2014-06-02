PRO contplot, f,xa,xe,ylo,yup,unit,name,rtime

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, $
                    xn,yn, iox,ioy,ioxf,ioyf, run, time, $
                    xpos,ypos0,ypos1,ypos2,ypos3,ypos4, $
                    ypos5,ypos6,xpmin,xpmax,ymin,ymax

; Subroutine to make contour plot
;   z for plotting data from MP simulations
;      PLOT        SIMULATION
;     -------       -------
;  z !       !    y!       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    !       !     !       !
;    <-------       ------->
;      x                  x

       !X.RANGE=[xpmin,xpmax]
       !Y.RANGE=[ymin,ymax]

        fa=interpolate(-f,ioxf,ioyf,/grid)
	bmax=max(fa,indmax) & bmin=min(fa,indmin)  
        if (bmax-bmin) lt 0.00001 then bmax=bmin+0.5
        del=(bmax-bmin)/16.
        lvec=findgen(17)*del + bmin 
        lvec=lvec(sort(lvec))

	!P.POSITION=[xa,ylo,xe,yup]
	contour,fa,xf,yf,levels=lvec,$
        c_linestyle=lvec lt 0.0,$ 
        title=name,xstyle=1,ystyle=1,$
        xtitle='x (km)',ytitle=ytit
	xyouts,xpos,ypos0,'time'
	xyouts,xpos,ypos1,' '+string(rtime,'(f6.2)')+' s' 
	xyouts,xpos,ypos2,run
	xyouts,xpos,ypos3,'Max='
	xyouts,xpos,ypos4,' '+string(bmax,'(E9.1)')+unit
	xyouts,xpos,ypos5,'Min='
	xyouts,xpos,ypos6,' '+string(bmin,'(E9.1)')+unit
return
end


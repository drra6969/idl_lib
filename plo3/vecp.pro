PRO vecp, pos1,pos2,xpos,ypos,nl1,nl2,names, $
             titl,xtitl,ytitl,smo,plane

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
COMMON program_var, fp1,fp2,xsu,ysu,f0,xchoice,ychoice,far1,far2,xar,yar,$
                    cutata,cutatb

;--- velocities on either side and average velocities---

	bmax=max(fp2) & bmin=min(fp2)
        chcolorscale, titl, bmin,bmax, fmin,fmax
        ytf='(e8.1)'
        if ((bmax-bmin) gt 0.01) then ytf='(f6.3)'
        if ((bmax-bmin) gt 1.) then ytf='(f6.2)'
        if ((bmax-bmin) gt 100.) then ytf='(f6.0)'
        if ((bmax-bmin) gt 10000.) then ytf='(e8.1)'

;       colorbar        
	!P.POSITION=pos2
	ddel=(bmax-bmin) & if ddel lt 0.000001 then ddel=1.0
        del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+bmin  & cbary=findgen(2,nxf)
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        image_c3, cbary,min_f=fmin,max_f=fmax
	contour,cbary,[0,1],ctab,levels=findgen(nl1)*ddel/(nl1-1.)+bmin,$
	c_linestyle=1,xstyle=1,ystyle=1,$
        xrange=[0,1],yrange=[bmin,bmax],$
        xtickname=names,xticks=1,ytickformat=ytf,/noerase
        
;       plot vz,magnetic field, and vel vectors       
	!P.POSITION=pos1
        if plane eq 'y' then $
           image_c3,rotate(fp2,5),min_f=fmin,max_f=fmax $
           else image_c3,fp2,min_f=fmin,max_f=fmax

	bmax=max(f0) & bmin=min(f0) & del=(bmax-bmin)/(nl2-1.)
        if del lt 0.000001 then del=0.1
        bav=0.5*(bmax+bmin) 
        print, 'vecpot:', bmax, bmin, bav
        lvec=findgen(nl2)*del+bmin 
        ; lvec(nl2-1)=bav-1.2*del
        lvec=lvec(sort(lvec))
        contour,f0,xchoice,ychoice,levels=lvec,$
        c_linestyle=1,$ 
        xstyle=1,ystyle=1,$
        xtitle=xtitl,thick=1.0,/noerase

        fmax=sqrt(max(far1^2+far2^2))
        vect3, far1, far2, xar, yar, length=1.1,$
        title=titl,/noerase

	xyouts,charsize=0.8,xpos(0),ypos(1),cutata 
	xyouts,charsize=0.8,xpos(0),ypos(2),cutatb       
	xyouts,charsize=1.0,xpos(1),ypos(7),ytitl
	xyouts,charsize=1.1,xpos(2),ypos(8),'time = '+string(time,'(i3)') 

;    xyouts,charsize=0.8,xpos(0),ypos(2),run 
  
return
end


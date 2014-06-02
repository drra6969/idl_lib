PRO sca1a, pos1,pos2,xpos,ypos,nl1,nl2,names,titl,xtitl,ytitl,smo,plane


COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
COMMON program_var, fp1,fp2,xsu,ysu,f0,xchoice,ychoice,far1,far2,xar,yar,$
                    cutata,cutatb


	bmax=max(fp1) & bmin=min(fp1) &  bav=0.5*(bmax+bmin)
        del=(bmax-bmin)/(nl1-1.)

        chcolorscale, titl, bmin,bmax, fmin,fmax

	!P.POSITION=pos1
        if plane eq 'y' then $
          image_c3,rotate(fp1,5),min_f=fmin,max_f=fmax $ 
          else image_c3,fp1,min_f=fmin,max_f=fmax

        if del lt 0.000001 then del=0.1
        if (bmax*bmin ge 0.) or (abs(bmin) lt 0.2*abs(bmax)) then $
          cb=bav else cb=0.
	contour,fp1,xsu,ysu,levels=findgen(nl1)*del+bmin,$
;        c_linestyle=findgen(nl1)*del+bmin lt cb,$
        c_linestyle=1,$
        title=titl,xstyle=1,ystyle=1,$
        xtitle=xtitl, /noerase

	xyouts,charsize=0.8,xpos(0),ypos(1),cutata 
	xyouts,charsize=0.8,xpos(0),ypos(2),cutatb       
	xyouts,charsize=1.0,xpos(1),ypos(7), ytitl
	xyouts,charsize=1.0,xpos(2),ypos(8),'time = '+string(time,'(i3)') 

        ytf='(e8.1)'
        if ((bmax-bmin) gt 0.01) then ytf='(f6.3)'
        if ((bmax-bmin) gt 1.) then ytf='(f6.2)'
        if ((bmax-bmin) gt 100.) then ytf='(f6.0)'
        if ((bmax-bmin) gt 10000.) then ytf='(e8.1)'
;       colorbar        
	!P.POSITION=pos2
	ddel=(bmax-bmin) & if ddel lt 0.000001 then ddel=1.0 
        del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+bmin & cbary=findgen(2,nxf)
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        image_c3, cbary,min_f=fmin,max_f=fmax
	contour,cbary,[0,1],ctab,levels=findgen(nl1)*ddel/(nl1-1.)+bmin,$
	xstyle=1,ystyle=1,c_linestyle=1,xrange=[0,1],yrange=[bmin,bmax],$
        xtickname=names,xticks=1,ytickformat=ytf,/noerase

  
return
end


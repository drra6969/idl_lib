PRO scap1, fa,f1min,f1max,pos1,pos2,xpos,ypos,nl1,nl2,names,$
               titl,xtitl,ytitl,glatt,aval,addcont


COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

;--- velocities on either side and average velocities---

        if glatt eq 'y' then  fa=smooth(fa,3)
        if (f1min ne 0. or f1max ne 0.) then begin
          ind=where(fa lt f1min,count) & if count ne 0 then fa(ind)=f1min
          ind=where(fa gt f1max,count) & if count ne 0 then fa(ind)=f1max
        endif
	fmax=max(fa) & fmin=min(fa)
        if ( (fmax-fmin) lt 0.000001) then begin
          fmax=fmax+0.00001 & fmin=fmin & endif

        if !x.range(1) gt !x.range(0) then nrot=0
        if !x.range(1) lt !x.range(0) then nrot=5

	!P.POSITION=pos1
        IMAGE_C, rotate(fa,nrot)
	bmax=max(fa) & bmin=min(fa) &  bav=0.5*(bmax+bmin)
        if ( (bmax-bmin) lt 0.00001) then begin
          bmax=bmax+0.00001 & bmin=bmin & endif
        del=(bmax-bmin)/(nl2-1.)
        if (bmax*bmin ge 0.) or (abs(bmin) lt 0.2*abs(bmax)) then $
          cb=bav else cb=0.

        lvec=findgen(nl2)*del+bmin
        thickarr=fltarr(nl2) & thickarr(*)=1.0
        lstylearr=intarr(nl2) & lstylearr(*)=1
        if addcont ne 0 and bmin*bmax lt 0 then begin
           lvec=findgen(nl2)*del+bmin+del & lvec(nl2-1)=aval
           thickarr(nl2-1)=addcont & lstylearr(nl2-1)=0
        indsort=sort(lvec)
        lvec=lvec(indsort) & thickarr=thickarr(indsort)
        lstylearr=lstylearr(indsort)
       endif

	contour,fa,xf,yf,levels=lvec,$
          c_linestyle=lstylearr, c_thick=thickarr,$
          title=titl,xstyle=1,ystyle=1,$
          xtitle=xtitl, /noerase
;	xyouts,charsize=0.8,xpos(0),ypos(3),'Max' 
;	xyouts,charsize=0.8,xpos(0),ypos(4),''+string(bmax,'(f5.2)') 
;	xyouts,charsize=0.8,xpos(0),ypos(5),'Min' 
;	xyouts,charsize=0.8,xpos(0),ypos(6),''+string(bmin,'(f5.2)') 
;	xyouts,charsize=0.8,xpos(0),ypos(0),'time' 
;	xyouts,charsize=0.8,xpos(0),ypos(1),' '+string(time,'(i3)') 
;	xyouts,charsize=0.8,xpos(0),ypos(2),run 
	xyouts,charsize=1.0,xpos(1),ypos(7), ytitl

;       colorbar        
	!P.POSITION=pos2
	fmax=bmax & fmin=bmin
        ytf='(e8.1)'
        if ((fmax-fmin) gt 0.01) then ytf='(f6.3)'
        if ((fmax-fmin) gt 1.) then ytf='(f6.2)'
        if ((fmax-fmin) gt 100.) then ytf='(f6.0)'
        if ((fmax-fmin) gt 10000.) then ytf='(e8.1)'
        if ( (fmax-fmin) lt 0.00001) then begin
          fmax=fmax+0.00001 & fmin=fmin & endif
	ddel=(fmax-fmin) & del=ddel/float(nxf-1)
	ctab=findgen(nxf) & ctab=del*ctab+fmin & cbary=findgen(2,nxf)
	cbary(0,*)=ctab(*) & cbary(1,*)=ctab(*)
        IMAGE_C, cbary
	contour,cbary,[0,1],ctab,levels=findgen(nl1)*ddel/(nl1-1.)+fmin,$
	xstyle=1,ystyle=1,c_linestyle=1,xrange=[0,1],yrange=[fmin,fmax],$
        xtickname=names,xticks=1,ytickformat=ytf,/noerase

  
return
end


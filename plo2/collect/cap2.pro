PRO cap2, fa,f2min,f2max,pos1,pos2,xpos,ypos,nl1,nl2,names,$
               titl,xtitl,ytitl,glatt,aval,addcont

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, x,y, xf,yf, xn,yn, iox,ioy, $
                    ioxf,ioyf, run, time

;--- velocities on either side and average velocities---

        if glatt eq 'y' then  fa=smooth(fa,3)
        if (f2min ne 0. or f2max ne 0.) then begin
          ind=where(fa lt f2min, count) & if count ne 0 then fa(ind)=f2min
          ind=where(fa gt f2max, count) & if count ne 0 then fa(ind)=f2max
        endif
	fmax=max(fa) & fmin=min(fa)
        if ( (fmax-fmin) lt 0.000001) then begin
          fmax=fmax+0.05 & fmin=fmin-0.05 & endif

	!P.POSITION=pos1

	bmax=max(fa) & bmin=min(fa) &  bav=0.5*(bmax+bmin)
        if ( (bmax-bmin) lt 0.000001) then begin
          bmax=bmax+0.0000005 & bmin=bmin-0.0000005 & endif
        del=(bmax-bmin)/(nl2-1.)
        if (bmax*bmin ge 0.) or (abs(bmin) lt 0.2*abs(bmax)) then $
          cb=bav else cb=0.

        lvec=findgen(nl2)*del+bmin
        thickarr=fltarr(nl2) & thickarr(*)=1.0
        lstylearr=intarr(nl2) & lstylearr(*)=1
        lcolarr=intarr(nl2) & lcolarr(*)=0
        if addcont ne 0 and bmin*bmax lt 0 then begin
           lvec=findgen(nl2)*del+bmin+del & lvec(nl2-1)=aval
           thickarr(nl2-1)=addcont & lstylearr(nl2-1)=0 & lcolarr(nl2-1)=0
        indsort=sort(lvec)
        lvec=lvec(indsort) & thickarr=thickarr(indsort)
        lstylearr=lstylearr(indsort) & lcolarr=lcolarr(indsort)
    endif
    print,'lcol:',lcolarr

	contour,fa,xf,yf,levels=lvec,$
          c_linestyle=lstylearr, c_thick=thickarr, c_colors=lcolarr,$
          xstyle=1,ystyle=1,/noerase
  
return
end


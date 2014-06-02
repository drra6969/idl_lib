PRO PLOBVEQS1, nt, t, ntb, tb, tv1,tv2,b,v,e,e1,e2,e3,ew,ind,strnn,xtit,vht
; plots of b, v, and e in coordinates of e1, e2, and e3

  names=strarr(15) & names=replicate(' ',15)
    
  pos1=[0.1, 0.6, .65, .85]
  pos2=[0.1, 0.35, .65, .6]
  pos3=[0.1, 0.1, .65, .35]
  tmin=tb(0)  & tmax=tb(ntb-1)
  
  max = tmax &  min=tmin
  if (max-min) gt 1.5 then tax = 'h' else tax = 'm'
  if 60.*(max-min) le 7. then tax = 'ms'
  if 60.*(max-min) le 1.5 then tax = 's'
  case tax of
    'h': begin tplb = tb   & tplv = t    & xminor = 6 
               tvv1 = tv1  & tvv2 = tv2  & end
    'm': begin min = 60.*min  & max = 60.*max
               tplb = 60.*tb  & tplv = 60.*t    & xminor = 0 
               tvv1 = 60.*tv1 & tvv2 = 60.*tv2  & end
    'ms': begin min = 60.*min  & max = 60.*max
                tplb = 60.*tb  & tplv = 60.*t    & xminor = 6 
                tvv1 = 60.*tv1 & tvv2 = 60.*tv2  & end
    's': begin min = 3600.*min  & max = 3600.*max
               tplb = 3600.*tb  & tplv = 3600.*t    & xminor = 0 
               tvv1 = 3600.*tv1 & tvv2 = 3600.*tv2  & end
  endcase

  
  jvec='j!d'+ind+'!n'   & xtit1=jvec
  kvec='k!d'+ind+'!n'   & xtit2=kvec
  ivec='i!d'+ind+'!n'   & ytit =ivec
  titl='!17 B, V, and E in Variance of !18'+ind+'!17 Coordinates!3'
  !P.FONT=-1

  bt=b 
  bt(*,0)=b(*,0)*e1(0)+b(*,1)*e1(1)+b(*,2)*e1(2)
  bt(*,1)=b(*,0)*e2(0)+b(*,1)*e2(1)+b(*,2)*e2(2)
  bt(*,2)=b(*,0)*e3(0)+b(*,1)*e3(1)+b(*,2)*e3(2)
  vt=v 
  vt(*,0)=v(*,0)*e1(0)+v(*,1)*e1(1)+v(*,2)*e1(2)
  vt(*,1)=v(*,0)*e2(0)+v(*,1)*e2(1)+v(*,2)*e2(2)
  vt(*,2)=v(*,0)*e3(0)+v(*,1)*e3(1)+v(*,2)*e3(2)
  et=e 
  et(*,0)=e(*,0)*e1(0)+e(*,1)*e1(1)+e(*,2)*e1(2)
  et(*,1)=e(*,0)*e2(0)+e(*,1)*e2(1)+e(*,2)*e2(2)
  et(*,2)=e(*,0)*e3(0)+e(*,1)*e3(1)+e(*,2)*e3(2)
  vhtt=vht
  vhtt(0)=total(vht(*)*e1(*))
  vhtt(1)=total(vht(*)*e2(*))
  vhtt(2)=total(vht(*)*e3(*))

  bmin=min(bt) & bmax=max(bt) & del=bmax-bmin 
  bmax=bmax+0.05*del  &  bmin=bmin-0.05*del
  vmin=min(vt) & vmax=max(vt) & del=vmax-vmin 
  vmax=vmax+0.05*del  &  vmin=vmin-0.05*del
  emin=min(et) & emax=max(et) & del=emax-emin 
  emax=emax+0.05*del  &  emin=emin-0.05*del

  !P.CHARSIZE=2.0
  !P.FONT=-1
  !P.CHARTHICK=2.
  plot, tplb(*), bt(*,0), $
    xrange=[min,max], yrange=[bmin, bmax],   xtitle=' ', ytitle='B', $
    xminor=xminor, $
    xstyle=1, ystyle=1,xtickname=names, pos=pos1
  oplot, tplb(*), bt(*,1), line=1
  oplot, tplb(*), bt(*,2), line=2
  if (tv1 gt tmin) and (tv1 lt tmax) then $
         oplot, [tvv1,tvv1], [bmin,bmax], line=1
  if (tv2 gt tmin) and (tv2 lt tmax) then $
         oplot, [tvv2,tvv2], [bmin,bmax], line=1

  plot, tplv(*), vt(*,0), $
    xrange=[min,max], yrange=[vmin, vmax],   xtitle=' ', ytitle='V', $
    xminor=xminor, $
    xstyle=1, ystyle=1,xtickname=names, pos=pos2
  oplot, tplv(*), vt(*,1), line=1
  oplot, tplv(*), vt(*,2), line=2
  if (tv1 gt tmin) and (tv1 lt tmax) then $
         oplot, [tvv1,tvv1], [vmin,vmax], line=1
  if (tv1 ge tmin) and (tv1 le tmax) then begin
    xyouts, tvv1,vhtt(0), 'i', alignment=0.5
    xyouts, tvv1,vhtt(1), 'j', alignment=0.5
    xyouts, tvv1,vhtt(2), 'k', alignment=0.5
  endif
  if (tv2 gt tmin) and (tv2 lt tmax) then $
         oplot, [tvv2,tvv2], [vmin,vmax], line=1
  

  plot, tplv(*), et(*,0), $
    xrange=[min,max], yrange=[emin, emax],   xtitle=xtit, ytitle='E', $
    xtickformat='XTICKS', xminor=xminor, $
    xstyle=1, ystyle=1, pos=pos3
  oplot, tplv(*), et(*,1), line=1
  oplot, tplv(*), et(*,2), line=2
  if (tv1 gt tmin) and (tv1 lt tmax) then $
         oplot, [tvv1,tvv1], [emin,emax], line=1
  if (tv2 gt tmin) and (tv2 lt tmax) then $
         oplot, [tvv2,tvv2], [emin,emax], line=1


  !P.FONT=0
  !P.CHARSIZE=1.1
  !P.CHARTHICK=1.
  
  xyouts, .36, .98, '!17Interval '+$
      cvhms(tmin)+' - '+cvhms(tmax)+'!3', $
      /norm, alignment=0.5
  xyouts, .65, .97, strnn, /norm
  xyouts, .65, .94, 'No. of Data points:'+string(nt,'(i4)'), /norm
  xyouts, .65, .91, '!17Variance Interval '+$
     cvhms(tv1)+' - '+cvhms(tv2)+'!3', /norm

  xyouts, .36, 0.93, titl, charsize=1.4,charthick=2.0,/norm, alignment=0.5
  
  xyouts, .70, .85, 'Eigenvectors:', /norm
  xyouts, .70, .8, ivec+' = '+string(e1(0),'(f7.4)'), /norm
  xyouts, .70, .77, '     '+string(e1(1),'(f7.4)'), /norm
  xyouts, .70, .74, '     '+string(e1(2),'(f7.4)'), /norm
  xyouts, .70, .7, jvec+' = '+string(e2(0),'(f7.4)'), /norm
  xyouts, .70, .67, '     '+string(e2(1),'(f7.4)'), /norm
  xyouts, .70, .64, '     '+string(e2(2),'(f7.4)'), /norm
  xyouts, .70, .6, kvec+' = '+string(e3(0),'(f7.4)'), /norm
  xyouts, .70, .57, '     '+string(e3(1),'(f7.4)'), /norm
  xyouts, .70, .54, '     '+string(e3(2),'(f7.4)'), /norm
  xyouts, .85, .85, 'Eigenvalue Ratios:', /norm
  if abs(ew(0)/ew(1)) lt 9999.9 then $
    xyouts, .85, .81, 'Max/Int: '+string((ew(0)/ew(1)),'(f7.2)'), /norm $
  else    xyouts, .87, .31, 'Max/Int: > 10000', /norm
  if abs(ew(1)/ew(2)) lt 9999.9 then $
    xyouts, .85, .78, 'Int/Min: '+string((ew(1)/ew(2)),'(f7.2)'), /norm $
  else    xyouts, .87, .78, 'Int/Min: > 10000', /norm
  
  iphix=180./!pi*acos(abs(e1(0))) & jphix=180./!pi*acos(abs(e2(0)))
  kphix=180./!pi*acos(abs(e3(0))) 
  iphiy=180./!pi*acos(abs(e1(1))) & jphiy=180./!pi*acos(abs(e2(1)))
  kphiy=180./!pi*acos(abs(e3(1))) 
  iphiz=180./!pi*acos(abs(e1(2))) & jphiz=180./!pi*acos(abs(e2(2)))
  kphiz=180./!pi*acos(abs(e3(2))) 
  xyouts, .70, .49,'Angles of Eigenvectors', /norm
  xyouts, .81, .46,'i', /norm, alignment=0.5
  xyouts, .87, .46,'j', /norm, alignment=0.5
  xyouts, .93, .46,'k', /norm, alignment=0.5
  xyouts, .70, .43,'with X:', /norm
  xyouts, .81, .43,string(iphix,'(f4.1)'), /norm, alignment=0.5
  xyouts, .87, .43,string(jphix,'(f4.1)'), /norm, alignment=0.5
  xyouts, .93, .43,string(kphix,'(f4.1)'), /norm, alignment=0.5
  xyouts, .70, .40,'with Y:', /norm
  xyouts, .81, .40,string(iphiy,'(f4.1)'), /norm, alignment=0.5
  xyouts, .87, .40,string(jphiy,'(f4.1)'), /norm, alignment=0.5
  xyouts, .93, .40,string(kphiy,'(f4.1)'), /norm, alignment=0.5
  xyouts, .70, .37,'with Z:', /norm
  xyouts, .81, .37,string(iphiz,'(f4.1)'), /norm, alignment=0.5
  xyouts, .87, .37,string(jphiz,'(f4.1)'), /norm, alignment=0.5
  xyouts, .93, .37,string(kphiz,'(f4.1)'), /norm, alignment=0.5

  xyouts, .70, .29, 'For !8B, V !3and !8E!3:', /norm
  xyouts, .70, .25, '____ = i component', /norm
  xyouts, .70, .22, '........ = j component', /norm
  xyouts, .70, .19, '------ = k component', /norm

  !P.CHARSIZE=1.
  !P.FONT=-1
  !P.CHARTHICK=1.
    
return
end

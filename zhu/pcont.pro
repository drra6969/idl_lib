PRO pcont, fa,xsu,ysu,contin,pos,head,xtit,ytit,cutata,cutatb,time,run, $
           xpos,yposa,yposb,ypos1,ypos2,ypos3,ypos4,ypos5,ypos6

    if contin eq 's' then fp=smooth(fa,3) else fp=fa 
    fpmax=max(fp) & fpmin=min(fp)
    formax='(f6.3)' & 
    if abs(fpmax) gt 10. then formax='(f6.1)'
    if abs(fpmax) gt 100. then formax='(f6.0)'
    formin='(f6.3)'
    if abs(fpmin) gt 10. then formin='(f6.1)'
    if abs(fpmin) gt 100. then formin='(f6.0)'
    if (fpmax*fpmin/(fpmax^2+fpmin^2) lt -0.2) then fptr=0.0 $
                                              else fptr=0.4*(fpmax+fpmin)
    delf=(fpmax-fpmin)/12. & if delf lt 0.0000001 then delf=0.0001
    !P.POSITION=pos
    contour,fp,xsu,ysu,levels=findgen(13)*delf+fpmin, $
;    c_linestyle=findgen(13)*delf+fpmin lt fptr, $
    c_linestyle=findgen(13)*delf+fpmin gt fptr, $
    title=head,xstyle=1,ystyle=1, $
    xtitle=xtit,ytitle=ytit 
    xyouts,xpos,yposa,cutata 
    xyouts,xpos,yposb,cutatb 
    xyouts,xpos,ypos1,'t='+string(time,'(i3)') 
    xyouts,xpos,ypos2,' '+run 
    xyouts,xpos,ypos3,'Max=' 
    xyouts,xpos,ypos4,' '+string(fpmax,formax) 
    xyouts,xpos,ypos5,'Min=' 
    xyouts,xpos,ypos6,' '+string(fpmin,formin) 

return
end


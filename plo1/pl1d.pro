PRO pl1d, farr,nx,x,itot,amin,amax,time,delt,svalue,rms,pos,xtit

       !P.THICK=1.      
       !P.POSITION=pos
       plot, x, farr(*,0),$
;          title='Temperature',/noerase,$
          title='Temperature',$
          xtitle=xtit, charsize=2.,xstyle=1, ystyle=1, line=0
      for i=1,itot-1 do  oplot, x, farr(*,i), line=3
      !P.THICK=2.      
      oplot, x, farr(*,itot), line=0
        xpos=1.02*x(nx-1)
        ypos=amin+0.6*(amax-amin)
        ypos1=amin+0.7*(amax-amin)
        ypos2=amin+0.6*(amax-amin)
        ypos3=amin+0.5*(amax-amin)
        ypos4=amin+0.4*(amax-amin)
        ypos5=amin+0.3*(amax-amin)
	xyouts,xpos,ypos1,'Time ='+string(time,'(f9.0)')
	xyouts,xpos,ypos2,'Nx ='+string(nx,'(i3)')
	xyouts,xpos,ypos3,'Dt ='+string(delt,'(f7.1)')
	xyouts,xpos,ypos4,'s ='+string(svalue,'(f6.3)')
	xyouts,xpos,ypos5,'RMS E ='+string(rms,'(f8.5)')

end


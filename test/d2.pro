; START OF MAIN PROGRAM

  nx=81 & pi=3.14159265358979 & withps='n' 
  xmin =  0 
  xmax = 1.0

  x=findgen(nx) 
  dx=(xmax-xmin)/float(nx-1)
  x=x*dx+xmin
  f=findgen(nx) & fd2=f & fd4=f & fd5=f & rhs=f & lhs=f & fd2num=f
  f=sin(pi*x)
  fd2=-pi*pi*sin(pi*x)
  fd2(0)=0.00001 & fd2(nx-1)=0.00001
  fd4=-pi*pi*fd2
  fd2num=( shift(f,-1)+shift(f,1)-2*f )/dx/dx
  fd2num(0)=0.00001 & fd2num(nx-1)=0.00001
  lhs=-1.0+fd2num/fd2
  rhs=1.0/12.0*fd4*dx*dx/fd2
  lhs(0)=0.0 & lhs(nx-1)=0.0
  rhs(0)=0.0 & rhs(nx-1)=0.0

      print, 'With postscript?'
      read, withps
       if withps eq 'y' then begin 
        set_plot,'ps'
        device,filename='dia.ps'
        device,/inches,xsize=8.,scale_factor=1.0,xoffset=0.5
        device,/inches,ysize=10.0,scale_factor=1.0,yoffset=0.5
        device,/times,/bold,font_index=3
       endif
    !P.REGION=[0.,0.,1.0,1.25]
    !P.MULTI=[0,0,2]
    !P.POSITION=[0.15,0.1,0.7,0.45]
    amax = [[max(rhs)], [max(lhs)]]
    amin = [[min(rhs)], [min(lhs)]]
    plot, x, lhs, title='rel error',$
        xtitle='x', font=3, yrange=[min(amin), max(amax)]
    oplot, x, rhs, line=1

    !P.POSITION=[0.15,0.55,0.7,0.9]
    amax = [[max(fd2)], [max(fd2num)]]
    amin = [[min(fd2)], [min(fd2num)]]
    plot, x, fd2, title='2. derivat. of sin(pix)',$
        xtitle='x', font=3, yrange=[min(amin), max(amax)]
    oplot, x, fd2num, line=1

     if withps eq 'y' then device,/close
     set_plot,'x'


;  print, dxf,xf

end


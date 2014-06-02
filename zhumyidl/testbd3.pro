PRO testbd3, nx,ny,nz,x,y,z,xmin,xmax,ymin,ymax,zmin,zmax

;--- test boundaries---
    print, 'Chosen boundaries:'
    print, 'xmin=', xmin, '  xmax=', xmax
    print, 'ymin=', ymin, '  ymax=', ymax
    print, 'zmin=', zmin, '  zmax=', zmax

    print, 'Simulation boundaries:'
    print, 'xmin=', x(1), '  xmax=', x(nx-2)
    print, 'ymin=', y(1), '  ymax=', y(ny-2)
    print, 'zmin=', z(1), '  zmax=', z(nz-2)

   if (xmin lt x(1)) then begin
     print, 'warning! xmin:',xmin,' out of bounds:',x(1),' Reset!'
     xmin=x(1)
   endif  
   if (xmax gt x(nx-2)) then begin
     print, 'warning! xmax:',xmax,' out of bounds:',x(nx-2),' Reset!'
     xmax=x(nx-2)
   endif  
   if (ymin lt y(0)) then begin
     print, 'warning! ymin:',ymin,' out of bounds:',y(0),' Reset!'
     ymin=y(0)
   endif  
   if (ymax gt y(ny-1)) then begin
     print, 'warning! ymax:',ymax,' out of bounds:',y(ny-1),' Reset!'
     ymax=y(ny-1)
   endif  
   if (zmin lt z(0)) then begin
     print, 'warning! zmin:',zmin,' out of bounds:',z(0),' Reset!'
     zmin=z(0)
   endif  
   if (zmax gt z(nz-1)) then begin
     print, 'warning! zmax:',zmax,' out of bounds:',z(nz-1),' Reset!'
     zmax=z(nz-1)
   endif  

return
end


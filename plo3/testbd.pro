PRO testbd, nx,ny,x,y,xmin,xmax,ymin,ymax

;--- test boundaries---
  
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

return
end


PRO smo, fin,fout,dir

;--- average array along direction dir ---
  
  s=size(fin) & mx=s(1) & my=s(2) 
    fout=fin
  if dir eq 1 then begin
    bound0=fin(0,*) & bound1=fin(mx-1,*)
    fout=0.5*fin+0.25*shift(fin,1,0)+0.25*shift(fin,-1,0)
    fout(0,*)=0.5*(fin(0,*)+fin(1,*))
    fout(mx-1,*)=0.5*(fin(mx-1,*)+fin(mx-2,*))
  endif
  if dir eq 2 then begin
    bound0=fin(*,0) & bound1=fin(*,mx-1)
    fout=0.5*fin+0.25*shift(fin,0,1)+0.25*shift(fin,0,-1)
    fout(0,*)=0.5*(fin(*,0)+fin(*,1))
    fout(mx-1,*)=0.5*(fin(*,mx-1)+fin(*,mx-2))
  endif  
  
return
end


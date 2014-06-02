PRO hgsetogsm, time,psi,t,a
  stept=30000.
start:
  b = a
  i=0 & j=-1
  ddim=size(time) & ntime=ddim(1)-1 & ddim=size(t) & nt=ddim(1)-1 

  while (time(i) lt (t(j+1)-stept)) do i=i+1
  
  while j lt nt do begin
    j=j+1
     
    while (time(i) lt (t(j)-stept)) do i=i+1

    b(1,j) = cos(psi(i))*a(1,j) - sin(psi(i))*a(2,j)
    b(2,j) = sin(psi(i))*a(1,j) + cos(psi(i))*a(2,j)
  endwhile 
  a=b

return
end



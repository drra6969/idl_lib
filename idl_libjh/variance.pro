; Variance matrix

PRO VARIANCE, II, JJ, N, POS, X, M
q=fltarr(3,3) & term=q & term1=q & term2=q & M=q
tot=fltarr(5,3)
tot=total(X(II:JJ,pos,*),1)
for k=0,2 do $
  for j=0,2 do $
    for i=II,JJ do begin
      term(j,k)=X(i,pos,j)*X(i,pos,k)
      term1(j,k)=term(j,k)+term1(j,k)
      term2(j,k)=(1./N)*tot(j)*tot(k)
endfor
M=(1./N)*(term1-term2)
return
end

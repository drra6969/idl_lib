;Eigenvectors & eigenvalues

PRO EVECS, M, D, E, EVAL, MAXDIR, MINDIR, INTDIR, EVEC
intdir=0
tred2, M,d,e
tqli, d,e,M
eval=d
mx=max(d,i) & maxdir=i
mn=min(d,i) & mindir=i
for i=0,2 do begin
  if (maxdir eq i or mindir eq i) then intdir = (i+1) mod 3
  if (intdir eq maxdir or intdir eq mindir) then intdir = (i+2) mod 3
endfor
evec=M
return
end
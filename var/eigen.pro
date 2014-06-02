PRO eigen, M, v1,v2,v3,d


 intdir=0
 nr_tred2, M,d,e	;Tridiagonalizes input matrix for TQLI
 nr_tqli, d,e,M	;Eigenvalues in d-vector, eig-vectors rows of M-matrix

 maxe=max([abs(d(0)),abs(d(1)),abs(d(2))],i)
 if i ne 0 then begin 
   t    = d(i)  &  tt     = M(*,i)
   d(i) = d(0)  &  M(*,i) = M(*,0)
   d(0) = t     &  M(*,0) = tt
 endif
 mine=min([abs(d(0)),abs(d(1)),abs(d(2))],i)
 if i ne 2 then begin 
   t    = d(i)  &  tt     = M(*,i)
   d(i) = d(2)  &  M(*,i) = M(*,2)
   d(2) = t     &  M(*,2) = tt
 endif
 v1 = M(*,0) & v2 = M(*,1) & v3 = M(*,2) 
 tt = crossp(v1,v2) 
 if total(tt*v3) lt 0 then v3=-v3

return
end

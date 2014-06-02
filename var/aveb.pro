PRO aveb, ivb,ive
COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv

bave=fltarr(ntv,3) & pbave=fltarr(ntv) & bavetot=pbave

k=0L
while ( tmb(k) le  tmv(ivb)-0.5*(tmv(ivb+1)-tmv(ivb)) ) do k=k+1
for i=ivb, ive-1 do begin
  k0=k
  while ( ( tmb(k) le tmv(i)+0.5*(tmv(i+1)-tmv(i)) ) $
                                and (k le (ntb-2)) ) do k=k+1
  if k-1 ge k0 then begin
    bave(i,0)=total(beqs(k0:(k-1),0))/float(k-k0)
    bave(i,1)=total(beqs(k0:(k-1),1))/float(k-k0)
    bave(i,2)=total(beqs(k0:(k-1),2))/float(k-k0)
    pbave(i)=total(pbeqs(k0:(k-1)))/float(k-k0)
    bavetot(i)=total(beqstot(k0:(k-1)))/float(k-k0)
  endif else begin
    print, 'BIG PROBLEM IN AVEB, time field:', tmb(k)
    print, '                   , time plasma:', tmv(i)
    bave(i,0)=0.5*(beqs(k0,0)+beqs(k0+1,0))
    bave(i,1)=0.5*(beqs(k0,1)+beqs(k0+1,1))
    bave(i,2)=0.5*(beqs(k0,2)+beqs(k0+1,2))
    pbave(i)=0.5*(pbeqs(k0)+pbeqs(k0+1))
    bavetot(i)=0.5*(beqstot(k0)+beqstot(k0+1))
  endelse
endfor

return
end

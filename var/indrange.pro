PRO indrange, t1min,t1max,tmin,tmax,ib,ie,ivb,ive,success,index
COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv

  
    success='yes'
;    print, 'Time min and max for B:', tmb(0), tmb(ntb-1)
;    print, 'Time min and max for Plasma:', tmv(0), tmv(ntv-1)
;    print, 'Present Range:', t1min,t1max
;    print, 'Input tmin,tmax:'
;    read, tmin,tmax
     if (tmin lt t1min) then begin
        print, 'TMIN has a wrong value!!!' & success='none' & endif
     if (tmax gt t1max) then begin
        print, 'TMAX has a wrong value!!!' & success='none' & endif
     if tmin ge tmax then begin
        print,'TMIN is larger then TMAX!!!' & success='none' & endif
    tvmin=tmin & tvmax=tmax

    ib=0L & ie=ntb-1 & ivb=0L & ive=ntv-1
    while tmb(ib) lt tmin do ib=ib+1 &   if ib gt 0 then ib=ib-1
    while tmb(ie) gt tmax do ie=ie-1 &   if ie lt ntb-1 then ie=ie+1
    while ( ((ie-ib) lt 2) and (ib gt 0) and (ie lt ntb-1)) do begin
     ib=ib-1 & ie=ie+1
    endwhile
    while tmv(ivb) lt tvmin do ivb=ivb+1 &   if ivb gt 0 then ivb=ivb-1
    while tmv(ive) gt tvmax do ive=ive-1 &   if ive lt ntv-1 then ive=ive+1
    while ( ((ive-ivb) lt 2) and (ivb gt 0) and (ive lt ntv-1)) do begin
     ivb=ivb-1 & ive=ive+1
    endwhile
    while (0.5*(tmv(ivb)+tmv(ivb+1))) le tmb(0) do ivb=ivb+1
    while (0.5*(tmv(ive)+tmv(ive-1))) ge tmb(ntb-1) do ive=ive-1
    if ((ive-ivb) lt 3) or ((ie-ib) lt 3) then success='none'

return
end

  

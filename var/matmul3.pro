PRO matmul3, m1, m2, mout

mout=fltarr(3,3)

mout(0,0)=total(m1(0,*)*m2(*,0))
mout(0,1)=total(m1(0,*)*m2(*,1))
mout(0,2)=total(m1(0,*)*m2(*,2))
mout(1,0)=total(m1(1,*)*m2(*,0))
mout(1,1)=total(m1(1,*)*m2(*,1))
mout(1,2)=total(m1(1,*)*m2(*,2))
mout(2,0)=total(m1(2,*)*m2(*,0))
mout(2,1)=total(m1(2,*)*m2(*,1))
mout(2,2)=total(m1(2,*)*m2(*,2))

return
end


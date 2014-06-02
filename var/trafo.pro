PRO trafo, M, xin,yin,zin,xout,yout,zout

;iter=size(y)

xout=m(0,0)*xin+m(1,0)*yin+m(2,0)*zin
yout=m(0,1)*xin+m(1,1)*yin+m(2,1)*zin
zout=m(0,2)*xin+m(1,2)*yin+m(2,2)*zin

return
end


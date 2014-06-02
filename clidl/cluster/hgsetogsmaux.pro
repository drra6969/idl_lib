PRO hgsetogsmaux, psi, a

b = a
b(1,*) = cos(psi(*))*a(1,*) - sin(psi(*))*a(2,*)
b(2,*) = sin(psi(*))*a(1,*) + cos(psi(*))*a(2,*)
a=b

return
end

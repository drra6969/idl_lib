; START OF PROGRAM


pi2=asin(1)
dx=0.1
xb=0.5
x=fltarr(5) & y=fltarr(5)
x(0)=xb-2.*dx & x(1)=xb-dx & x(2)=xb & x(3)=xb+dx & x(4)=xb+2.*dx
y=sin(pi2*x)

dya=(y(3)-y(1))/2./dx
dyb=(y(3)-y(2))/dx
dyc=(-y(4)+8.*y(3)-8.*y(1)+y(0))/12./dx

fexact=pi2*cos(pi2*xb)
print, 'dy/dx exact:', fexact
print, 'dx:',dx
print, ' (a) dy/dx: ', dya, '   dely/delx-dy/dx:', dya-fexact
print, ' (b) dy/dx: ', dyb, '   dely/delx-dy/dx:', dyb-fexact
print, ' (c) dy/dx: ', dyc, '   dely/delx-dy/dx:', dyc-fexact


pi2=asin(1)
dx=0.025
xb=0.5
x=fltarr(5) & y=fltarr(5)
x(0)=xb-2.*dx & x(1)=xb-dx & x(2)=xb & x(3)=xb+dx & x(4)=xb+2.*dx
y=sin(pi2*x)

dya=(y(3)-y(1))/2./dx
dyb=(y(3)-y(2))/dx
dyc=(-y(4)+8.*y(3)-8.*y(1)+y(0))/12./dx

fexact=pi2*cos(pi2*xb)
print, 'dy/dx exact:', fexact
print, 'dx:',dx
print, ' (a) dy/dx: ', dya, '   dely/delx-dy/dx:', dya-fexact
print, ' (b) dy/dx: ', dyb, '   dely/delx-dy/dx:', dyb-fexact
print, ' (c) dy/dx: ', dyc, '   dely/delx-dy/dx:', dyc-fexact




end
;START OF MAIN PROGRAM

y=fltarr(40,40)
z=shift(dist(40),20,20)
z=z/20*cos(z/3.0)
x=shift(dist(40),10)
erase
surface, z,ax=70,az=25  
erase
for i=0,39 do y(*,i)=x(i)*z(*,i)
surface, y,ax=70,az=25  
end

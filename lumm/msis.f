	program msis
*	test the MSIS atmosphere for a number of input parameters
	parameter (mxlyr = 141)
	real z(mxlyr)
	real rho(mxlyr),N2(mxlyr),O2(mxlyr),O(mxlyr)
	real He(mxlyr),Ar(mxlyr),H(mxlyr),N(mxlyr),T(mxlyr)
	character*8 text		! for MSIS-90
	real temp(2),dens(8)		! for MSIS-90
	integer yyddd			! for MSIS-90

*	z 	Altitude of layers in cm (zmin, zmax in km)
*	rho	density at layer boundaries (g cm-3)
	
	read(*,*) mlyr
	read(*,*) (z(m), m=1,mlyr)
	read(*,*) yyddd, hour, glat, glon, f107av, f107, ap

	do m=1,mlyr
	   call atm(text,z(m),yyddd,hour,glat,glon,f107av,f107,ap,
     .                 temp,dens)
	   rho(m)=dens(6)
	   N2(m)=dens(3)
	   O2(m)=dens(4)
	   O(m)=dens(2)
	   Ar(m)=dens(5)
	   He(m)=dens(1)
	   H(m)=dens(7)
	   N(m)=dens(8)
	   T(m)=temp(2)
	end do

	print*, (rho(m),m=1,mlyr)
	print*, (N2(m),m=1,mlyr)
	print*, (O2(m),m=1,mlyr)
	print*, (O(m),m=1,mlyr)
	print*, (N(m),m=1,mlyr)
	print*, (He(m),m=1,mlyr)
	print*, (H(m),m=1,mlyr)
	print*, (Ar(m),m=1,mlyr)
	print*, (T(m),m=1,mlyr)

	stop
	end
 

*subroutine to call MSIS

	subroutine atm(text,z,yyddd,hour,gglat,gglon,f107av,
     .	  f107,ap,temp,dens)
*
*	Computes a model atmosphere based on MSIS
*
	real z,hour,gglat,gglon,f107av,f107,ap(8),temp(2),dens(8)
	real sec,alt,stl
	integer yyddd
	character*8 text
	text=' MSIS-90'
	alt=z*1.e-5
	stl=hour+int(gglon/15.)-gglon/15.              	! solar time
	sec=amod(hour-gglon/15.+24.,24.)*3600.         	! UT in seconds
	call gtd6(yyddd,sec,alt,gglat,gglon,stl,f107av,f107,
     .	  ap,48,dens,temp)
C     OUTPUT:
C        D(1) - HE NUMBER DENSITY(CM-3)
C        D(2) - O NUMBER DENSITY(CM-3)
C        D(3) - N2 NUMBER DENSITY(CM-3)
C        D(4) - O2 NUMBER DENSITY(CM-3)
C        D(5) - AR NUMBER DENSITY(CM-3)                       
C        D(6) - TOTAL MASS DENSITY(GM/CM3)
C        D(7) - H NUMBER DENSITY(CM-3)
C        D(8) - N NUMBER DENSITY(CM-3)
C        T(1) - EXOSPHERIC TEMPERATURE
C        T(2) - TEMPERATURE AT ALT

	return
	end


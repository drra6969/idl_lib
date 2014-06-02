c**************************
	program   magn
c**************************
        include 'rin'
        
	real     bx(nv,nz),by(nv,nz),bz(nv,nz),
     +           sx(nv,nz),sy(nv,nz),sz(nv,nz),
     +           rho(nv,nz),u(nv,nz),res(nv,nz),prof(nv,nz)
	real     xa(nx),dxa(nx),ya(ny),dya(ny),za(nz),dza(nz),
     +           zeit
   
c
	integer         mx,my,mz,ksafe,mout,nfirst,nlast
       character*5      magon
       character*7      magalln
       character*9      parno
c................................................................
c   program to convert formatted data to unformatted
c     here all mhd quantities
c................................................................
      magon='magon'
      magalln='magalln'
      parno='123456789'
      nfirst = 1
      nlast = 1
      write(*,*) 'Input first dataset and last dataset', 
     +            ' (one digitnumbers only!)'
      read(*,*) nfirst, nlast

      do 1000 mout = nfirst,nlast
      msafe=mout

      magon(5:5)=parno(mout:mout)
      open (16,file=magon)
      magalln(7:7)=parno(mout:mout)
      open (14,file=magalln,form='unformatted')
 
      write(*,*) 'dataset:', mout
      read(16,88) mx, my, mz, zeit
      if (mx.ne.nx .or. my.ne.ny .or. mz.ne.nz) then
        write(*,*) 'number of gridpoints in dataset'
        write(*,*) 'no_x=', mx, 'no_y=', my, 'no_z=', mz
        write(*,*) 'not equal to program'
        stop
      end if

      read(16,888)
      do 11 ix = 1,nx
        read(16,8888) xa(ix),dxa(ix)
c        write(*,*)  xa(ix),dxa(ix)
 11   continue
      read(16,888)
      do 12 iy = 1,ny
 	 read(16,8888) ya(iy),dya(iy)
c        write(*,*)  ya(iy),dya(iy)
 12   continue
      read(16,888)
      do 13 iz = 1,nz
 	 read(16,8888) za(iz),dza(iz)
c        write(*,*)  za(iz),dza(iz)
 13   continue

      call inwas
      do 100 iz = 1,nz
c      write(*,*) help(2,iz), help(nv-nx-1,iz)
      do 100 iv = 1,nv
        rho(iv,iz)=help(iv,iz)
 100  continue

      write(14)   nx,ny,nz,zeit
      write(14)   xa,dxa,xa,xa,xa,xa,xa,
     +            ya,dya,ya,ya,ya,ya,ya,
     +            za,dza,za,za,za,za,za
      write(14)   bx,by,bz
      write(14)   sx,sy,sz
      write(14)   rho,u,res
      
      close(14)
      close(16)
      write(*,*) 'set', mout,'  done'

 1000 continue
c
   88 format(/,3i7,f9.3)
  888 format(/)
 8888 format(1x,7f12.6)
c
      stop
      end
c####################################################################
	subroutine inwas
        include 'rin'
c
        integer    iv,iy,ix1,ix2
        real       xh9
c .....................................
c  formatted output
c .....................................
      do 200 iz = 1,nz
      read(16,1) 
        ix1 = 1
        ix2 = 9
  110   read(16,3) xh9
c        write(*,*) xh9
        do 150 iy = 1,ny
	  read(16,4) (help(iv,iz),iv=ix1+(iy-1)*nx,ix2+(iy-1)*nx)
c	  write(*,4) (help(iv,iz),iv=ix1+(iy-1)*nx,ix2+(iy-1)*nx)
  150   continue
        ix1 = ix1+9
        ix2 = ix2+9
        if(ix2.gt.nx) ix2=nx
        if(ix1.le.nx) go to 110
  200 continue
c
    1 format(/)
    3 format(f8.3)
    4 format(9(f8.4))
c
      return
      end

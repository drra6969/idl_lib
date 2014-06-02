PRO ploinfo, fa,fb,fc,plane,fgroup,head1,head2

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time
COMMON program_var, fp1,fp2,xsu,ysu,f0,xchoice,ychoice,far1,far2,xar,yar,$
                    cutata,cutatb

    if plane eq 'x' then begin 
      if fgroup eq '2' then begin
        head1='Velocity Vy' & head2='Velocity Vz' & endif
      if fgroup eq '3' then begin
        head1='Magn. Field By' & head2='Magn. Field Bz' & endif
      if fgroup eq '4' then begin
        head1='Curr. Dens. Jy' & head2='Curr. Dens. Jz' & endif
      if fgroup eq '5' then begin
        head1='Electric Field Ey' & head2='Electric Field Ez' & endif
      fp1=fb
      fp2=fc
    endif
    if plane eq 'y' then begin 
      if fgroup eq '2' then begin
        head1='Velocity Vx' & head2='Velocity Vz' & endif
      if fgroup eq '3' then begin
        head1='Magn. Field Bx' & head2='Magn. Field Bz' & endif
      if fgroup eq '4' then begin
        head1='Curr. Dens. Jx' & head2='Curr. Dens. Jz' & endif
      if fgroup eq '5' then begin
        head1='Electric Field Ex' & head2='Electric Field Ez' & endif
      fp1=fa
      fp2=fc
    endif
    if plane eq 'z' then begin 
      if fgroup eq '2' then begin
        head1='Velocity Vx' & head2='Velocity Vy' & endif
      if fgroup eq '3' then begin
        head1='Magn. Field Bx' & head2='Magn. Field By' & endif
      if fgroup eq '4' then begin
        head1='Curr. Dens. Jx' & head2='Curr. Dens. Jy' & endif
      if fgroup eq '5' then begin
        head1='Electric Field Ex' & head2='Electric Field Ey' & endif
      fp1=fa
      fp2=fb
    endif

return
end


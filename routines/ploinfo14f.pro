PRO ploinfo14f, fa,fb,fc,fc1,fc2,fc3,plane,fgroup,head4,head5

; Program ploinfo14f
; Modification of Antonius Otto's ploinfo1
; Works with col4f.pro
; Version 1
; by Fred Hall IV
; 3 December 1999

COMMON program_par, nx,nxn,nxf, ny,nyn,nyf, nz,nzn,nzf,$
                    x,y,z, xf,yf,zf, xn,yn,zn, iox,ioy,ioz, $
                    ioxf,ioyf,iozf, run, time

COMMON program_var, fp1,fp2,xsu,ysu,f0,xchoice,ychoice,far1,far2,xar,yar,$
                    cutata,cutatb

    if plane eq 'x' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vx' & head5='Velocity Vy/Vz' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field Bx' & head5='Magn. Field By/Bz' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jx' & head5='Curr. Dens. Jy/Jz' & endif
      if fgroup eq '5' then begin
        head4='Electric Field Ex' & head5='Electric Field Ey/Ez' & endif
      if fgroup eq '8' then begin
        head4='Inertial Force (F_in)_x'
        head5='Inertial Force (F_in)_y/(F_in)_z'
      endif
      if fgroup eq '9' then begin
        head4='Pressure Gradient Force (F_gp)_x'
        head5='Pressure Gradient Force (F_gp)_y/(F_gp)_z'
      endif
      if fgroup eq '10' then begin
        head4='J X B force (J X B)_x'
        head5='J X B force (J X B)_y/(J X B)_z'
      endif
      if fgroup eq '11' then begin
        head4='Total Force (F_tot)_x'
        head5='Total Force (F_tot)_y/(F_tot)_z'
      endif
      xar=yn & yar=zn  
      fp2=fa 
      f0=fc1
      far1=interpolate(fc2,ioy,ioz,/grid)
      far2=interpolate(fc3,ioy,ioz,/grid)
    endif
    if plane eq 'y' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vy' & head5='Velocity Vz/Vx' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field By' & head5='Magn. Field Bz/Bx' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jy' & head5='Curr. Dens. Jz/Jx' & endif
      if fgroup eq '5' then begin
        head4='Electric Field Ey' & head5='Electric Field Ez/Ex' & endif
      if fgroup eq '8' then begin
        head4='Inertial Force (F_in)_y'
        head5='Inertial Force (F_in)_z/(F_in)_x'
      endif
      if fgroup eq '9' then begin
        head4='Pressure Gradient Force (F_gp)_y'
        head5='Pressure Gradient Force (F_gp)_z/(F_gp)_x'
      endif
      if fgroup eq '10' then begin
        head4='J X B force (J X B)_y'
        head5='J X B force (J X B)_z/(J X B)_x'
      endif
      if fgroup eq '11' then begin
        head4='Total Force (F_tot)_y'
        head5='Total Force (F_tot)_z/(F_tot)_x'
      endif
      xar=xn & yar=zn  
      fp2=fb 
      f0=fc2
      far1=interpolate(fc1,iox,ioz,/grid)
      far2=interpolate(fc3,iox,ioz,/grid)
    endif
    if plane eq 'z' then begin 
      if fgroup eq '2' then begin
        head4='Velocity Vz' & head5='Velocity Vx/Vy' & endif
      if fgroup eq '3' then begin
        head4='Magn. Field Bz' & head5='Magn. Field Bx/By' & endif
      if fgroup eq '4' then begin
        head4='Curr. Dens. Jz' & head5='Curr. Dens. Jx/Jy' & endif
      if fgroup eq '5' then begin
        head4='Electric Field Ez' & head5='Electric Field Ex/Ey' & endif
      if fgroup eq '8' then begin
        head4='Inertial Force (F_in)_z'
        head5='Inertial Force (F_in)_x/(F_in)_y'
      endif
      if fgroup eq '9' then begin
        head4='Pressure Gradient Force (F_gp)_z'
        head5='Pressure Gradient Force (F_gp)_x/(F_gp)_y'
      endif
      if fgroup eq '10' then begin
        head4='J X B force (J X B)_z'
        head5='J X B force (J X B)_x/(J X B)_y'
      endif
      if fgroup eq '11' then begin
        head4='Total Force (F_tot)_z'
        head5='Total Force (F_tot)_x/(F_tot)_y'
      endif
      xar=xn & yar=yn 
      fp2=fc 
      f0=fc3 
      far1=interpolate(fc1,iox,ioy,/grid)
      far2=interpolate(fc2,iox,ioy,/grid)
    endif
    print,size(far1)
    far1(0,*)=0. & far1(*,0)=0. & far2(0,*)=0. & far2(*,0)=0.
    far1(nxn-1,*)=0. & far1(*,nyn-1)=0. & far2(nxn-1,*)=0. & far2(*,nyn-1)=0.


return
end


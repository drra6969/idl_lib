PRO setplotvar,ie,ib,ive,ivb,timeb,timev,exr,eyr,ezr,smo
COMMON ref, bxr,byr,bzr,vxr,vyr,vzr, $
            rhor,pr,babr,ptotr,pbr,tempr,beta,t, $
            xsat1,ysat1,zsat1,vxsat1,vysat1,vzsat1, $
            xsat2,ysat2,zsat2,vxsat2,vysat2,vzsat2, $
            index, starttime, phi, xtit, withps

COMMON eqs, beqs,veqs,neqs,teqs,peqs,pbeqs,beqstot,$
            bave,pbave,bavetot,tmb,tmv,ntb,ntv
COMMON plaxis,tax


    bxr=fltarr(ie-ib+1) & byr=bxr & bzr=bxr
    bxr=beqs(ib:ie,0)  & byr=beqs(ib:ie,1) & bzr=beqs(ib:ie,2)
    babr=beqstot(ib:ie) 
    vxr=fltarr(ive-ivb+1) & vyr=vxr & vzr=vxr
    baxr=vxr & bayr=vxr & bazr=vxr
    baxr=bave(ivb:ive,0) & bayr=bave(ivb:ive,1) & bazr=bave(ivb:ive,2) 
    vxr=veqs(ivb:ive,0)  & vyr=veqs(ivb:ive,1) & vzr=veqs(ivb:ive,2)
    rhor=neqs(ivb:ive) & pr=peqs(ivb:ive)      
    ptotr=peqs(ivb:ive)+pbave(ivb:ive) & pbr=pbave(ivb:ive) 
    tempr=teqs(ivb:ive) & beta=peqs(ivb:ive)/pbeqs(ivb:ive)
    timeb=tmb(ib:ie) & timev=tmv(ivb:ive)
    efieldeqs, vxr,vyr,vzr, baxr,bayr,bazr, exr,eyr,ezr
    if smo eq 'y' then begin 
      vxr=smooth(vxr,3) & vyr=smooth(vyr,3) & vzr=smooth(vzr,3)
      rhor=smooth(rhor,3) & endif

return
end

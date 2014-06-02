PRO printstuff, bx,by,bz,sx,sy,sz,rho,u,res

  amin=min(rho) & amax=max(rho) &  thresh=amin+0.5*(amax-amin)
  print,'rho:',amin,amax,thresh
  amin=min(bx) & amax=max(bx) &  thresh=amin+0.5*(amax-amin)
  print,'bx:',amin,amax,thresh
  amin=min(by) & amax=max(by) &  thresh=amin+0.5*(amax-amin)
  print,'by:',amin,amax,thresh
  amin=min(bz) & amax=max(bz) &  thresh=amin+0.5*(amax-amin)
  print,'bz:',amin,amax,thresh
  amin=min(sx) & amax=max(sx) &  thresh=amin+0.5*(amax-amin)
  print,'sx:',amin,amax,thresh
  amin=min(sy) & amax=max(sy) &  thresh=amin+0.5*(amax-amin)
  print,'sy:',amin,amax,thresh
  amin=min(sz) & amax=max(sz) &  thresh=amin+0.5*(amax-amin)
  print,'sz:',amin,amax,thresh
  amin=min(res) & amax=max(res) &  thresh=amin+0.5*(amax-amin)
  print,'res:',amin,amax,thresh


return
end


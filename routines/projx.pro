  pro projx, xval, yli, zli

  s=size(yli)
  n=s(2)
  xli=replicate(xval,n)
  plots,xli,yli,zli,linestyle=1,/t3d,/data
end

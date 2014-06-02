  pro projy, yval, xli, zli

  s=size(xli)
  n=s(2)
  yli=replicate(yval,n)
  plots,xli,yli,zli,linestyle=1,/t3d,/data
end

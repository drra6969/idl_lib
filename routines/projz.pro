  pro projz, zval, xli, yli

  s=size(yli)
  n=s(2)
  zli=replicate(zval,n)
  plots,xli,yli,zli,linestyle=1,/t3d,/data
end

Pro hcldate, time, datestrn, strnn

  CDF_EPOCH, time(0), yr, mo, dy, hr, mn, sc, milli, /BREAK
  datestrn = string(yr,'(i4.4)')+'.'+string(mo,'(i2.2)')+'.'+string(dy,'(i2.2)')
  strnn = 'Cluster '+datestrn
  print, strnn

  return
end
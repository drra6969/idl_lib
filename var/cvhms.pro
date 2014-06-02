FUNCTION cvhms, time
 hour = LONG(time)
 minute = LONG(60.*(time-hour)) 
 sec = 3600*time mod 60
RETURN, STRING(hour, minute, sec, $
        FORMAT="(i2.2, ':', i2.2, ':', i2.2)")
END

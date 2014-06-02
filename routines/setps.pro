pro setps

        set_plot,'ps'
        device,filename='con.ps'
        device,/landscape
        !P.THICK=2.
        device,/inches,xsize=7.,scale_factor=1.0,xoffset= 0.8
        device,/inches,ysize=5.5,scale_factor=1.0,yoffset=9.5
;        device,/times,/bold,font_index=3

return
end

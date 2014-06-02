Pro reseng

; Plot the graphs of resonance energy h_res versus resonance numbers 
; N_res for b_z = 0.05, phi = 90, and alpha_in = 0, 15, 30, and 45
; degrees.

; Resonance numbers N_res
N_res = 2*indgen(10) + 1

; Computed resonance energies
h_com = (5.D-2*(N_res + 6.D-1))^4

; Normalized energies (uncorrected) for alpha = 0
h_0=[1.2D-3,7.5D-3,2.7D-2,7.D-2,1.5D-1,3.D-1,5.D-1,9.2D-1,1.35D0]

; Normalized energies (uncorrected) for alpha = 15
h_15=[1.2D-3,6.95D-3,2.6D-2,6.6D-2,1.45D-1,2.7D-1,5.D-1,9.1D-1,1.3D0]

; Normalized energies for alpha = 30
h_30=[1.3D-3,6.1D-3,2.4D-2,5.9D-2,0.13D0,0.25D0,0.53D0,0.9D0,1.2D0]

; Normalized energies for alpha = 45
h_45=[1.18D-3,6.D-3,2.2D-2,5.4D-2,1.D-1,2.2D-1,4.7D-1,1.51D0]

; Generate plots
; set_plot,'ps'
; device,/landscape
!P.MULTI = [0,2,2]


Plot_IO,N_res(1:*),h_0,Title='!7a!3!Din!N = 0!9%!3',Xtitle='N!Dres!N',    $
        Ytitle='h!Dres!N',PSYM=-2,LINE=0
OPlot,N_res, h_com,PSYM=-7,LINE=1
LegendX=[2,5]
STX=7
OPlot,LegendX,[4.D0,4.D0],PSYM=-2,LINE=0
XYOUTS,STX,4.D0,'Actual'
OPlot,LegendX,[2.D0,2.D0],PSYM=-7,LINE=1
XYOUTS,STX,2.D0,'Computed'


Plot_IO,N_res(1:*),h_15,Title='!7a!3!Din!N = 15!9%!3',Xtitle='N!Dres!N',  $
        Ytitle='h!Dres!N',PSYM=-2,LINE=0
OPlot,N_res, h_com,PSYM=-7,LINE=1
LegendX=[2,5]
STX=7
OPlot,LegendX,[4.D0,4.D0],PSYM=-2,LINE=0
XYOUTS,STX,4.D0,'Actual'
OPlot,LegendX,[2.D0,2.D0],PSYM=-7,LINE=1
XYOUTS,STX,2.D0,'Computed'


Plot_IO,N_res(1:*),h_30,Title='!7a!3!Din!N = 30!9%!3',Xtitle='N!Dres!N',  $
        Ytitle='h!Dres!N',PSYM=-2,LINE=0
OPlot,N_res, h_com,PSYM=-7,LINE=1
LegendX=[2,5]
STX=7
OPlot,LegendX,[4.D0,4.D0],PSYM=-2,LINE=0
XYOUTS,STX,4.D0,'Actual'
OPlot,LegendX,[2.D0,2.D0],PSYM=-7,LINE=1
XYOUTS,STX,2.D0,'Computed'


Plot_IO,N_res(1:*),h_45,Title='!7a!3!Din!N = 45!9%!3',Xtitle='N!Dres!N',  $
        Ytitle='h!Dres!N',PSYM=-2,LINE=0
OPlot,N_res, h_com,PSYM=-7,LINE=1
LegendX=[2,5]
STX=7
OPlot,LegendX,[4.D0,4.D0],PSYM=-2,LINE=0
XYOUTS,STX,4.D0,'Actual'
OPlot,LegendX,[2.D0,2.D0],PSYM=-7,LINE=1
XYOUTS,STX,2.D0,'Computed'

; device,/close
; set_plot,'x'

End

PRO chcolorscale, titl, bmin,bmax, fmin,fmax

        chval0='' & chval1=''

        fmin=bmin & fmax=bmax 
        print,'fmin and fmax for color set to min,max of field ',titl,':'
        print,'fmin = ',bmin, '       fmax = ',bmax
        print,'Return or <no> to keep values, fmin+return to change values'
    
        read, chval0
        if (chval0 ne '') and (chval0 ne 'n') and (chval0 ne 'no') then begin
          read, chval1 & fmin=float(chval0) & fmax=float(chval1)
        endif

return
end


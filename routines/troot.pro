         function FUNC, x
           return, exp(sin(x)^2 + cos(x)^2 - 1) - 1 
         end 



         x = [0.0, -!pi/2, !pi]
         root = FX_ROOT(x, 'FUNC', /double)
         print, exp(sin(root)^2 + cos(root)^2 - 1) - 1
         x = [complex(-!pi/3, 0), complex(0, !pi), complex(0, -!pi/6)]
         root = FX_ROOT(x, 'FUNC')
         print, exp(sin(root)^2 + cos(root)^2 - 1) - 1

end

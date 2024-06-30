module func_module
    use func_calc
    implicit none 

    contains 

    subroutine newton(a, b, c, d, x1, n)
        implicit none 

        real(8) ,intent(in)    :: a, b, c, d
        real(8) ,intent(inout) :: x1
        integer ,intent(out)   :: n

        real(8)  x2, f, df, er

        real(8) ,parameter :: eps =1.0d-15, df_min = 1.0d-99
        integer ,parameter :: n_max = 1000, output_unit = 10
        character(32) :: fmt = '(i3,e24.16)'

        open(unit = output_unit, file = 'out.dat', status = 'replace', action = 'write')

        write(output_unit, fmt) 0, x1
        do n = 1, n_max 
            call function(a, b, c, d, x1, f) 
            call derivative(a, b, c, x1, df) 

            if (abs(df) < df_min) stop 'Error, derivative is zero.'
    
            x2 = x1 - f / df
            write(output_unit, fmt) n, x2

            er = abs(x1 - x2)/abs(x1)
            if (er < eps) exit

            x1 = x2
        enddo
    end subroutine newton
end module func_module
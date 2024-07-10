!絶対誤差で収束計算

!つまり解がすでにわかってる状態を想定されており、収束の速さを見るために使う

!出力は反復回数に対する、誤差(真値-計算値)


module newton
    use func_calc
    implicit none 
    contains 

    subroutine newton_method(a, b, c, d, x1, n)
        implicit none 

        real(8) ,intent(in)    :: a, b, c, d
        real(8) ,intent(inout) :: x1
        integer ,intent(out)   :: n

        real(8)  x2, f, df, er

        real(8) ,parameter :: eps =1.0d-15
        integer ,parameter :: n_max = 1000, output_unit = 10
        !ここで真値を定義する。
        integer ,parameter :: true_value = -1

        character(32) :: fmt = '(i3,e24.16)'

        open(unit = output_unit, file = 'out.dat', status = 'replace', action = 'write')

        write(output_unit, fmt) 0, x1 - true_value
        do n = 1, n_max 
            call function(a, b, c, d, x1, f) 
            call derivative(a, b, c, x1, df) 

            if (abs(df) == 0) stop 'Error, derivative is zero.'
    
            x2 = x1 - f / df
            write(output_unit, fmt) n, x2 - true_value

            er = abs(x2 - 1)
            if (er < eps) exit

            x1 = x2
        enddo
    end subroutine newton_method

end module newton
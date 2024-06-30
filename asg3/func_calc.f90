module func_calc
    implicit none 
    contains
    !f(x) = ax^3 + bx^2 + cx + d
    subroutine function(a, b, c, d, x, f)
        real(8) ,intent(in) :: a, b, c, d, x 
        real(8) ,intent(out) :: f 
        f = a*x**3 + b*x**2 + c*x + d
    end subroutine function

    !f'(x) = 3ax^2 + 2bx + c
    subroutine derivative(a, b, c, x, df)
        real(8) ,intent(in) :: a, b, c, x
        real(8) ,intent(out) :: df
        df = 3.0d0*a*x**2 + 2.0d0*b*x + c   
    end subroutine derivative
end module func_calc
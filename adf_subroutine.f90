
module deriv_m 
    implicit none 

    interface operator(.add.)
        module procedure :: add_p
    end interface

    interface operator(.sub.)
        module procedure :: sub_p
    end interface

    interface operator(.mul.)
        module procedure :: mul_p
    end interface

    interface operator(.div.)
        module procedure :: div_p
    end interface
contains

    function add_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  + b(1)
         res(2) = a(2) + b(2)
    end function add_p

    function sub_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  - b(1)
         res(2) = a(2) - b(2)
    end function sub_p

    function mul_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  * b(1)
         res(2) = a(2) * b(1) + a(1) * b(2)
    end function mul_p

    function div_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  / b(1)
         res(2) = (a(2) * b(1) - a(1) * b(2)) / b(1)**2
    end function div_p

end module deriv_m

module ad
    use deriv_m
    implicit none 
    contains 

    subroutine auto_derivative(a_val, b_val, c_val, d_val, x_val, dy)
        real(8), dimension(2) :: a, b, c, d, x, y

        real(8), intent(in)  :: a_val, b_val, c_val, d_val, x_val
        real(8), intent(out) :: dy

        a = (/a_val,0.0d0/)
        a(1) = a_val; a(2) = 0.0d0
        b(1) = b_val; b(2) = 0.0d0
        c(1) = c_val; c(2) = 0.0d0
        d(1) = d_val; d(2) = 0.0d0
        x(1) = x_val; x(2) = 1.0d0

        y = a .mul. x .mul. x .mul. x .add. (b .mul. x .mul. x) .add. (c .mul. x) .add. d
         
        dy = y(2)

    end subroutine auto_derivative
end module ad 


program auto_derivative_p
    use deriv_m
    use ad
    implicit none

    real(8) df, a, b, c, d, x
    
    !値の設定
    a = 1.0d0
    b = 0.0d0
    c = -1.0d0
    d = 0.0d0
    x = 1.0d0

    call auto_derivative(a, b, c, d, x, df)
    write(*,*) 'dy/dx = ', df

end program auto_derivative_p
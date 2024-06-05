!数値微分の完成

!微分の線形性や、ライプニッツ則のアルゴリズムを定義
!各演算子ごとの、オペレーションを type を用いて派生タイプを定義し、その中に関数を定義
module deriv_m 

    implicit none 

    type :: pair_t
        real(8) :: x, dx 
    end type pair_t 

    interface operator(+)
        module procedure :: add_p
    end interface

    interface operator(-)
        module procedure :: sub_p
    end interface

    interface operator(*)
        module procedure :: mul_p
    end interface

    interface operator(/)
        module procedure :: div_p
    end interface
contains

    !微分の線形性を用いた演算子の定義
    pure elemental type (pair_t) function add_p(a, b) result(res)
         type(pair_t), intent(in) :: a, b
         res%x  = a%x  + b%x
         res%dx = a%dx + b%dx
    end function add_p

    pure elemental type (pair_t) function sub_p(a, b) result(res)
         type(pair_t), intent(in) :: a, b
         res%x  = a%x  - b%x
         res%dx = a%dx - b%dx
    end function sub_p

    !ライプニッツ則を用いた演算子の定義
    pure elemental type (pair_t) function mul_p(a, b) result(res)
         type(pair_t), intent(in) :: a, b
         res%x  = a%x  * b%x
         res%dx = a%dx * b%x + a%x * b%dx
    end function mul_p

    pure elemental type (pair_t) function div_p(a, b) result(res)
         type(pair_t), intent(in) :: a, b
         res%x  = a%x  / b%x
         res%dx = (a%dx * b%x - a%x * b%dx) / b%x**2
    end function div_p

end module deriv_m

program auto_derivative
    use deriv_m
    implicit none

    type(pair_t) :: a, b, c, d, x, y
    real(8) :: a_val, b_val, c_val, d_val, x_val

    !定数値の設定
    a_val = 1.0d0
    b_val = 1.0d0
    c_val = -1.0d0
    d_val = 0.0d0
    x_val = 1.0d0

    !定数値を代入、微分値を設定
    a%x = a_val; a%dx = 0.0d0
    b%x = b_val; b%dx = 0.0d0
    c%x = c_val; c%dx = 0.0d0
    d%x = d_val; d%dx = 0.0d0
    x%x = x_val; x%dx = 1.0d0

    ! y = ax^2 + bx + c の計算
    y = a * x*x*x + b * x*x 

    ! y の導関数を出力します
    write(*,*) 'Derivative of ax^2 + bx + c at x = ', x_val, ' is ', y%dx
end program auto_derivative
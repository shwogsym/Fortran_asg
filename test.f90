!数値微分の完成
!func →　subroutine として書かせたもの、かなり複雑になっていることがわかる

module deriv_m
    implicit none

    type :: pair_t
        real(8) :: x, dx
    end type pair_t

contains

    pure elemental subroutine add_p(a, b, res)
         type(pair_t), intent(in) :: a, b
         type(pair_t), intent(out) :: res
         res%x  = a%x  + b%x
         res%dx = a%dx + b%dx
    end subroutine add_p

    pure elemental subroutine sub_p(a, b, res)
         type(pair_t), intent(in) :: a, b
         type(pair_t), intent(out) :: res
         res%x  = a%x  - b%x
         res%dx = a%dx - b%dx
    end subroutine sub_p

    pure elemental subroutine mul_p(a, b, res)
         type(pair_t), intent(in) :: a, b
         type(pair_t), intent(out) :: res
         res%x  = a%x  * b%x
         res%dx = a%dx * b%x + a%x * b%dx
    end subroutine mul_p

    pure elemental subroutine div_p(a, b, res)
         type(pair_t), intent(in) :: a, b
         type(pair_t), intent(out) :: res
         res%x  = a%x  / b%x
         res%dx = (a%dx * b%x - a%x * b%dx) / b%x**2
    end subroutine div_p

end module deriv_m


!フォワードモードの自動微分
program auto_derivative
    use :: deriv_m
    implicit none

    type(pair_t) :: a, b, c, d, x, y, temp1, temp2, temp3, temp4
    real(8) :: a_val, b_val, c_val, d_val, x_val

    !定数値の設定
    a_val = 1.0d0
    b_val = 0.0d0
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
    call mul_p(a, x, temp1)  ! ax
    call mul_p(temp1, x, temp2)  ! ax^2
    call mul_p(b, x, temp3)  ! bx
    call add_p(temp2, temp3, temp4)  ! ax^2 + bx
    call add_p(temp3, c, temp2)  ! ax^2 + bx + c
    call add_p(temp2, d, y)

    ! y の導関数を出力します
    write(*,*) 'Derivative of ax^2 + bx + c at x = ', x_val, ' is ', y%dx
end program auto_derivative
!解像度上げたい。

module func_module 
    implicit none 
    contains 

    subroutine simpson(n, h, f, integral_appro)
        integer i
        integer, intent(in) :: n
        real(8), intent(in) :: h
        real(8), intent(in) :: f(2,0:n)
         !intend in にて、パラメータが外部からインプットされる変数であり、サブルーチン内でそれらを変更しないように定義している。
        real(8), intent(inout) :: integral_appro

        !integral_appro (微小区間の積分値) の初期設定
        integral_appro = f(2,0) + f(2,n) 

        do i = 1, n-1, 2
            integral_appro = integral_appro + 4.0d0 * f(2,i) 
        end do 

        do i = 0, n-2, 2 
            integral_appro = integral_appro + 2.0d0 * f(2,i) 
        end do 
        
        integral_appro = integral_appro * h / 3.0d0 
    end subroutine simpson

end module func_module



program asg7 
    use func_module
    implicit none 
    integer :: n, i 
    real(8) :: x, integral_appro, a, b, h
    real(8), allocatable :: f(:,:)
    
    a = 0.0d0 
    b = 2.0d0 * acos(0.0d0)  !積分範囲の設定、上限値はπ

    write(*,*) 'Input n (number of divisions)'
    read(*,*) n 

    h = (b-a) / n  !台形の刻み幅の計算

    allocate(f(2,0:n)) 

    !分割点での sin(x) の値の計算
    do i = 0, n 
        x = a + i * h 
        f(1,i) = x
        f(2,i) = sin(x) 
    end do 

    call simpson(n, h, f, integral_appro)
    
    deallocate(f) 

    write(*,*) 'Approximate integral of sin(x) = ', integral_appro 
end program asg7  

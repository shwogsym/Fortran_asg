module func_module
    implicit none 
    contains 

    subroutine bisection_method(a, b, c, x1, x2, t, xm)
    implicit none 
    real(8) :: a, b, c, x1, x2, xm, fm
    integer :: t, max_t = 1000, fi = 10
    real(8), parameter :: er = 1.0e-6


    if (func(a, b, c, x1) * func(a, b, c, x2) > 0) then
        stop 'One of the values should be positive and the other negative.'
    endif 
    !２つの初期値が二分法を回す条件を満たしているのか検証

    t = 0 
    open(fi, file = 'outasg4.d')

    do while (abs(x2 - x1) > er .and. t < max_t) 
        xm = (x1 + x2) / 2.0
        fm = func(a, b, c, xm)
        !中央値の計算

        if (fm < 0) then
             !中点の値が0以下のとき→x1,x2の中負の値の方と値を交換したい。↓で判定
            if (func(a, b, c, x2) < 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        else 
            !else つまり、中点の値が0より大きいとき同様に判定
            if (func(a, b, c, x2) > 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        endif
        !中点の値に応じて初期値の入れ替えを行い、次のサイクルに準備

        t = t + 1 
        write(fi,*) t, xm 
    end do

    close(fi)
    end subroutine bisection_method

    real(8) function func(a, b, c, x)
        implicit none 
        real(8) :: a, b, c, x
        func = a*x**2.0d0 + b*x + c
    end function func

end module func_module


program asg4 
    use func_module
    implicit none 
    real(8) a, b, c, x1, x2, xm
    integer :: t, max_t = 1000

    a = 0.0d0
    b = -2.0d0
    c = 2.0d0
    !係数の指定

    write(*,*) "Input the start value x1 and end value x2"
    read(*,*) x1, x2 

    call bisection_method(a, b, c, x1, x2, t, xm)

    if (t == max_t) then
        write(*,*) "No solution was found."
    else
        write(*,*) "Solution :", xm
    end if
    write(*,*) 'Number of repetitions:', t
    write(*,*) 'Output file :"outasg4.d"'

end program asg4



!subroutineと　functionについて
!サブルーチンはイメージ的にはデータ、作業の一連操作を定義するもの
!function は数学の関数みたいに使うときに定義する




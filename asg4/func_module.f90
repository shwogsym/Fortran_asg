
module func_module
    implicit none
    contains 

    subroutine bisection_method(a, b, c, d, x1, x2, t, xm)
    implicit none 
 
    real(8), intent(in) :: a, b, c, d
    real(8) :: x1, x2, xm, xm0, fm, er = 1.0
    integer :: t, io

    integer, parameter :: output_file_number = 11, max_t = 1000
    real(8), parameter :: eps = 1.0e-15

    !２つの初期値が二分法を回す条件を満たしているのか検証
    if (function(a, b, c, d, x1) * function(a, b, c, d, x2) > 0) then
        stop 'One of the func value should be positive and the other negative as initial condition.'
    endif 
  

    t = 0 
    open(output_file_number, file = 'data.dat', status = 'replace', iostat = io)
    if (io /= 0) stop 'Failure to open output file'
 
    do while ( er > eps .and. t <= max_t) !最大回数に達するまで繰り返し
        xm = 0.5 * (x1 + x2)
        fm = function(a, b, c, d, xm)
        !中央値の計算

        !xm (中点)が 0のときは、それは正数であると扱われる。
        if (fm < 0) then
             !中点の値が0以下のとき→x1,x2の中負の値の方と値を交換したい。↓で判定
            if (function(a, b, c, d, x2) < 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        else 
            !else 中点の値が0より大きいとき同様に判定
            if (function(a, b, c, d, x2) > 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        endif
        !中点の値に応じて初期値の入れ替えを行い、次のサイクルに準備
        t = t + 1 
        write(output_file_number,*) t, xm 
        
        er = abs(xm0 - xm)/abs(xm0)
        xm0 = xm

    end do

    close(output_file_number)

    if (t == max_t) then
        write(*,*) "No solution was found."
    else
        write(*,*) "Solution              :", xm
    end if

    end subroutine bisection_method

    real(8) function function(a, b, c, d, x)
        implicit none 
        real(8) :: a, b, c, d, x
        function = a*x*x*x + b*x*x + c*x + d
    end function function

end module func_module
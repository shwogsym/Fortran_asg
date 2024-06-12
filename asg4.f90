!初期値入力型の二分法プログラム、係数は2次以下で任意
!出力過程をみて、xm =0 のときの挙動をしっかり把握したい。

module func_module
    implicit none
    contains 

    subroutine bisection_method(a, b, c, d, x1, x2, t, xm)
    implicit none 
 
    real(8), intent(in) :: a, b, c, d
    real(8) :: x1, x2, xm, fm
    integer :: t, io

    integer, parameter :: output_file_number = 11, max_t = 1000
    real(8), parameter :: er = 1.0e-15

    if (func(a, b, c, d, x1) * func(a, b, c, d, x2) > 0) then
        stop 'One of the values should be positive and the other negative.'
    endif 
    !２つの初期値が二分法を回す条件を満たしているのか検証

    t = 0 
    open(output_file_number, file = 'asg4_file/outasg4.dat',iostat = io)
    if (io /= 0) stop 'Failure to open output file'

    do while (abs(x2 - x1) > er .and. t <= max_t) 
        xm = 0.5 * (x1 + x2)
        fm = func(a, b, c, d, xm)
        !中央値の計算

        !xm (中点)が 0のときは、それは正数であると扱われる。
        if (fm < 0) then
             !中点の値が0以下のとき→x1,x2の中負の値の方と値を交換したい。↓で判定
            if (func(a, b, c, d, x2) < 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        else 
            !else 中点の値が0より大きいとき同様に判定
            if (func(a, b, c, d, x2) > 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        endif
        !中点の値に応じて初期値の入れ替えを行い、次のサイクルに準備
        t = t + 1 
        write(output_file_number,*) t, xm 
    end do
    close(output_file_number)

    if (t == max_t) then
        write(*,*) "No solution was found."
    else
        write(*,*) "Solution :", xm
    end if

    end subroutine bisection_method

    real(8) function func(a, b, c, d, x)
        implicit none 
        real(8) :: a, b, c, d, x
        func = a*x*x*x + b*x*x + c*x + d
    end function func

end module func_module

program asg4 
    use func_module
    implicit none 
    real(8) a, b, c, d, x1, x2, xm
    integer t, io
    integer, parameter :: input_file_number = 10, max_t = 1000

    open(input_file_number, file = 'asg4_file/inpasg4.dat', status='old', action='read', iostat=io)
    if (io /= 0) stop 'Filure to open input file.'

    read(input_file_number,*) a, b, c, d, x1, x2

     !不定、不能を弾く
    if (a == 0) then 
        if (b == 0) then 
            if (c == 0) then 
                if (d == 0) then 
                    stop 'Undeterminate' ! 0x = 0の場合であり、解が無数にある
                else 
                    stop 'Unsolvable' !0x + d = 0の場合であり、解が存在しない
                end if 
            end if 
        end if 
    end if 


    !二分法サブルーチンの呼び出し、解の出力も兼ねている。
    call bisection_method(a, b, c, d, x1, x2, t, xm)
    
    write(*,*) 'Number of repetitions:', t
    write(*,*) 'Output file :"outasg4.d"'

end program asg4






!走査型の二分法のプログラム
module func_module
    implicit none
    contains 
    
    !二分法の計算をするプログラムをここに
    subroutine bisection_method(a, b, c, x1, x2, t, xm,i)
        implicit none 
        real(8) :: a, b, c, x1, x2, xm, fm
        integer :: t, max_t = 1000, fi = 10, io,i
        real(8), parameter :: er = 1.0e-15
        character(len=20) :: filename

        if (func(a, b, c, x1) * func(a, b, c, x2) > 0) stop 'One of the values should be positive and the other negative.'
    
         ! 整数iを文字列に変換し、ファイル名を生成
        write(filename, '(A,I0,A)') 'outasg4_', i,'.d'
        ! ファイルを開く
        open(i+10, file=filename, iostat=io)
        if (io /= 0) stop 'Failure to read file'

        t = 0
        do while (abs(x2 - x1) > er .and. t < max_t) 
            xm = (x1 + x2) / 2.0
            fm = func(a, b, c, xm)

            if (fm < 0) then
                if (func(a, b, c, x2) < 0) then
                    x2 = xm
                else 
                    x1 = xm
                endif 
            else 
                if (func(a, b, c, x2) > 0) then
                    x2 = xm
                else 
                    x1 = xm
                endif
            endif

            t = t + 1
            write(i+10,*) t, xm
        end do

        close(fi)
    end subroutine bisection_method

    !使う関数の定義をここで
    real(8) function func(a, b, c, x)
        implicit none
        real(8) :: a, b, c, x
        func = a*x**2.0d0 + b*x + c
    end function func

    !与えられた範囲内で二分法の初期条件を満たす値を判定し、x1s,x2sという配列にそれぞれの値を格納する。
    subroutine hantei(a, b, c, f, l, x1s, x2s, count)
        implicit none 
        real(8) :: a, b, c, f, l, step, x1, x2
        integer :: i, count
        real(8), dimension(:), allocatable :: x1s, x2s
        real(8) :: test1, test2

        allocate(x1s(100))
        allocate(x2s(100))
        !一応 100分割だからこれで十分

        step = (abs(l - f)) / 100.0d0
        !一応100分割をしてるけど、関数や走査範囲により不都合が生じる可能性がある。

        x1 = f
        x2 = x1 + step
        count = 0

        do i = 1, 100
            test1 = func(a, b, c, x1)
            test2 = func(a, b, c, x2)
            if (test1 * test2 <= 0.0d0) then
                count = count + 1
                x1s(count) = x1
                x2s(count) = x2
                if (count == 100) exit
            endif
            x1 = x2
            x2 = x1 + step
        end do

        if (count == 0) stop 'No valid initial points found in the range.'
    end subroutine hantei

end module func_module


!mainプログラム
program asg4 
    use func_module
    implicit none 
    real(8) a, b, c, f, l
    real(8), allocatable :: x1s(:), x2s(:)
    real(8) xm
    integer :: t, i, count
    
    !二次関数の係数を指定
    a = 1.0d0
    b = 0.0d0
    c = -2.0d0

    write(*,*) "Input calculate range f and l"
    read(*,*) f, l

    call hantei(a, b, c, f, l, x1s, x2s, count)

    do i = 1, count
        call bisection_method(a, b, c, x1s(i), x2s(i), t, xm,i)
        if (t == 1000) then
            write(*,*) "No solution was found for interval ", i
        else
            write(*,*) "Solution in interval ", i, ":", xm
        endif
        write(*,*) 'Number of repetitions for interval ', i, ':', t
    end do
    write(*,*) 'Output file :"outasg4.d"'

end program asg4
        
!走査型の二分法のプログラム、任意範囲を任意回数で分割し、初期条件を探す。

!入出力ファイルはasg4_fileディレクトリ
!入力ファイルはinpasg4.dat
!出力ファイルはdata_01.dat, data_02.dat, ...となる。

module func_module
    implicit none
    contains 
    
    !二分法の計算、出力をするプログラム
    subroutine bisection_method(a, b, c, x1, x2, t, xm,i)
        implicit none 
        real(8) a, b, c, x1, x2, xm, fm
        integer t,io, i

        real(8), parameter :: er = 1.0e-15
        integer, parameter :: output_file_number = 10, max_t = 1000
        character(32)      :: filename
        
        if (func(a, b, c, x1) * func(a, b, c, x2) > 0) stop 'One of the values should be positive and the other negative.'
    
         ! iに応じたファイル名を生成
        write(filename, '("bisection_file/data_"i2.2".dat")') i
        ! ファイルを開く
        open(i+10, file=filename, status = "replace",iostat=io)
        if (io /= 0) stop 'Failure to open output file'

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
            write(i+10,'(I2.2, 2X, F24.16)') t, xm  !gnuplotでプロットする場合、02とかは構文エラー吐くから修正
        end do

        close(output_file_number)
    end subroutine bisection_method

    !使う関数の定義をここで
    real(8) function func(a, b, c, x)
        implicit none
        real(8) :: a, b, c, x
        func = a*x**2.0d0 + b*x + c
    end function func

    !与えられた範囲内で二分法の初期条件を満たす値を判定し、x1s,x2sという配列にそれぞれ初期値を格納する。
    subroutine hantei(a, b, c, f, l, x1s, x2s, count,split)
        implicit none 
        real(8) :: a, b, c, f, l, step, x1, x2
        integer :: i, count,split
        real(8), dimension(:), allocatable :: x1s, x2s
        !ディメンションを任意とした配列 x1s、x2sの定義
        real(8) :: test1, test2

        allocate(x1s(split))
        allocate(x2s(split))
        !一応 100分割だからこれで十分

        step = (abs(l - f)) / split  !'split'間隔で走査を行う。

        x1 = f
        x2 = x1 + step
        count = 0

        do i = 1, split
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
    integer t, i, count, split, io
    integer, parameter :: input_file_number = 11
    
    open(input_file_number, file = 'bisection_file/input.dat', status='old', action='read', iostat=io)
    if (io /= 0) stop 'Filure to open input file.'

    read(input_file_number,*) a,b,c,f,l,split

    call hantei(a, b, c, f, l, x1s, x2s, count,split)

    do i = 1, count
        call bisection_method(a, b, c, x1s(i), x2s(i), t, xm,i)
        if (t == 1000) then
            write(*,*) "No solution was found for interval ", i
        else
            write(*,*) "Solution in interval ", i, ":", xm
        endif
        write(*,*) 'Number of repetitions for interval ', i, ':', t
        write(*,*) 'Output file : "asg4_file/data_', i, '.dat"'
    end do

end program asg4
        
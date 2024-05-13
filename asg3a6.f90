!任意の三次関数解をニュートン法で求める
!虚数解に対応できてない

module func_module
    implicit none 
    
    contains 

    subroutine newton(a, b, c, d,x1,i,k)
        implicit none 
        real(8) a, b, c,d, x1, x2,f,df,er
        real(8) ,parameter :: er0=1.0d-15
        integer k,io,i
        integer ,parameter :: km = 100

        integer ,parameter :: output_file_number = 11
        character(32)      :: filename


        write(filename, '("asg3_6_file/data_"i2.2".dat")') i
        open(output_file_number, file= filename, action='write',iostat=io)
        if (io /= 0) stop 'Failure to read file'


        do k = 1,km 

            f = a*x1**3 + b*x1**2 + c*x1 + d            ! f(x)
            df = 3*a*x1**2 + 2*b*x1 + c                 ! f'(x)

            if (df == 0) then
                print *, 'Error, derivative is zero.'
                exit
            endif
            !導関数が0のとき、収束がないため

            x2 = x1 - f / df
            er = abs(x2 - x1)

            write (output_file_number,*) k,x2

            if (er < er0) exit
            x1 = x2
        enddo
        close (output_file_number) 
    end subroutine newton


    subroutine hantei(a,b,c,f,l,xs,count,split)
        implicit none 
        real(8) :: a, b, c,f, l, step,x1,x2
        integer :: i, count,split
        real(8), dimension(:), allocatable :: xs
        !ディメンションを任意とした配列 
        real(8) :: test1, test2

        allocate(xs(split))
        !最大分の配列を作っとく

        step = (abs(l - f)) / split
        
        xs(1) = f
        x1 = f
        x2 = x1 + step
        count = 1

        do i = 1, split
            test1 = 3*a*x1**2 + 2*b*x1 + c    
            test2 = 3*a*x2**2 + 2*b*x2 + c    
            if (test1 * test2 <= 0.0d0) then
                if (x2 /= 0) then
                    count = count + 1
                    xs(count) = x2
                endif    
                if (count == 100) exit
            endif
            x1 = x2
            x2 = x1 + step
        end do

    end subroutine hantei


end module func_module



program asg3_6 
    use func_module
    implicit none

    real (8) :: a,b,c,d,f,l
    integer :: fi = 10,io,i,count,split,k
    real(8) ,allocatable :: xs(:)

    open(fi, file='asg3_6_file/inpasg3a6.d', action='read', iostat=io)
    if (io /= 0) stop 'Filure to open file.'
    !ファイル読み込みに失敗→終了

    read(fi,*) a,b,c,d 
    !a,b,c,dを３次関数の係数としてそれぞれファイルに入力
    close(fi)

    write(*,*) "Input calculate range f and l (f < range < l)"
    read(*,*) f, l

    write(*,*) "Input split time"
    read(*,*) split

    call hantei(a,b,c,f,l,xs,count,split)

    do i = 1,count
        call newton(a, b, c, d, xs(i),i,k)
        write(*,*) "Solution in interval               ", i, ":", xs(i)
        write(*,*) 'Number of repetitions for interval ', i, ':', k
        write(*,*) 'Output file : "asg4_file/data_', i, '.dat"'
    enddo 

    write(*,*) count

end program asg3_6



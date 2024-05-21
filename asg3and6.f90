!任意区間での任意の三次関数解をでニュートン法で求めるプログラム
!0付近での動作が不安定 →　プログラムが間違ってるわけではなくて、局地付近で値が飛んでるだけ。実際に初期値を確認すると±0.5 くらいのときに、0収束判定が出ない
!初期値として選択されたものを画面出力に出すといいかもしれない。

module func_module
    implicit none 
    contains 

    subroutine newton(a,b,c,d,x1,i,k)
        implicit none 
        real(8) a, b, c, d, x1, x2, f, df, er
        real(8) ,parameter :: er0=1.0d-15
        integer k, io, i
        integer ,parameter :: km = 100

        integer ,parameter :: output_file_number = 11
        character(32)      :: filename

        write(filename, '("asg3_6_file/data_"i2.2".dat")') i
        open(output_file_number, file= filename, status='replace', action='write',iostat=io)
        if (io /= 0) stop 'Failure to open output file'

        write (output_file_number,'(I2.2, 2X, F24.16)') 0,x1 !初期値の記録

        do k = 1,km 
            f = a*x1**3 + b*x1**2 + c*x1 + d
            df = 3*a*x1**2 + 2*b*x1 + c 

            if (df == 0) then
                write(*,*) 'Error, derivative is zero.'
                exit
            endif
            !導関数が0のとき、収束がないため

            x2 = x1 - f / df
            er = abs(x2 - x1)

            write (output_file_number,'(I2.2, 2X, F24.16)') k,x2

            if (er < er0) exit
            x1 = x2
        enddo
        close (output_file_number) 
    end subroutine newton


    subroutine hantei(a,b,c,f,l,xs,count,split)
        implicit none 
        real(8) :: a, b, c,f, l, step, x1, x2
        integer :: i, count, split
        real(8), allocatable :: xs(:)
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

    real (8) a,b,c,d,f,l
    integer  io,i,count,split,k
    real(8) ,allocatable :: xs(:)
    integer :: input_file_number = 10

    open(input_file_number, file='asg3_6_file/inpasg3_6.dat', status='old',action='read', iostat=io)
    if (io /= 0) stop 'Filure to open inout file.'

    read(input_file_number,*) a, b, c, d, f, l, split
    !a,b,c,dを３次関数の係数としてそれぞれファイルに入力　"Input calculate range f and l (f < range < l)" and split time
    close(input_file_number)

    call hantei(a,b,c,f,l,xs,count,split)

    do i = 1,count
        call newton(a, b, c, d, xs(i),i,k)
        write(*,*) "Solution in interval               ", i, ":", xs(i)
        write(*,*) 'Number of repetitions for interval ', i, ':', k
        write(*,*) 'Output file : "asg4_file/data_', i, '.dat"'
    enddo 


end program asg3_6



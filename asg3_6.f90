!初期値を入力するタイプのニュートン法、関数は三次以下で任意

module func_module
    implicit none 
    
    contains 

    subroutine newton(a, b, c, d, x1, k)
        implicit none 
        real(8) a, b, c,d, x1, x2,f,df,er
        integer k,io

        real(8) ,parameter :: er0=1.0d-15
        integer ,parameter :: k_max = 100, output_file_number = 11
        character (32) filename

        write(filename, '("asg3_6_file/data.dat")') 
        open(output_file_number, file= filename, action='write',iostat=io)
        if (io /= 0) stop 'Failure to open output file'

        write (output_file_number,'(I2.2, 2X, F24.16)') 0,x1 !初期値の記録
        
        do k = 1,k_max 

            f = a*x1**3.0d0 + b*x1**2.0d0 + c*x1 + d            ! f(x)
            df = 3.0d0*a*x1**2.0d0 + 2.0d0*b*x1 + c                 ! f'(x)

            if (df == 0) then
                print *, 'Error, derivative is zero.'
                exit
            endif
            !導関数が0のとき収束がない

            x2 = x1 - f / df
            er = abs(x2 - x1)

            write (output_file_number,'(I2.2, 2X, F24.16)') k,x2

            if (er < er0) exit
            x1 = x2
        enddo

        close (output_file_number) 
    end subroutine newton

end module func_module


program asg6 
    use func_module
    implicit none

    real (8) a,b,c,d,xs
    integer io,k

    integer ,parameter   :: input_file_number = 10

    open(input_file_number, file='asg3_6_file/inpasg3_6.dat', action='read', iostat=io)
    if (io /= 0) stop 'Filure to read input file.'
    !ファイル読み込みに失敗→終了

    read(input_file_number,*) a,b,c,d,xs
    !a,b,c,dを３次関数の係数と初期値
    close(input_file_number)

    call newton(a, b, c, d, xs, k)
        
    write(*,*) "Solution                     :", xs
    write(*,*) 'Number of repetitions :',k
    write(*,*) 'Output file : "asg3_6_file/data.dat"'

end program asg6



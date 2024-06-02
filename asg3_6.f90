!初期値を入力するタイプのニュートン法、関数は三次以下で任意

module func_module
    implicit none 
    contains 

    subroutine newton(a, b, c, d, x1, k)
        implicit none 

        real(8) ,intent(in)    :: a, b, c, d
        real(8) ,intent(inout) :: x1
        integer ,intent(out)   :: k

        real(8)  x2, f, df, er
        integer  io

        real(8) ,parameter :: er0=1.0d-15, near0 = 1.0d-13 
        integer ,parameter :: k_max = 100, output_file_number = 11

        character (32) filename

        !出力ファイルのオープン
        write(filename, '("asg3_6_file/data.dat")') 
        open(output_file_number, file = filename, status = 'replace' ,action='write', iostat=io)
        if (io /= 0) stop 'Failure to open output file'
        
        !初期値を記録
        write (output_file_number,'(I2.2, 2X, d24.16)') 0, x1 
        
        !ニュートン法のループを回す
        do k = 1, k_max 

            f = a*x1**3 + b*x1**2 + c*x1 + d            ! f(x)
            df = 3.0d0*a*x1**2 + 2.0d0*b*x1 + c         ! f'(x)

            if ( abs(df) < near0 ) then  !導関数が0の時に収束が起きないため、エラーを出す
                write(*,*) 'Error, derivative is zero.'
                exit 
            end if 

            x2 = x1 - f / df
            er = abs(x2 - x1)

            write (output_file_number,'(I2.2, 2X, d24.16)') k, x2

            if (er < er0) exit
            x1 = x2
        enddo

        close (output_file_number) 
    end subroutine newton

end module func_module


program asg6 
    use func_module
    implicit none

    real (8) a, b, c, d, x_sol, f
    integer io, k

    integer ,parameter :: input_file_number = 10

    open(input_file_number, file='asg3_6_file/inpasg3_6.dat', action='read', iostat=io)
    if (io /= 0) stop 'Filure to read input file.'
    !ファイル読み込みに失敗→終了
    read(input_file_number,*) a, b, c, d, x_sol
    !a,b,c,dの３次関数の係数と初期値を読み込む
    close(input_file_number)

    !ニュートン法サブルーチンの呼び出し
    call newton(a, b, c, d, x_sol, k)

    write(*,'(A, d24.16)') 'Solution              :', x_sol
    write(*,'(A, I5)') 'Number of repetitions :', k
    write(*,'(A)') 'Output file           :  asg3_6_file/data.dat'

    f = a*x_sol**3 + b*x_sol**2 + c*x_sol + d  
    !解の妥当性を確認
    write(*,'(A, d24.16)') 'Funcvalue at solution :', f

end program asg6


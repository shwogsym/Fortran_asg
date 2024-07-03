!ニュートン法による収束計算、 -2から2まで0.01刻みで初期値を変えて計算を行う
!出力は、反復回数、収束先、初期値となる

!初期値に依存して収束速度や収束先が変わることを確認できる

module func_module
    implicit none 
    contains 

    subroutine newton(a, b, c, d, x1, k)
        implicit none 

        real(8) ,intent(in)    :: a, b, c, d
        real(8) ,intent(inout) :: x1
        integer ,intent(out)   :: k

        real(8)  x_sol, f, df, er, initial_value

        real(8) ,parameter :: er0=1.0d-16, near0 = 1.0d-20
        integer ,parameter :: k_max = 100, output_file_number = 11

        initial_value = x1 !初期値を記録
        
        !ニュートン法のループを回す
        do k = 1, k_max 

            f = a*x1**3 + b*x1**2 + c*x1 + d            ! f(x)
            df = 3.0d0*a*x1**2 + 2.0d0*b*x1 + c         ! f'(x)

            if ( abs(df) < near0 ) then  !導関数が0の時に収束が起きないため、エラーを出す
                write(*,*) 'Error, derivative is zero.'
                exit 
            end if 
            
            x_sol = x1 - f / df
            er = abs(x_sol - x1)

            if (er < er0) exit
            x1 = x_sol
        enddo

        write (output_file_number,'(I2, d24.16, d24.16)') k, x_sol, initial_value
    end subroutine newton

end module func_module



program asg6 
    use func_module
    implicit none

    real (8) a,b,c,d,xs
    integer i,io,k
    character (32) filename
    
    integer ,parameter   ::  output_file_number = 11

    a = 1.0d0 
    b = 0.0d0
    c = -1.0d0
    d = 0.0d0

    write(filename, '("loopdata.dat")') 
    open(output_file_number, file = filename, status = 'replace' ,action='write', iostat=io)
    if (io /= 0) stop 'Failure to open output file'
    
    !刻み収束計算を行う
    do i = -2000,2000,1 
        xs = i / 1000.0d0
        call newton(a, b, c, d, xs, k)
    enddo 

    close (output_file_number) 
    !ファイルioをサブルーチン内で行ってたんだけど、データが逐一書き換えられてしまうので、main プログラムに移動した

end program asg6



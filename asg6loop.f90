! -2から2まで0.01刻みで初期値を走査

module func_module
    implicit none 
    
    contains 

    subroutine newton(a, b, c, d, x1, k)
        implicit none 
        real(8) a, b, c,d, x1, x2,f,df,er,t
        integer k,io

        real(8) ,parameter :: er0=1.0d-15
        integer ,parameter :: k_max = 100, output_file_number = 11
        character (32) :: filename = ("asg6_file/loopdata.dat")

        t = x1

        open(output_file_number, file= filename, action='write',position = 'append',iostat=io)
        if (io /= 0) stop 'Failure to read file'

        do k = 1,k_max 

            f = a*x1**3 + b*x1**2 + c*x1 + d            ! f(x)
            df = 3*a*x1**2 + 2*b*x1 + c                 ! f'(x)

            if (df == 0) then
                print *, 'Error, derivative is zero.'
                exit
            endif
            !導関数が0のとき収束がない
            x2 = x1 - f / df
            er = abs(x2 - x1)

            if (er < er0) exit
            x1 = x2
        enddo

        write (output_file_number,'(I3, F24.16 ,F24.16)') k,x2,t

        close (output_file_number) 
    end subroutine newton

end module func_module



program asg6 
    use func_module
    implicit none

    real (8) a,b,c,d,xs
    integer i,io,k

    integer ,parameter   :: input_file_number = 10

    open(input_file_number, file='asg6_file/inpasg6.dat', action='read', iostat=io)
    if (io /= 0) stop 'Filure to open input file.'
    !ファイル読み込みに失敗→終了

    read(input_file_number,*) a,b,c,d
    !a,b,c,dを３次関数の係数と初期値
    close(input_file_number)
    
    do i = -200,200,1
        xs = i / 100.0d0
        call newton(a, b, c, d, xs, k)
    enddo 

end program asg6



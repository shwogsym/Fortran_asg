module func_module
    implicit none
    contains 

    subroutine newton(a,b,c,x1,k)
        implicit none 

        real (8) :: a,b,c,x1,x2,er,f,df
        integer  :: k,k_max = 1000
        
        real (8) ,parameter :: er0 = 1.0d-15
        integer  ,parameter :: output_file_number = 11

        k = 0
        er = 1
        do while (er > er0 .and. k < k_max) 

            f  = a*x1**2 + b*x1 + c           
            df = 2*a*x1 + b   
    
            if (df == 0) then
                write(*,*) 'Error, derivative is zero.'
                exit
            endif

            x2 = x1 - f / df
            er = abs(x2 - x1)

            write (output_file_number,'(I2.2, 2X, F24.16)') k,x2
            x1 = x2

            k = k + 1
        enddo
    end subroutine newton

    subroutine bisection_method(a, b, c, x1, x2, k, xm)
        implicit none 
        real(8) :: a, b, c, x1, x2, xm, fm
        integer :: k, k_max = 1000, output_file_number = 12
        real(8), parameter :: er = 1.0e-15
    
        if (func(a, b, c, x1) * func(a, b, c, x2) > 0) then
            stop 'One of the values should be positive and the other negative.'
        endif 
        
        k = 0 
        do while (abs(x2 - x1) > er .and. k < k_max) 
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
            
            k = k + 1 
            write(output_file_number,'(I2.2, 2X, F24.16)') k, xm 
        end do
    end subroutine bisection_method
    
    real(8) function func(a, b, c, x)      
    implicit none     
    real(8) :: a, b, c, x    
    func = a*x**2.0d0 + b*x + c
end function func

end module func_module


program asg5_1 
    use func_module
    implicit none

    real (8) :: a,b,c,xn,x1,x2,xm
    integer  :: k,max_t = 1000,io
    
    real (8) ,parameter :: er0=1.0d-15
    integer  ,parameter :: k_max = 1000, input_file_number = 10, output_file_number = 11


    open(input_file_number, file='asg5_file/inpasg5.dat', action='read', iostat=io)
    if (io /= 0) stop 'Filure to open input file.'

    read(input_file_number,*) a,b,c,xn,x1,x2 !この順でinputfileを入力

    close (input_file_number)

    write(*,*) 'The magnitude of the Error :',er0

    write(*,*) 'Newtons method'

    open(output_file_number, file='asg5_file/outasg5_1.dat',action = 'write',iostat = io)
    if (io /= 0) stop 'Failure to open output file'
    call newton(a,b,c,xn,k)

    if (k == max_t) then
        write(*,*) "No solution was found."
    else
        write(*,*) "Solution :", xn
    end if
    write(*,*) 'Number of repetitions:', k
    write(*,*) 'Output file :"outasg5_1.dat"'
    close (output_file_number)

    write(*,*) ''
    write(*,*) 'Bisection method'

    open(output_file_number+1, file='asg5_file/outasg5_2.dat',action = 'write',iostat = io)
    if (io /= 0) stop 'Failure to optn output file'

    call bisection_method(a, b, c, x1, x2, k, xm)

    if (k == max_t) then
        write(*,*) "No solution was found."
    else
        write(*,*) "Solution :", xm
    end if
    write(*,*) 'Number of repetitions:', k
    write(*,*) 'Output file :"outasg5_2.dat"'


    close (output_file_number+1)

end program asg5_1 




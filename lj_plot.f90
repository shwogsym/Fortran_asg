program ljplot
    implicit none 
    integer :: i 
    real(8) x, y
    integer ,parameter :: output_file_number = 11
    real(8) ,parameter ::  sigma = 2.4945, eps = 0.87381d-2

    x = 1.0d0
    open(output_file_number, file='data.dat', status='replace')
    do i = 1, 200
        x = x + 0.0001d0*i
        y = 4.0d0 * eps * ( (sigma/x)**12 - (sigma/x)**6 ) 
        write(output_file_number,*) x, y
    end do
    close(output_file_number)
end program ljplot
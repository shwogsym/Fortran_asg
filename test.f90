module func_module
    implicit none
    contains 

    subroutine bisection_method(a, b, c, x1, x2, t, xm)
        implicit none 
        real(8) :: a, b, c, x1, x2, xm, fm
        integer :: t, max_t = 1000, fi = 10, io
        real(8), parameter :: er = 1.0e-6

        if (func(a, b, c, x1) * func(a, b, c, x2) > 0) then
            stop 'One of the values should be positive and the other negative.'
        endif 

        t = 0 
        open(fi, file = 'outasg4.d', iostat = io)
        if (io /= 0) stop 'Failure to read file'

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
            write(fi,*) t, xm 
        end do

        close(fi)
    end subroutine bisection_method

    real(8) function func(a, b, c, x)
        implicit none 
        real(8) :: a, b, c, x
        func = a*x**2.0d0 + b*x + c
    end function func

    subroutine hantei(a, b, c, f, l, x1s, x2s, count)
        implicit none 
        real(8) :: a, b, c, f, l, step, x1, x2
        integer :: i, count
        real(8), dimension(:), allocatable :: x1s, x2s
        real(8) :: test1, test2

        allocate(x1s(100))
        allocate(x2s(100))
        step = (l - f) / 100.0d0
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

        if (count == 0) then
            stop 'No valid initial points found in the range.'
        endif
    end subroutine hantei

end module func_module

program asg4 
    use func_module
    implicit none 
    real(8) a, b, c, f, l
    real(8), allocatable :: x1s(:), x2s(:)
    real(8) xm
    integer :: t, i, count

    a = 0.0d0
    b = -2.0d0
    c = 2.0d0

    write(*,*) "Input calculate range f and l"
    read(*,*) f, l

    call hantei(a, b, c, f, l, x1s, x2s, count)

    do i = 1, count
        call bisection_method(a, b, c, x1s(i), x2s(i), t, xm)
        if (t == 1000) then
            write(*,*) "No solution was found for interval ", i
        else
            write(*,*) "Solution in interval ", i, ":", xm
        endif
        write(*,*) 'Number of repetitions for interval ', i, ':', t
    end do
    write(*,*) 'Output file :"outasg4.d"'

end program asg4
        
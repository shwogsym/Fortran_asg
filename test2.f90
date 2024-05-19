program read_matrix
    implicit none
    integer, parameter :: max_rows = 100, max_cols = 100
    real :: matrix(max_rows, max_cols)
    integer :: i, j, n, m
    character(len=100) :: line
    character(len=100), dimension(max_rows) :: lines
    integer :: num_lines, num_columns
    logical :: first_line

    ! Initialize
    num_lines = 0
    first_line = .true.

    ! Open the input file
    open(unit=10, file='asg2_file/inpasg2.dat', status='old', action='read')

    ! Read the file line by line
    do i = 1, max_rows
        read(10, '(A)', iostat=j) line
        if (j /= 0) exit
        num_lines = num_lines + 1
        lines(num_lines) = line

        if (first_line) then
            ! Assuming one space between numbers, count spaces and add 1 for the number of columns
            num_columns = count([(line(i:i) == ' ', i=1, len_trim(line))]) + 1
            first_line = .false.
        end if

    end do

    close(10)

    ! Determine the size of the matrix
    n = num_lines
    m = num_columns

    ! Read the matrix values
    do i = 1, n
        read(lines(i), *) (matrix(i, j), j = 1, m)
    end do

    ! Print the matrix size
    print *, 'Matrix size: ', n, ' x ', m

    ! Print the matrix
    do i = 1, n
        do j = 1, m
            write(*, '(F6.2)', advance='no') matrix(i, j)
        end do
        print *
    end do

end program read_matrix

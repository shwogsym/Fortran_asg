!データ修正の演算をここで定義
module func_module
    implicit none   
    contains

    subroutine Fix_data(x)
        implicit none 
        real(8)x
        x=x-0.01d0
    end subroutine Fix_data
 end module func_module


program read_data
    use func_module
    implicit none
   
    integer :: i, j, io
    integer, parameter :: max_rows = 100, max_cols = 100
    integer :: size_lines, size_columns = 2, input_file_number = 10, output_file_number = 11
    
    real(8) :: line(max_cols) 
    real(8), dimension(max_rows, max_cols) :: lines 

    size_lines = 0
  
    open(input_file_number, file='asg2_file/inpasg2.dat', status='old', action='read',iostat = io)
    if (io /= 0) stop 'Failure to open input file'
    open(output_file_number, file='output.dat', status='replace', action='write',iostat = io)
    if (io /= 0) stop 'Failure to open output file'

    !データの読み込み
    do i = 1, max_rows
        read(input_file_number, *, iostat=io) (line(j), j = 1, size_columns) !1列分のデータの読み込み
        if(io /= 0) exit !データ行の最終行を検出
        size_lines = size_lines + 1

        do j = 1, size_columns
            lines(i, j) = line(j) !1列分　(line)のデータを linesに格納
        end do
    end do
    close(input_file_number)

    !2列目のデータについてデータの修正を行う
    do i = 1,size_lines
        call Fix_data(lines(i,2))
    enddo 

    do i = 1, size_lines
        do j = 1, size_columns
            write(output_file_number, '(F24.16)', advance='no') lines(i, j)
            ! advance = no →　出力時に改行しない
        end do
        write(output_file_number, *) !ファイル内での改行
    end do
    close(output_file_number) 

end program read_data

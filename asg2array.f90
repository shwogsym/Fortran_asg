
!このモジュール内には、データの修正を行うサブルーチンが含まれている。

module func_module
    implicit none  
    contains
 
    subroutine Fix_data(r)
        implicit none 
        real(8) r
        r = r - 0.01d0
    end subroutine Fix_data
    !値を-2乗するサブルーチン
    subroutine Fix_data2(r)
        implicit none 
        real(8) r
        r = r**(-2)
    end subroutine Fix_data2
end module func_module

 
 program read_data
    use func_module
    implicit none
   
    integer :: i, j, io
    integer, parameter :: max_rows = 100, max_cols = 100
    integer :: size_lines, size_columns = 2, Fix_colum_number = 2 ! Size_colums→　データの行数を指定、Fix_colum_number →修正したいデータ列の番号 、データの行数については動的に決定される。
    integer :: input_file_number = 10, output_file_number = 11
    
    real(8) :: line(max_cols) 
    real(8), dimension(max_rows, max_cols) :: lines 
 
    size_lines = 0
  
    open(input_file_number, file='asg2_file/inpasg2.dat', status='old', action='read',iostat = io)
    if (io /= 0) stop 'Failure to open input file'
    open(output_file_number, file='asg2_file/outasg2.dat', status='replace', action='write',iostat = io)
    if (io /= 0) stop 'Failure to open output file'
 
    !データの読み込み
    do i = 1, max_rows
        read(input_file_number, *, iostat=io) (line(j), j = 1, size_columns) !1行分のデータの読み込み
        if(io /= 0) exit !データのデータ端を検出
        size_lines = size_lines + 1 !データの行数をカウント
 
        do j = 1, size_columns
            lines(i, j) = line(j) !1行分　(line)のデータを linesに格納
        end do
    end do
    close(input_file_number)
 
    !Fix_colum_number 列目のデータについてデータの修正を行う
    do i = 1,size_lines
        call Fix_data(lines(i,Fix_colum_number))
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
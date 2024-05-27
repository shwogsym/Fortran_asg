!データ修正の演算をここで定義
!修正後データを最も右の列に出力したい

module func_module
    implicit none  
    contains
 
    subroutine Fix_data(r)
        implicit none 
        real(8) r
        r = r - 0.01d0
    end subroutine Fix_data

    subroutine calc_data(x) !なにかおもしろい演算でも考えたら、
        implicit none 
        real(8)x
        x=x+0.01d0
    end subroutine calc_data

 end module func_module
 

 program read_data
    use func_module
    implicit none
   
    integer :: i, j, io
    integer, parameter :: max_rows = 100, max_cols = 100
    integer :: size_lines, size_columns = 2
    integer :: input_file_number = 10, output_file_number = 11
    integer :: Fix_colum_number = 2

     character(32) :: filename
    
    real(8) :: line(max_cols) 

 
    size_lines = 0
  
    open(input_file_number, file='asg2_file/inpasg2.dat', status='old', action='read',iostat = io)
    if (io /= 0) stop 'Failure to open input file'
    open(output_file_number, file='asg2_file/outasg2.dat', status='replace', action='write',iostat = io)
    if (io /= 0) stop 'Failure to open output file'
 
    !データの読み込み
    do i = 1, max_rows
        read(input_file_number, *, iostat=io) (line(j), j = 1, size_columns) !1行分のデータの読み込み
        if(io /= 0) exit !データのデータ端を検出

        write(filename, '("asg2_file/"i2.2".dat")') i
        open(i+output_file_number, file=filename, status = "replace",iostat=io)
        if (io /= 0) stop 'Failure to open output file'
        do j = 1, size_columns
            if(j == Fix_colum_number) then 
                call Fix_data(line(j))
                write(i+output_file_number,'(F24.16)') line(j)
            else
            write(i+output_file_number,'(F24.16)') line(j)
            endif
        enddo
        close(i+output_file_number)
    enddo
 

 end program read_data
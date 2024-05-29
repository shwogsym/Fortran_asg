module func_module
    implicit none  
    contains

    ! バックグラウンド値の引き算
    subroutine Fix_data(r)
        implicit none 
        real(8) :: r
        r = r - 0.01d0
    end subroutine Fix_data

    ! 最大値取得サブルーチン
    subroutine max_value(size_lines, max_x, max_n)
        implicit none
        integer :: i, io, size_lines
        real(8) :: x, n, max_x, max_n
        integer :: edit_file_number = 100
        character(32) :: filename

        do i = 1, size_lines
            write(filename, '("asg2v2_file/"i2.2".dat")') i
            open(i + edit_file_number, file=filename, status = "old", iostat=io)
            read(i + edit_file_number, *) n, x
            close(i + edit_file_number)

            if (i == 1) then 
                max_x = x    
                max_n = n
            else
                if (x > max_x) then
                    max_x = x
                    max_n = n
                endif
            endif
        enddo
    end subroutine max_value
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
    real(8) :: line(max_cols), max_x, max_n

    size_lines = 0

    open(input_file_number, file='asg2v2_file/inpasg2.dat', status='old', action='read', iostat=io)
    if (io /= 0) stop 'Failure to open input file'

    ! データの読み込みと処理
    do i = 1, max_rows
        read(input_file_number, *, iostat=io) (line(j), j = 1, size_columns)
        if (io /= 0) exit

        ! データ修正
        call Fix_data(line(Fix_colum_number))

        ! 修正データの出力
        write(filename, '("asg2v2_file/"i2.2".dat")') i
        open(i + output_file_number, file=filename, status="replace", iostat=io)
        if (io /= 0) stop 'Failure to open output file'
        write(i + output_file_number, '(F24.16, 1X, F24.16)') line(1), line(2)
        close(i + output_file_number)

        size_lines = size_lines + 1
    enddo
    close(input_file_number)

    ! 最大値の取得
    call max_value(size_lines, max_x, max_n)
    write(*, '(A, F24.16)') "Max value found in file: ", max_n
    write(*, '(A, F24.16)') "Max value: ", max_x

end program read_data


! 改善ポイントの詳細
! データの一括読み込みと処理:

! データの読み込みと修正を一度に行い、修正後のデータを一括で書き出すようにしました。
! ファイルI/Oの削減:

! 各ファイルの読み書き操作を最小限にし、必要なときにだけファイルを開閉するようにしました。
! コードの簡素化:

! 重複するコードを整理し、読みやすさと保守性を向上させました。
! これにより、処理速度の向上とコードの可読性、保守性が向上するはずです





!正直、確かに、シンプルにはされてるけど、汎用性が落ちれるから却下
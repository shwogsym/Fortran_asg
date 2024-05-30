!データ演算系のモジュール
module func_module
    implicit none  
    contains

    !バックグラウンド値の引き算サブルーチン
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


!データ取得系のモジュール
module max_module
    implicit none
    contains
     !最大値取得サブルーチン
    subroutine max_value(size_lines, max_x, max_filename)
        !サイズラインは、ファイルの数を示し、max_xは最大値、max_filenameは最大値があるファイル名とする。
        implicit none
        integer :: i, io, size_lines
        real(8) :: x, n, max_x
        integer :: edit_file_number = 110
        character(32) :: filename, max_filename

        do i = 1, size_lines

            !連番ファイル番号を用いて、ファイルネームを書き込む
            write(filename, '("asg2_file/"i2.2".dat")') i
            open(i + edit_file_number, file=filename, status = "old",iostat=io)
            !中の値を読む
            read(i + edit_file_number, *) n, x
            close(i + edit_file_number)

            !初期値を基準に判定を行う、単純な値の入れ替え
            if (i == 1) then 
                max_x = x    
                max_filename = filename
            else
                if (x > max_x) then
                    max_x = x
                    max_filename = filename
                end if
            end if
        end do
    end subroutine max_value


    !最大値周りのデータを取得するサブルーチン
    subroutine get_max_around_data(max_filename, size_lines) !←問題出てるけど、コメントアウト部分で使うので無視
        implicit none
        character(32) :: max_filename,filename
        character(2) :: file_num
        integer :: i, io, max_file_data_number, size_lines
        integer ,parameter :: data_r = 2, edit_file_number = 110, max_file_number = 210
        real(8) :: x, n
                
        ! max_filenameからファイル番号を抽出し、整数に変換する

        ! do i = 1, size_lines
        !     write(filename, '("asg2_file/"i2.2".dat")') i
        !     if (filename == max_filename) then 
        !         max_file_data_number = i
        !         exit
        !     end if
        ! end do

        ! file_num = max_filename(1:2)
        ! read(file_num, '(I2)') max_file_data_number


        file_num = max_filename(11:12)
        read(file_num, '(I2)', iostat=io) max_file_data_number

        if (io/= 0) then
            write(*,*) 'Error: Failed to convert file number from filename.'
            stop
        endif

        do i = max_file_data_number - data_r, max_file_data_number + data_r
            if (i > 0) then 
                write(filename, '("asg2_file/"i2.2".dat")') i
                open(i + edit_file_number, file=filename, status = "old", iostat=io)

                if (io /= 0) then
                    write(*,*)  'Failure to open output file for get_max_around_data'
                end if
                read(i + edit_file_number, *) n, x                
            end if 

            !同じデータをasg2v2_fileに書き込む
            write(filename, '("asg2_out/"i2.2".dat")') i
            open(i + max_file_number, file=filename, status = "replace", iostat=io)
            if (io /= 0) stop 'Failure to open output file for get_max_around_data'
            write(i + max_file_number, '(2F24.16)') n, x
            close(i + max_file_number)
        end do 
    end subroutine get_max_around_data
end module max_module



!メインプログラム
 program asg2
    use func_module
    use max_module
    implicit none
   
    integer :: i, j, io
    integer, parameter :: max_rows = 99, max_cols = 99
    integer :: size_lines, size_columns = 2,Fix_colum_number = 2 !inputの列数、演算を行う列、の指定ここでは題意通りの2列とする
    integer :: input_file_number = 10, output_file_number = 11

    character(32) :: filename,max_filename
    real(8) :: line(max_cols), max_x

    size_lines = 0
  
    !インプットファイルのオープン
    open(input_file_number, file='asg2_file/inpasg2.dat', status='old', action='read',iostat = io)
    if (io /= 0) stop 'Failure to open input file'


    !データの読み込み、修正を行う
    !rowの数は最大値を99として、それ以内で任意、列の数はsize_colums = 2 として大きさに応じてここを指定することで変更可能
    do i = 1, max_rows
        read(input_file_number, *, iostat=io) (line(j), j = 1, size_columns) !1行分のデータの読み込み
        if(io /= 0) exit !データ端を検出
        
        !連番のファイルをライトする(福島さんのナス参考)
        write(filename, '("asg2_file/"i2.2".dat")') i
        open(i+output_file_number, file=filename, status = "replace", iostat = io)
        if (io /= 0) stop 'Failure to open output file'

        !これは、全てのデータについて、Fix_column_number列目のデータを修正、した後に、各行を連番ファイルに出力
        do j = 1, size_columns
            if(j == Fix_colum_number) then 
                call Fix_data(line(j))
                write(i+output_file_number,'(F24.16)', advance = 'no') line(j)
            else
                write(i+output_file_number,'(F24.16)', advance = 'no') line(j)
            endif
        end do

        !改行を入れる
        write(i+output_file_number,*) 

        close(i+output_file_number)
        !行数のカウント
        size_lines = size_lines + 1

    end do
    close(input_file_number)


    !最大値取得サブルーチンの呼び出し
    call max_value(size_lines, max_x, max_filename)
    write(*, '(A, F24.16)') "Max value    : ",max_x
    write(*, '(A, A)') "Found in file: ", max_filename

    !最大値周りのデータを取得するサブルーチンの呼び出し
    call get_max_around_data(max_filename, size_lines)
    write(*,*) 'Data around the maximum value is stored in the asg2_out folder.'
 end program asg2



 





  !どうすればいいんだろうか、指定の行っていうのは、inputファイルのナンバリングとしてある nを参照すればいいのか、
    !それとも、連番ファイルの番号を参照したほうがいいのか

    !→　結論、xデータの方を参照して、行数を出力するようにする。

    !実際にプログラミングで使うのは、データ列の中で、最大値、または最小値を参照することが多い
    !つまり、00~01.datのデータファイルの中で、最大値、最小値を求めるプログラミむを作りたい。

    !最高の目的
    !任意の四則演算を何種類か作る。で上記の取得方法で様々な値が取り出せるような漢字にできたら最高
    !例えば最大値をとりだす、取得を行う時、演算1,2で異なる値を取り出すことができるようにする。
    !これは実際に力を見たい時や、エネルギーを見たいときに大応する。

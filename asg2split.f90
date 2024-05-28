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

        !これは、全てのデータについて、Fix_column_number列目のデータを修正する、プログラム
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

    close(input_file_number)

    !どうすればいいんだろうか、指定の行っていうのは、inputファイルのナンバリングとしてある nを参照すればいいのか、
    !それとも、連番ファイルの番号を参照したほうがいいのか

    !→　結論、xデータの方を参照して、行数を出力するようにする。

    !実際にプログラミングで使うのは、データ列の中で、最大値、または最小値を参照することが多い
    !つまり、00~01.datのデータファイルの中で、最大値、最小値を求めるプログラミむを作りたい。

    !最高の目的
    !任意の四則演算を何種類か作る。で上記の取得方法で様々な値が取り出せるような漢字にできたら最高
    !例えば最大値をとりだす、取得を行う時、演算1,2で異なる値を取り出すことができるようにする。
    !これは実際に力を見たい時や、エネルギーを見たいときに大応する。

    !2番目さいだいとかもだしたり、ソートとかもしてみたいかなー



 

 end program read_data
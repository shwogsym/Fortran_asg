program asg2 
    implicit none
    integer :: t, fi = 20, fo = 21,io
    real(8) :: i(10), x(10)  ! 変数の型宣言に :: を使用して明確にする
    character(len=100) :: filename1, filename2

    filename1 = 'inpasg2.d'
    filename2 = 'outasg2.d'

    ! 入力ファイルを開く
    open(unit=fi, file=filename1, action="read", status="old", iostat=io)
    if (io /= 0) then
        print *, "Error opening file: ", filename1
        stop
    endif

    ! データを読み込む
    do t = 1, 10
        read(fi, *, iostat=io) i(t), x(t)
        if (io /= 0) then
            print *, "Read error on line: ", t
            stop
        endif
    end do
    close(fi)

    ! データを処理する
    x(:) = x(:) - 0.01d0

    ! 出力ファイルを開く
    open(unit=fo, file=filename2, action="write", status="replace", iostat=io)
    if (io /= 0) then
        print *, "Error opening file: ", filename2
        stop
    endif

    ! データを書き込む
    do t = 1, 10
        write(fo, '(2F24.16)') i(t), x(t)
    end do
    close(fo)

end program asg2
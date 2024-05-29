program extract_number
    implicit none
    character(len=6) :: filename
    character(len=2) :: num_str
    integer :: n

    ! ファイル名を設定
    filename = '01.dat'

    ! 数字部分を抽出
    num_str = filename(1:2)

    ! 文字列を整数に変換
    read(num_str, '(I2)') n

    ! 結果を出力
    print *, 'Extracted number is: ', n

end program extract_number

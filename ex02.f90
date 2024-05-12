!連番ファイル名をつけて、出力結果を同じディレクトリのフォルダ内に出力するプログラム　→　課題に応用する。
!同様に考えて　　入力と出力を同じファイルに入れておいて、そこからそれぞれ取り出せるようにプログラムを組みたい。


program ex02
    implicit none
    integer, parameter :: min_step = 0
    integer, parameter :: max_step = 100
    integer :: step = min_step

    real(8), parameter :: dt = 0.25d0
    real(8) :: t = 0d0

    real(8), parameter :: delta = 0.5d0
    real(8), parameter :: x_min = 0d0
    real(8), parameter :: x_max = 100d0
    real(8) :: x = x_min

    integer,       parameter :: output_file_number = 10
    character(32)            :: output_file_name
    character(16), parameter :: output_format = '(2d24.16)'

    do step = min_step, max_step
        ! ファイル名を書き込む(データは後ほど)
        write(output_file_name, '("ex02_output/data_"i4.4".dat")') step

        ! ファイルを開く
        open(output_file_number, file=output_file_name, status="replace")

        ! do while ループでサインカーブを描く
        x = x_min
        do while (x < x_max)
            write(output_file_number, output_format) x, sin(x - t)
            x = x + delta
        end do
        t = t + dt

        ! ファイルを閉じるのを忘れない
        close(output_file_number)
    end do

    ! [注意]
    ! このプログラムはファイルを 'ex02_output' 内に出力します.
    ! 'ex02_output' が無いと実行時エラーが生じるので, 注意してください.

end program ex02
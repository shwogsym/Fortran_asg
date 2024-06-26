

module func_calc
    implicit none 
    contains
    !f(x) = ax^3 + bx^2 + cx + d
    subroutine function(a, b, c, d, x, f)
        real(8) ,intent(in) :: a, b, c, d, x 
        real(8) ,intent(out) :: f 
        !乗数の書き方注意
        f = a*x**3 + b*x**2 + c*x + d
    end subroutine function

    !f'(x) = 3ax^2 + 2bx + c
    subroutine derivative(a, b, c, x, df)
        real(8) ,intent(in) :: a, b, c, x
        real(8) ,intent(out) :: df
        df = 3.0d0*a*x**2 + 2.0d0*b*x + c   
    end subroutine derivative

end module func_calc


!ニュートン法の計算
module func_module
    use func_calc
    use ad
    implicit none 

    contains 

    subroutine newton(a, b, c, d, x1, n)
        implicit none 

        real(8) ,intent(in)    :: a, b, c, d
        real(8) ,intent(inout) :: x1
        integer ,intent(out)   :: n

        real(8)  x2, f, df, er
        integer  io

        real(8) ,parameter :: eps =1.0d-15, df_min = 1.0d-99
        integer ,parameter :: n_max = 100, output_file_number = 11

        character (32) filename

        !出力ファイルのオープン
        write(filename, '("asg3_6_file/data.dat")') 
        open(output_file_number, file = filename, status = 'replace' ,action='write', iostat=io)
        if (io /= 0) stop 'Failure to open output file'
        
        !初期値を記録
        write (output_file_number,'(I2.2, 2X, d24.16)') 0, x1 
        
        !ニュートン法のループを回し、解がエラー範囲内で見つかれば、exit
        do n = 1, n_max 
            call function(a, b, c, d, x1, f) !f(x) → f
            ! call auto_derivative(a, b, c, d, x1, df) !f'(x) → df !自動微分を使いたかったらこの行を有効化する
            call derivative(a, b, c, x1, df) !f'(x) → df

            if (abs(df) < df_min) stop 'Error, derivative is zero.'
    
            x2 = x1 - f / df

            write (output_file_number,'(I2.2, 2X, d24.16)') n, x2

            !正規化誤差の計算
            er = abs(x1 - x2)/abs(x1)
            if (er < eps) exit

            !次のループのための準備
            x1 = x2
        enddo

        !解が見つからなかったときの処理と、解が見つかったときの処理
        if (n == n_max) then 
            stop 'solution was not found.'
        else
            !結果の出力
            write(*,'(A, d24.16)') 'Solution              :', x2
            write(*,'(A, I4)') 'Number of repetitions :', n
            write(*,'(A)') 'Output file           :  asg3_6_file/data.dat'
            !解の確認
            call function(a, b, c, d, x2, f) !f(x)
            write(*,'(A, d24.16)') 'Funcvalue at solution :', f
        end if

        close (output_file_number) 
    end subroutine newton

end module func_module


!メインプログラム
program asg3_6
    use func_module
    use func_calc
    implicit none

    real (8) a, b, c, d, x_ini
    integer io, n 

    integer ,parameter :: input_file_number = 10

    open(input_file_number, file='asg3_6_file/input.dat', status = 'old', action='read', iostat=io)
    if (io /= 0) stop 'Filure to read input file.'
    !ファイル読み込みに失敗→終了
    read(input_file_number,*) a, b, c, d, x_ini
    !a,b,c,dの３次関数の係数と初期値を読み込む
    close(input_file_number)

    !不定、不能を弾く
    if (a == 0) then 
        if (b == 0) then 
            if (c == 0) then 
                if (d == 0) then 
                    stop 'Undeterminate' ! 0x = 0の場合であり、解が無数にある
                else 
                    stop 'Unsolvable' !0x + d = 0の場合であり、解が存在しない
                end if 
            end if 
        end if 
    end if 

    !ニュートン法サブルーチンの呼び出し結果の出力も兼ねる
    call newton(a, b, c, d, x_ini, n) 

end program asg3_6


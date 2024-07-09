!Newton法のメインプログラム
program main
    use newton
    implicit none
    real (8) a, b, c, d, x_ini
    integer n 

    !初期値と係数の設定
    a = 0.0d0
    b = 1.0d0
    ! b = 0.00000001d0
    c = 0.0d0
    ! d = -0.00000001d0
    d = -1.0d0
    x_ini = 100.0d0

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
    
    !ニュートン法の呼び出し
    call newton_method(a, b, c, d, x_ini, n)
    
    !結果の表示
    write(*,*) 'The solution         : ', x_ini
    write(*,*) 'Number of iterations : ', n
    write(*,*) 'Output file          : data.dat'
end program main
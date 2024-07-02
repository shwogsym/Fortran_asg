!Bisection法のメインプログラム
program main
    use bisection
    implicit none 
    real(8) a, b, c, d, x1, x2, xm
    integer n

    a = 1.0d0
    b = 0.0d0
    c = -1.0d0
    d = 0.0d0
    x1 = 0.0d0
    x2 = 100.0d0

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

    !二分法サブルーチンの呼び出し
    call bisection_method(a, b, c, d, x1, x2, n, xm)
    
    write(*,*) 'Number of iterations : ', n
    write(*,*) 'Output file          : data.dat'

end program main

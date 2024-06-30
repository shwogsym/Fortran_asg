
program asg4 
    use func_module
    implicit none 
    real(8) a, b, c, d, x1, x2, xm
    integer t
    integer, parameter :: input_file_number = 10, max_t = 1000

    a = 0.0d0
    b = 1.0d0
    c = 0.0d0
    d = -1.0d0
    x1 = 0.0d0
    x2 = 200.0d0

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


    !二分法サブルーチンの呼び出し、解の出力も兼ねている。
    call bisection_method(a, b, c, d, x1, x2, t, xm)
    
    write(*,*) 'Number of repetitions :', t
    write(*,*) 'Output file           : data.dat'

end program asg4

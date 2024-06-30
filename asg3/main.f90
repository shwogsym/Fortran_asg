program main
    use func_module
    implicit none

    !係数と初期値
    real (8) a, b, c, d, x_ini
    !反復回数
    integer n 

    !初期値と係数の設定
    a = 1.0d0
    b = 0.0d0
    c = -1.0d0
    d = 0.0d0
    x_ini = 100.0d0

    !ニュートン法の呼び出し
    call newton(a, b, c, d, x_ini, n)
    
    !結果の表示
    write(*,*) 'The solution        : ', x_ini
    write(*,*) 'Number of iterations: ', n
end program main
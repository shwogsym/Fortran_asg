program asg3
    implicit none
    real (8) :: x0,x1,x2,er,er0=1.0d-6  !1.0d-6は10^{-6}を意味する
    integer :: k,km = 100, fi = 10, fo = 11

    open(unit=fi, file='inpasg3.d')
    open(unit=fo, file='outasg3.d')
    read(fi,*) x0 
    close(fi) 
    !inpasg3.d にニュートン法の初期値を入力

    x1 = x0 ! x0は解の近似値の初期値であり、f(x0)・f''(x0) > 0 とすると良い解が得やすい

    do k = 1,km
        x2 = x1 - 0.5d0*(x1**2 - 1.0d0)/x1 !x1より真の値に近いx2を計算する部分、テキスト　(1.6)(1.7) 式参照
        er = abs(x2-x1)
        write (fo,*) k,x2
        if (er < er0) exit !x2とx1の差がrealで定義したer0より小さくなれば、値が収束したと考えてloopを終える。
        x1 = x2 !計算したx2(より真の値に近い)をx1に代入し、同様にループを回し精度を高めていく
    enddo
    !ループに上限値を設定する理由
    !Newton法では初期値により、値が発散する可能性があり、最大値を設定しておかないと計算がいつまで立っても終わらない可能性がある。

    close (fo)
end program asg3

!3次の関数まで対応した改良版が3a6プログラムにある。

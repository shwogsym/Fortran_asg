program asg6
    implicit none
    real (8) :: x0,x1,x2,er,er0=1.0d-6  !1.0d-6は10^{-6}を意味する
    integer :: k,km = 100, fi = 10, fo = 11

    open(unit=fi, file='inpasg6.d')
    open(unit=fo, file='outasg6.d')
    read(fi,*) x0 
    close(fi) 
    !形式上初期値をx0としているだけで、いきなりX1に入れても良い。

    x1 = x0 ! x0は解の近似値の初期値であり、f(x0)・f''(x0) > 0 とすると良い解が得やすい

    do k = 1,km 
        x2 = x1 - (x1**3 - x1)/(3*x1**2-1)
        er = abs(x2-x1)
        write (fo,*) k,x2
        if (er < er0) exit
        x1 = x2 
    enddo
    close (fo)
end program asg6


!x**3 - x のグラフを観たら分かる通り解は1,-1の２つである。そしてこのプログラムを用いるとどちらか一つの解を計算することができて、どちらが選ばれるかは初期値による。
!なぜか？　→　テキスト25P観て分かる通り、ニュートン法は初期値が入力されて地点において、微分係数が小さくなる方向に向けてxを逐一計算し、求めていく手法のため

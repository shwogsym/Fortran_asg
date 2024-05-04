program asg5_2 
    implicit none 
    real(8) a,b,c,x1,x2,xm,fm
    real(8) :: er = 1.0e-6 
    integer t
    integer :: max_t = 1000  

    write(*,*) "Input a,b,c (ax^2 + bx + c = 0):" 
    read(*,*) a,b,c 

    write (*,*) "Input the start value x1 and end value x2" 
    read (*,*) x1,x2 
    !ここで初期値はそれぞれ、f(x1) < 0 、f(x2) > 0　として選ぶ必要がある。

    t = 0 
    open(4,file = 'outasg5_2.d')

    do while (abs(x2-x1) > er .and. t < max_t) 

        xm = (x1 + x2) / 2.0
        fm = a*xm**2 + b*xm + c
        !初期範囲の中点における値を計算する。

        if (fm < 0)  then
            x2 = xm
        else
            x1 = xm
        endif
        t = t + 1 
        write(4,*)t,xm 
    end do

end program asg5_2



!二分法も解が２つある場合、初期値に依存して解が決まる。


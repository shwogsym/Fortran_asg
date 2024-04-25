program asg4 
    implicit none 
    real(8) a,b,c,x1,x2,xm,fm
    real(8) :: er = 1.0e-6 
    integer :: t,max_t = 1000,fi = 10

    write(*,*) "Input a, b, c (ax^2 + bx + c = 0):" 
    read(*,*) a, b, c 

    write(*,*) "Input the start value x1 and end value x2"
    read(*,*) x1, x2 

    if (func(a, b, c, x1) * func(a, b, c, x2) > 0) then
        stop 'One of the values should be positive and the other negative.'
    endif 
    !２つの初期値が二分法を回す条件を満たしているのか検証

    t = 0 
    open(fi,file = 'outasg4.d')

    do while (abs(x2-x1) > er .and. t < max_t) 
        xm = (x1 + x2) / 2.0
        !中点値の計算
        fm = func(a, b, c, xm)
        !中点を題意の関数に代入
        if (fm < 0)  then
            if (func(a, b, c, x2) < 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        else 
            if (func(a, b, c, x2) > 0) then
                x2 = xm
            else 
                x1 = xm
            endif 
        endif
        !中点の値に応じて初期値の入れ替えを行い、次のサイクルに準備
     
        t = t + 1 
        !反復回数のカウント
        write(fi,*)t,xm 
    end do 

    if (t == max_t) then
        write(*,*)  "No solution was found."
    else
        write (*,*)  "Solution :", xm
    end if
    write (*,*) 'Number of repetition ;',t

contains 

    real(8) function func(a, b, c, x)
        implicit none 
        real(8) :: a, b, c, x
        func = a*x**2.0d0 + b*x + c
    end function func
    !関数の定義

end program asg4


    



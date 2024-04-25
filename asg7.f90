program asg7 
    implicit none 
    integer n ,i 
    real(8)  :: x, integral_appro, a, b, h
    real(8) ,allocatable :: f(:,:)
    
    a = 0.0d0 
    b = 2.0d0 * acos(0.0d0)
    !積分範囲の設定、上限値はπ

    write (*,*) 'Input n (number of divisions)'
    read (*,*) n 

    h = (b-a)/n 
    !台形の刻み幅の計算

    allocate(f(2,0:n)) 

    !分割点での sin(x) の値の計算
    do i = 0,n 
        x = a + i*h 
        f(1,i) = x
        f(2,i) = sin(x) 
    end do 
    ! y = sin (x) を配列に格納　

    !integral_appro (微小区間の積分値) の初期設定
    integral_appro = f(2,0) + f(2,n) 

    do i  = 1,n-1,2
        integral_appro  = integral_appro + 4.0d0 * f(2,i) 
    enddo 

    do i = 0,n-2,2 
        integral_appro = integral_appro + 2.0d0 * f(2,i) 
    enddo 
    
    integral_appro = integral_appro * h/3.0d0 

    deallocate (f) 

    write (*,*) 'Approximate integral of sin(x) = ',integral_appro 
end program asg7 


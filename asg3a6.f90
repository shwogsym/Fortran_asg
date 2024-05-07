module func_module
    implicit none 
    
    contains 

    subroutine newton(a, b, c, d,x1)
    implicit none 
    real(8) :: a, b, c,d, x1, x2,f,df,er,er0=1.0d-6
    integer :: k,km = 100, fo = 11

    open(fo, file='outasg3a6.d',action='write')

    do k = 1,km 

        ! Calculate the value of the function and its derivative at x1
       f = a*x1**3 + b*x1**2 + c*x1 + d            ! f(x)
       df = 3*a*x1**2 + 2*b*x1 + c                 ! f'(x)

       if (df == 0) stop 'Error, derivative is zero.'
       !導関数が0のとき、収束がないので計算を止める

       ! Apply Newton's method
       x2 = x1 - f / df
       er = abs(x2 - x1)

       write (fo,*) k,x2

       if (er < er0) exit 
       x1 = x2 
   enddo
   close (fo) 
   end subroutine newton

end module func_module



program asg3_6 
    use func_module
    implicit none

    real (8) :: x1,a,b,c,d
    integer :: fi = 10,ios

    open(fi, file='inpasg3a6.d',action='read',iostat=ios)
    if (ios /= 0) stop 'Filure to open file.'
    !ファイル読み込みに失敗→終了

    read(fi,*) a,b,c,d 
    !a,b,c,dを３次関数の係数としてそれぞれファイルに入力
    close(fi)

    write (*,*) 'Input initial value'
    read (*,*) x1
    !初期値はターミナルから入力、いちいちファイルを弄るのが面倒

    call newton(a, b, c, d, x1)

end program asg3_6


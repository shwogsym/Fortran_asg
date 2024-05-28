!桁落ちが起きるプロトタイプ

module func_module
    implicit none
contains

subroutine solution_formula(a,b,c,x1,x2)
    !二次方程式の解の係数公式を計算するサブルーチン
    !Dを判別式として、実数解、重解で、if 場合分けをして、解を求める

    implicit none 
    real (8) :: a, b, c, x1, x2, D, sqrt_D
    integer ,parameter :: input_file = 10, output_file = 11

    D = b**2.0d0 - 4.0d0*a*c

    !まず、1次方程式以下の場合の処理をする。
    if (a == 0.0 .or. b == 0.0) stop 'Error : This is not a quadratic equation'
    if (D < 0) stop 'Error : This equation has no real solution'
        
    !実数解計算    
    sqrt_D = sqrt(D)
    
    x1 = (-b+D**0.5)/(2*a)
    x2 = (-b-D**0.5)/(2*a)

    write (output_file,*) 'x =',x1, x2
    write (*,*) 'x =',x1, x2

    close(output_file)

end subroutine solution_formula 
end module func_module


program asg1
    use func_module
    implicit none

    real (8) a,b,c,x1,x2
    integer :: io
    integer ,parameter ::input_file = 10 ,output_file = 11

    open (input_file ,file = 'asg1_file/inpasg1.dat',action = 'read',iostat = io)
    if(io /= 0) stop 'Error : failed to open file'
    open (output_file, file = 'asg1_file/outasg1.dat',action = 'write',iostat = io)
    if(io /= 0) stop 'Error : failed to open file'

    read(input_file,*) a,b,c
    close(input_file)

    call solution_formula(a,b,c,x1,x2) 

    write (*,*) 'Output file was made in asg1_file directory with name outasg1.dat'

end program asg1



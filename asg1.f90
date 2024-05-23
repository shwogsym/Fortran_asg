module func_module
    implicit none
contains

subroutine solution_formula(a,b,c,x1,x2)
    implicit none 
    real (8) a,b,c,x1,x2,D,sqrt_D
    integer ,parameter ::input_file = 10 ,output_file = 11
    
    D = b**2 - 4.0d0*a*c
    sqrt_D = sqrt(D)
    
    if (a == 0.0) then
        if(b == 0.0) then 
            stop "Invalid input a,b = 0"
        else 
            x1 = -c/b 
            write(output_file,*) 'x = ',x1
            write(*,*) 'x = ',x1
        endif 
    else 
        if (D < 0) stop 'Invalid input D < 0'   
        
        ! x1 = (-b+(b**2-4*a*c)**0.5)/(2*a)
        ! x2 = (-b-(b**2-4*a*c)**0.5)/(2*a)

        if (b > 0) then
            x1 = (-b - sqrt_D) / (2*a)
            x2 = 2*c / (-b - sqrt_D)
        else
            x1 = 2*c / (-b + sqrt_D)
            x2 = (-b + sqrt_D) / (2*a)
        endif
        write (output_file,*) 'x =',x1,x2
        write (*,*) 'x =',x1,x2
    endif     
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

    read(input_file,*) a,b,c !ファイルに二次関数の計数を入れておく (ax^2 + bx + c = 0)
    close(input_file) 

    call solution_formula(a,b,c,x1,x2) 
    !解の係数公式の呼び出し　→　ファイル出力も含まれている
    write (*,*) 'Output file was made in asg1_file'

end program asg1




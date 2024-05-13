module func_module
    implicit none 

    contains 

    subroutine solution_formula(a,b,c,x1,x2)
        implicit none 
        real (8) a,b,c,x1,x2
        integer ,parameter ::input_file = 10 ,output_file = 11
        
        if (a == 0.0) then
            if(b == 0.0) then 
                stop "Invaid file input a,b = 0 "
            else 
                x1 = -c/b 
                write(output_file,*) 'x = ',x1
            endif 
        else 
            if (b**2 - 4.0d0*a*c < 0) stop 'Invaid input D < 0'
            x1 = (-b+(b**2-4*a*c)**0.5)/(2*a)
            x2 = (-b-(b**2-4*a*c)**0.5)/(2*a)
        endif     
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
        
    if(x1 == x2) then  
        write (output_file,*) 'x=',x1
        write (*,*) 'x=',x1
        write (*,*) 'Output file was made in asg1_file'

    else
        write (output_file,*) 'x =',x1,x2
        write (*,*) 'x=',x1,x2
        write (*,*) 'Output file was made in asg1_file'
    endif
    close(output_file)

    
end program asg1




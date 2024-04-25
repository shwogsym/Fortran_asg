program test
    implicit none
    integer n 
    real(8) ::  x = 10,dx
    write(*,*) "input split time"
    read(*,*) n 

    dx = x/n 

    x = dx *n 

    write (*,*) x
end program test
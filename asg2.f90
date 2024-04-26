program asg2 
    implicit none
    integer :: t, fi = 20,fo = 21
    real(8) i(10),x(10) 
    open(fi,file = 'inpasg2.d',action = "read") 

    do t = 1,10
        read(fi,*) i(t),x(t) 
    enddo 
    close(fi)

    x(:) = x(:) - 0.01d0

    open(fo,file = 'outasg2.d', action = "write") 
    do t = 1,10
        write (fo,'(i2,d24.16)') t,x(t) 
    enddo 

    close(fo) 
    
endprogram asg2 
    
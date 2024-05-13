!サブルーチン化するまでもないかなと、

program asg2 
    implicit none
    integer :: t, fi = 20,fo = 21,io
    real(8) i(10),x(10) 

    open(fi,file = 'asg2_file/inpasg2.d',action = "read",iostat=io) 
    if (io /= 0) stop 'Failure to read file'

    do t = 1,10
        read(fi,*) i(t),x(t) 
    enddo 
    close(fi)

    x(:) = x(:) - 0.01d0

    open(fo,file = 'asg2_file/outasg2.d', action = "write",iostat = io) 
    if (io /= 0) stop 'Failure to read file'
    do t = 1,10
        write (fo,'(i2,d24.16)') t,x(t) 
    enddo 

    close(fo) 
    
endprogram asg2 
    
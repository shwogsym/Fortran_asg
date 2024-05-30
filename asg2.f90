module func_module
    implicit none   
    contains

    subroutine Fix_data(x)
        implicit none 
        real(8)x
        x=x-0.01d0
    end subroutine Fix_data

 end module func_module
 
 program asg2
    use func_module
    implicit none

    real(8) x
    integer i,n,io
    integer ,parameter :: input_file_number = 10, output_file_number = 11

    open(input_file_number,file = 'asg2_file/inpasg2.dat',action = "read",iostat=io) 
    if (io /= 0) stop 'Failure to read file'
    open(output_file_number,file = 'asg2_file/outasg2.dat', action = "write",iostat = io) 
    if (io /= 0) stop 'Failure to read file'

    do i=1,100
       read(input_file_number,*,iostat=io)n,x
       if(io < 0) exit !テキスト33p参照、ファイル最終行の検出
       call Fix_data(x)
       write(output_file_number,'(I2, 2X, F24.16)')n,x
    enddo
    close(input_file_number)
    close(output_file_number)

 end program asg2
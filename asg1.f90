program asg1
    implicit none
    real (8) a,b,c,xp,xm
    integer :: fi = 10 ,fo = 11

    open (fi ,file = 'inpasg1.d',action = 'read')
    open (fo, file = 'outasg1.d',action = 'write')
    read(fi,*) a,b,c
    close(fi) 

    if (b**2 - 4*a*c < 0) stop 'Invaid input D < 0'

    if (a == 0.0) then
        if(b == 0.0) then 
            write (fo,*) "Invaid file input a,b = 0 "
            stop
        else 
            xp = -c/b 
            write(fo,*) 'x = ',xp
        endif 
    else 
        xp = (-b+(b**2-4*a*c)**0.5)/(2*a)
        xm = (-b-(b**2-4*a*c)**0.5)/(2*a)
        if(xp == xm) then  
            write (fo,*) 'x=',xp
        else 
            write (fo,*) 'x =',xp,xm
        endif
    end if

end program asg1




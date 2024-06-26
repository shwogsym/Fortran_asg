module func_module
    implicit none
contains

subroutine solution_formula(a,b,c,x1,x2)
    !二次方程式の解の係数公式を計算するサブルーチン
    !Dを判別式として、実数解、重解で、if 場合分けをして、解を求める

    implicit none 
    real (8) :: a, b, c, x1, x2, D, sqrt_D, absD
    integer, parameter :: input_file = 10, output_file = 11
    complex (8) :: x1_c, x2_c
    
    !判別式をDとする
    D = b**2 - 4.0d0*a*c
    absD = abs(D)

    !まず、1次方程式以下の場合の処理をする。
    
    if (a == 0.0) then
        if (b == 0.0) then 
            if (c == 0.0) then 
                stop 'huteikai' !その後にも処理が続くので、stopは合ってもなくても良い
            else
                stop 'hunou'
            endif
        else
            x1 = -c/b 
            write(output_file,'(F26.16)') 'x = ',x1
            write(*,*) 'x = ',x1
        endif
    else if (absD < 1d-13) then 
        write(*,*) 'jyuukai'
        
    else

        !虚数解計算
        if (D < 0) then 
            sqrt_D = sqrt(-D)
            x1_c = cmplx(-b /(2.0d0*a), sqrt_D/(2.0d0*a), kind = 8)
            x2_c = cmplx(-b /(2.0d0*a), -sqrt_D/(2.0d0*a), kind = 8)
            write(output_file,'(a,2F24.16,a)') 'x1 = ',x1_c ,'i'
            write(output_file,'(a,2F24.16,a)') 'x2 = ',x2_c ,'i'

            write(*,'(SP,A,2F24.16,A)') 'x1 = ',x1_c ,'i'
            write(*,'(SP,A,2F24.16,A)') 'x2 = ',x2_c ,'i'
        
        else if (D > 0) then
        !実数解計算
            sqrt_D = sqrt(D)

            if (b >= 0) then
                x1 = (-b - sqrt_D) / (2.0d0*a)
                 x2 = 2.0d0*c / (-b - sqrt_D)
            else
                x1 = 2.0d0*c / (-b + sqrt_D)
                x2 = (-b + sqrt_D) / (2.0d0*a)
            endif
        endif

        ! x1 = (-b+(b**2-4*a*c)**0.5)/(2*a)
        ! x2 = (-b-(b**2-4*a*c)**0.5)/(2*a)　としていないのは、桁落ち防止の為。

        write (output_file,*) 'x =' ,x1, x2
        write (*,*) 'x =' ,x1, x2
    endif

    close(output_file)
end subroutine solution_formula    
end module func_module






program asg1
    use func_module
    implicit none

    !入力に使う係数と、出力用の解の変数の宣言
    real (8) a,b,c,x1,x2
    integer :: io
    !ファイル番号を ioそれぞれ決めている。
    integer ,parameter ::input_file = 10 ,output_file = 11

    open (input_file ,file = 'asg1_file/inpasg1.dat',action = 'read',iostat = io)
    if(io /= 0) stop 'Error : failed to open file'
    open (output_file, file = 'asg1_file/outasg1.dat',action = 'write',iostat = io)
    if(io /= 0) stop 'Error : failed to open file'

    read(input_file,*) a,b,c !ファイルに二次関数の計数を入れておく (ax^2 + bx + c = 0)
    close(input_file)

    call solution_formula(a,b,c,x1,x2) 
    !解の係数公式の呼び出し　→　ファイル出力も行われる。

    write (*,*) 'Output file was made in asg1_file directory with name outasg1.dat'

end program asg1




!明日事前に問題設定をしてプログラムを回すから、あからじめファイルを作って置くといいかんも

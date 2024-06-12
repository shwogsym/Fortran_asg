
!微分の線形性や、ライプニッツ則のアルゴリズムを定義
!各演算子ごとの、オペレーションを type を用いて派生タイプを定義し、その中に関数を定義

module deriv_m 
    implicit none 

    ! .add.っていう演算子が来たら、add_pっていう関数を呼び出す
    interface operator(.add.)
        module procedure :: add_p
    end interface

    interface operator(.sub.)
        module procedure :: sub_p
    end interface

    interface operator(.mul.)
        module procedure :: mul_p
    end interface

    interface operator(.div.)
        module procedure :: div_p
    end interface
contains

    !微分の線形性を用いた演算子の定義
    function add_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  + b(1)
         res(2) = a(2) + b(2)
    end function add_p

    function sub_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  - b(1)
         res(2) = a(2) - b(2)
    end function sub_p

    !ライプニッツ則を用いた演算子の定義
    function mul_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  * b(1)
         res(2) = a(2) * b(1) + a(1) * b(2)
    end function mul_p

    function div_p(a, b) result(res)
         real(8), dimension(2), intent(in) :: a, b
         real(8), dimension(2) :: res
         res(1)  = a(1)  / b(1)
         res(2) = (a(2) * b(1) - a(1) * b(2)) / b(1)**2
    end function div_p

end module deriv_m

program auto_derivative
    use deriv_m
    implicit none

    real(8), dimension(2) :: a, b, c, d, x, y
    real(8) :: a_val, b_val, c_val, d_val, x_val

    !定数値の設定
    a_val = 1.0d0
    b_val = 1.0d0
    c_val = 1.0d0
    d_val = 1.0d0
    x_val = 1.0d0

    !定数値を代入、微分値を設定
    a(1) = a_val; a(2) = 0.0d0
    b(1) = b_val; b(2) = 0.0d0
    c(1) = c_val; c(2) = 0.0d0
    d(1) = d_val; d(2) = 0.0d0
    x(1) = x_val; x(2) = 1.0d0

    !出力が理論値と異なる　→　プログラミングノート参照
    !結論原因は和と積の演算子の優先順位が反映されていない。
    !解決策として、括弧を用いて演算子の優先順位を明示する。

    !微分の計算
    !サブルーチンじゃなくて、ファンクションだから、式中に定義した演算子を直接書き込んで計算できる、演算子部分で関数は適宜呼び出される。
    y = a .mul. x .mul. x .mul. x .add. (b .mul. x .mul. x) .add. (c .mul. x) .add. d

    !→　このように、ファンクションのメリットは数式自体に外部モジュールが用いられ、数式として形作られるときに、簡潔な記述ができる。
    !したがって、今回は絶対にfunctionの方が良い、subroutine だと関数の形を変えるときに、逐一callするものを動かさないと行けない →　超面倒 & 難しい 
    write(*,*) 'y = ', y(1)
    write(*,*) 'dy/dx = ', y(2)

end program auto_derivative
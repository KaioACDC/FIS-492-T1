module estatistica
    implicit none 

contains

!Vamos calcular a média
    function media(valor, x) result(y)
        !Definindo 
        real, dimension(:), intent(in) :: valor
        integer, intent(in) :: x
        real :: y
        y = sum(valor)/ real(x)
    end function media

    
module estatistica
    implicit none 

contains

!Vamos calcular a média
    function media(valor, x) result(y)
        !Definindo dimensão dos valores
        real, dimension(:), intent(in) :: valor
        integer, intent(in) :: x
        real :: y
        y = sum(valor) / real(x)
    end function media

!Vamos calcular a variância
    function variancia(valor, x, y) result(vvariancia)
        !Definindo dimensão dos valores
        real, dimension(:), intent(in) :: valor
        integer, intent(in) :: x
        real, intent(in) :: y
        real :: vvariancia
        vvariancia = sum((valor - y)**2) / real(x-1)
    end function variancia


! Examen 1
! Problema 20
! Diego Antonio Villalba González
! Método de trapecio

program trapecio
  
  ! Declaramos de variables
  double precision a, b, h, integral
  integer n, i

  ! Declaramos los límites de integración y del número de intervalos
  a = 0.0          ! Límite inferior
  b = 4.0          ! Límite superior
  n = 10000          ! Número de subintervalos

  ! Calcular el tamaño del step
  h = (b - a) / real(n, 8)

  ! Calculamos la integral usando el método del trapecio
  integral = 0.5 * (f(a) + f(b))
  do i = 1, n-1
     integral = integral + f(a + i * h)
  end do

  ! Multiplicamos por el tamaño del step para obtener el valor de la integral
  integral = integral * h

  ! Imprimir el resultado
  print *, "La longitud de arco aproximada es: ", integral

contains
  ! Función para la integranda sqrt(1 + (dy/dx)^2)
  double precision function f(x)
    double precision, intent(in) :: x
    f = sqrt(1.0 + (2.0 * x)**2)
  end function f

end 
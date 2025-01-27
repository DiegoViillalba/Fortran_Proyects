! Examen 1
! Problema 20
! Diego Antonio Villalba González
! Método de simpson

program simpson

  ! Declaramos de variables
  double precision a, b, h, integral
  integer n, i

  ! Declaramos los límites de integración y del número de intervalos
  a = 0.0          ! Límite inferior
  b = 4.0          ! Límite superior
  n = 100         ! Número de subintervalos (debe ser par), si no se convierte

  ! Comprobar que el número de intervalos es par
  if (mod(n, 2) /= 0) then
    n = n+1
     print *, "Se convirtió a numero par de intervalos:  n = ",n
  end if

  ! Calcular el tamaño del step
  h = (b - a) / real(n, 8)

  ! Calcular la integral usando el método de Simpson

  integral = f(a) + f(b)        ! Calculamos el valor de los extremos 
  do i = 1, n-1
     if (mod(i, 2) == 0) then
        integral = integral + 2.0 * f((a + i) * h)        ! Multiplicamos por 2 si el indice es par
     else
        integral = integral + 4.0 * f((a + i) * h)        ! Multiplicamos por 4 si el indice es impar
     end if
  end do

  ! Dividir suma entre 3 
  integral = integral * h / 3.0

  ! Imprimir el resultado
  print *, "La longitud de arco aproximada es: ", integral


  ! Insertar nuestra función en el programa principal
contains

  ! Función a integrae sqrt(1 + (dy/dx)^2) 
  ! Notemos que dx/dy = 2x

  double precision function f(x)
    double precision, intent(in) :: x
    f = sqrt(1.0 + (2.0 * x)**2)
  end function f


end 

program find_roots
  implicit real (a-h, o-z)
  real :: a, b, c, f_a, f_b, f_c, tol
  real :: interval_start, interval_end, step
  integer :: max_iter, iter

  ! Configuración inicial
  interval_start = 0.0       ! Inicio del intervalo
  interval_end = 5.0         ! Fin del intervalo
  step = 0.5                 ! Paso para dividir el intervalo
  tol = 1.0e-6               ! Tolerancia
  max_iter = 100             ! Número máximo de iteraciones

  ! Encontrar raíces en cada subintervalo dentro del rango [0, 5]
  print *, "Raíces encontradas en el intervalo [", interval_start, ",", interval_end, "]:"
  a = interval_start
  do while (a < interval_end)
    b = a + step
    iter = 0

    ! Evaluar la función en los extremos del subintervalo
    f_a = f(a)
    f_b = f(b)

    ! Verificar que haya un cambio de signo
    if (f_a * f_b < 0.0) then
      ! Aplicar el método de falsa posición
      do
        ! Calcular el punto c usando falsa posición
        c = b - f_b * (b - a) / (f_b - f_a)

        ! Evaluar la función en el punto c
        f_c = f(c)

        ! Verificar convergencia
        if (abs(f_c) < tol .or. abs(b - a) < tol .or. iter >= max_iter) exit

        ! Actualizar el intervalo [a, b]
        if (f_a * f_c < 0.0) then
          b = c
          f_b = f_c
        else
          a = c
          f_a = f_c
        end if

        iter = iter + 1
      end do

      if (iter < max_iter) then
        print *, "Raíz encontrada: ", c
      else
        print *, "El método de falsa posición no convergió en el subintervalo [", a, ",", b, "]"
      end if
    end if

    ! Avanzar al siguiente subintervalo
    a = b
  end do
end program

! Función definida por el usuario
real function f(x)
  implicit none
  real, intent(in) :: x
  ! Definir aquí la función
  f = x**3 - 6.0 * x**2 + 11.0 * x - 6.0  ! Ejemplo: Polinomio cúbico con raíces en x=1, x=2, x=3
end function
program biseccion_funcion_arbitraria
  implicit none
  real(8) :: a, b, tol, raiz
  integer :: iter_max, iter
  logical :: exito

  ! Solicitar intervalo inicial
  print *, 'Introduce el intervalo [a, b]:'
  read *, a, b

  ! Solicitar tolerancia
  print *, 'Introduce la tolerancia:'
  read *, tol

  ! Solicitar el número máximo de iteraciones
  print *, 'Introduce el número máximo de iteraciones:'
  read *, iter_max

  ! Llamar a la función que realiza el método de bisección
  call metodo_biseccion(a, b, tol, iter_max, raiz, exito)

  ! Mostrar resultado
  if (exito) then
     print *, 'La raíz encontrada es: ', raiz
  else
     print *, 'El método no ha convergido.'
  endif

contains

  ! Función arbitraria a evaluar (puedes modificar esta función para cambiar la ecuación)
  real(8) function funcion_arbitraria(x)
    real(8), intent(in) :: x
    ! Ejemplo: f(x) = x^3 - 4x^2 + 6x - 24 (puedes cambiar la función aquí)
    funcion_arbitraria = x**3 - 4*x**2 + 6*x - 24
  end function funcion_arbitraria

  ! Subrutina que implementa el método de bisección
  subroutine metodo_biseccion(a, b, tol, iter_max, raiz, exito)
    real(8), intent(in) :: a, b, tol
    integer, intent(in) :: iter_max
    real(8), intent(out) :: raiz
    logical, intent(out) :: exito
    real(8) :: fa, fb, fm, m
    integer :: iter

    exito = .false.
    fa = funcion_arbitraria(a)
    fb = funcion_arbitraria(b)

    ! Verificar que haya un cambio de signo en el intervalo
    if (fa * fb > 0.0) then
      print *, 'No hay un cambio de signo en el intervalo dado.'
      return
    endif

    do iter = 1, iter_max
      ! Punto medio
      m = (a + b) / 2.0
      fm = funcion_arbitraria(m)

      ! Verificar si hemos alcanzado la precisión deseada
      if (abs(fm) < tol) then
        raiz = m
        exito = .true.
        return
      endif

      ! Decidir en qué subintervalo continuar
      if (fa * fm < 0.0) then
        b = m
        fb = fm
      else
        a = m
        fa = fm
      endif
    end do

    ! Si llegamos aquí, el método no ha convergido en el número de iteraciones
    raiz = (a + b) / 2.0
  end subroutine metodo_biseccion

end program biseccion_funcion_arbitraria

program find_polynomial_roots
  implicit none
  integer, parameter :: n = 4 ! Grado del polinomio
  real :: x_points(n), y_points(n), coefficients(n)
  real :: guess, tol
  integer :: i

  ! Leer puntos desde un archivo
  call read_points('interpolation_e1.dat', x_points, y_points, n)

  ! Calcular los coeficientes del polinomio interpolador de Newton
  call newton_interpolation(x_points, y_points, coefficients, n)

  ! Imprimir los coeficientes del polinomio
  print *, 'Coeficientes del polinomio interpolador:'
  do i = 1, n
    print *, 'a(', i, ') = ', coefficients(i)
  end do

  ! Encontrar raíces del polinomio usando el método de Newton-Raphson
  tol = 1.0e-6
  print *, 'Raíces del polinomio:'
  do i = 1, n
    guess = x_points(i) ! Usar los puntos iniciales como aproximaciones iniciales
    call newton_raphson(coefficients, n, guess, tol)
    print *, 'Raíz ', i, ': ', guess
  end do

contains

  subroutine read_points(filename, x, y, size)
    implicit none
    character(len=*), intent(in) :: filename
    real, intent(out) :: x(size), y(size)
    integer, intent(in) :: size
    integer :: i
    open(unit=10, file=filename, status='old')
    do i = 1, size
      read(10, *) x(i), y(i)
    end do
    close(10)
  end subroutine read_points

  subroutine newton_interpolation(x, y, a, size)
    implicit none
    real, intent(in) :: x(size), y(size)
    real, intent(out) :: a(size)
    integer, intent(in) :: size
    real :: diff_table(size, size)
    integer :: i, j

    ! Construir la tabla de diferencias divididas
    diff_table(:, 1) = y(:)
    do j = 2, size
      do i = 1, size - j + 1
        diff_table(i, j) = (diff_table(i + 1, j - 1) - diff_table(i, j - 1)) / (x(i + j - 1) - x(i))
      end do
    end do

    ! Los coeficientes del polinomio son la primera fila de la tabla
    a(:) = diff_table(1, :)
  end subroutine newton_interpolation

  subroutine newton_raphson(a, size, root, tol)
    implicit none
    real, intent(in) :: a(size)
    integer, intent(in) :: size
    real, intent(inout) :: root
    real, intent(in) :: tol
    real :: f, df, h
    integer :: i

    do
      f = a(size)
      df = 0.0

      ! Evaluar el polinomio y su derivada
      do i = size - 1, 1, -1
        df = df * root + f
        f = f * root + a(i)
      end do

      ! Calcular el siguiente valor de la raíz
      h = f / df
      root = root - h

      if (abs(h) < tol) exit
    end do
  end subroutine newton_raphson

end program find_polynomial_roots

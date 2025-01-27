! Proyecto : Ecuación secular de un sistema de perturbaciones con cargas

! Diego Antonio Villalba Gonzalez
! Jonathan Alexis Hernández Cuevas
! Hirám Noé López Sánchez

! Programa : Potential matrix calculation 

!!--- Instrucciones-----
! El programa calcula las matrices de potencial para los diferentes estados de x1 y x2 en nuestra grilla
! Como salida obtenemos n-2 matrices con nombre m_i.dat
! El resultado lo usaremos en el segundo programa para conocer los valores propios

program p_1
  implicit none

  ! Declare variables
  integer, parameter :: n = 7         ! Grid size
  real, parameter :: h = 1             ! Step size
  real :: x(n), y(n)                   ! Coordinate arrays
  real :: f(n, n)                      ! Function values
  real :: d2fdx2(n, n), d2fdy2(n, n), d2fdxdy(n, n)  ! Second-order partial derivatives
  integer :: i, j                      ! Loop indices
  character(len=1024) :: filename      ! Name of the matrix file

  ! Sistem Variables
  real :: k, m, q, a

  !q = 8.98755e9
  ! We'll use Natural Units (1/4π€_0) = 1
  k = 5.0
  m = 1.0
  q = 1.0
  a = 1.0

  ! Initialize the grid and the function values
  call initialize_grid(x, y, n)
  call initialize_function(f, x, y, n)

  ! Compute second-order partial derivatives using central differences
  do i = 2, n-1
    do j = 2, n-1
      d2fdx2(i, j) = (f(i+1, j) - 2.0 * f(i, j) + f(i-1, j)) / (h**2)
      d2fdy2(i, j) = (f(i, j+1) - 2.0 * f(i, j) + f(i, j-1)) / (h**2)
      d2fdxdy(i, j) = (f(i+1, j+1) - f(i+1, j-1) - f(i-1, j+1) + f(i-1, j-1)) / (4.0 * h**2)
    end do
  end do

  ! Output the derivatives
  call output_derivatives(d2fdx2, d2fdy2, d2fdxdy, n)

  ! Matrix Storage
  call matrix_storage(d2fdx2, d2fdy2, d2fdxdy, n)

contains

  ! Subroutine to initialize the grid
  subroutine initialize_grid(x, y, n)
    real, intent(out) :: x(n), y(n)
    integer, intent(in) :: n
    integer :: i

    do i = 1, n
      x(i) = -3.0 + (i-1) * h
      y(i) = -3.0 + (i-1) * h
    end do

    ! Print the grid points
    print *, "Grid points:"
    print *, "x-coordinates:", x
    print *, "y-coordinates:", y

  end subroutine initialize_grid

  ! Subroutine to initialize the function values
  subroutine initialize_function(f, x, y, n)
    real, intent(out) :: f(n, n)
    real, intent(in) :: x(n), y(n)
    integer, intent(in) :: n
    integer :: i, j

    do i = 1, n
      do j = 1, n
        f(i, j) = (0.5)*k*x(i)**2 + (0.5)*k*y(j)**2 + (0.5)*k*(x(i)-y(j))**2 + q*(1/(y(j)-x(i)+a))
      end do
    end do
  end subroutine initialize_function

  ! Subroutine to output the derivatives
  subroutine output_derivatives(d2fdx2, d2fdy2, d2fdxdy, n)
    real, intent(in) :: d2fdx2(n, n), d2fdy2(n, n), d2fdxdy(n, n)
    integer, intent(in) :: n
    integer :: i, j

    print *, "Second-order partial derivatives:"
    do i = 2, n-1
    if (i<j) then
      do j = 2, n-1
        print *, "At (", x(i), y(j), "):"
        print *, "d2f/dx2 =", d2fdx2(i, j)
        print *, "d2f/dy2 =", d2fdy2(i, j)
        print *, "d2f/dxdy =", d2fdxdy(i, j)
      end do
    endif
    end do
  end subroutine output_derivatives

  ! Subroutine to save in matrix form
  subroutine matrix_storage(d2fdx2, d2fdy2, d2fdxdy, n)
    real, intent(in) :: d2fdx2(n, n), d2fdy2(n, n), d2fdxdy(n, n)
    integer, intent(in) :: n
    integer :: i, j
    
    do i = 2, n-1
    if (i<j) then
    do j = 2, n-1
        open(unit=i, file='m_'//trim(str(i))//'.dat')
        write (i,*) d2fdx2(i, j),d2fdxdy(i, j)
        write (i,*) d2fdxdy(i, j),d2fdy2(i, j)
        close(i)
      end do
    end if
    end do

    print *, "All matrix saved"
  end subroutine matrix_storage

  ! Function to convert an integer to string 
  character(len=20) function str(k)

    integer, intent(in) :: k
    write (str, *) k
    str = adjustl(str)
  end function str

end program p_1


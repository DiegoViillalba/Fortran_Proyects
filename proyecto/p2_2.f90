program p_2
  implicit none

  ! Parameters and variable declarations
  integer, parameter :: n = 2
  real :: A(n, n)               ! Input Matrix
  real :: M_aux(n, n), M_inv(n, n)  ! Auxiliary Matrices
  real :: V_eval(n), M_evec(n, n)   ! Eigenvalues and Eigenvectors
  real :: U(n, n), v(n)             ! Identity Matrix and Vector
  real :: vapropmax, vpc, h, b
  integer :: i, j, k
  logical :: cond

  ! Read matrix from file
  open(1, file='m_2.dat')
  do i = 1, n
    read(1, *) (A(i, j), j = 1, n)
  end do
  close(1)

  ! Print matrix
  write(*,*) "The matrix A is:"
  do i = 1, n
    write(*,*) (A(i, j), j = 1, n)
  end do
  write(*,*)

  ! Initialize identity matrix
  U = 0.0
  do i = 1, n
    U(i, i) = 1.0
  end do

  ! Calculate the largest eigenvalue using the power method
  call power_method(n, A, vapropmax, M_evec(1, 1:n))
  V_eval(1) = vapropmax

  ! Step size for eigenvalue exploration
  h = ABS(vapropmax) / 100.0
  k = 1

  do i = 1, 1000
    b = -ABS(vapropmax) + i * h
    M_aux = A - b * U
    call inverse(n, M_aux, M_inv)

    ! Apply power method to the inverse matrix
    call power_method(n, M_inv, vpc, v)
    call ev_verification(n, b, vpc, v, A, vpc, cond)

    if (cond .AND. (ABS(vpc - V_eval(k)) > 1.0E-3)) then
      k = k + 1
      V_eval(k) = vpc
      M_evec(k, 1:n) = v
    end if

    if (k == n) exit
  end do

  ! Print results
  do i = 1, n
    write(*,*) "λ(", i, ") = ", V_eval(i)
    write(*,*) "Eigenvector:", M_evec(i, 1:n)
    write(*,*)
  end do
end program p_2

! Subroutine for power method
subroutine power_method(n, A, l, v)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: A(n, n)
  real, intent(out) :: l
  real, intent(inout) :: v(n)

  real :: v_new(n), norm, error
  integer :: i
  real, parameter :: tol = 1.0e-5

  ! Initialize vector
  v = 1.0
  l = 0.0
  error = 1.0

  do
    ! Matrix-vector multiplication
    v_new = matmul(A, v)

    ! Normalize vector
    norm = sqrt(dot_product(v_new, v_new))
    v_new = v_new / norm

    ! Update eigenvalue
    l = dot_product(v_new, matmul(A, v_new))

    ! Check for convergence
    error = sqrt(sum((v_new - v)**2))
    if (error < tol) exit

    ! Update vector
    v = v_new
  end do
end subroutine power_method

! Subroutine for matrix inversion (2x2 matrix)
subroutine inverse(n, A, M_inv)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: A(n, n)
  real, intent(out) :: M_inv(n, n)
  real :: det

  ! Calculate determinant
  det = A(1, 1) * A(2, 2) - A(1, 2) * A(2, 1)
  if (ABS(det) < 1.0E-10) then
    print *, "Matrix is singular or nearly singular."
    stop
  end if

  ! Compute inverse
  M_inv(1, 1) =  A(2, 2) / det
  M_inv(1, 2) = -A(1, 2) / det
  M_inv(2, 1) = -A(2, 1) / det
  M_inv(2, 2) =  A(1, 1) / det
end subroutine inverse

! Subroutine for eigenvalue verification
subroutine ev_verification(n, b, l, v, A, l_nc, cond)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: b, l
  real, intent(in) :: A(n, n)
  real, intent(in) :: v(n)
  real, intent(out) :: l_nc
  logical, intent(out) :: cond

  real :: conf1, conf2, conf3, l_1, l_2

  cond = .FALSE.
  l_nc = 0.0
  l_1 = (1.0 / l) + b
  l_2 = b - (1.0 / l)

  conf1 = dot_product(A(1, 1:n), v)
  conf2 = l_1 * v(1)
  conf3 = l_2 * v(1)

  if (ABS(conf1 - conf2) < 1.0E-3) then
    l_nc = l_1
    cond = .TRUE.
  end if

  if (ABS(conf1 - conf3) < 1.0E-3) then
    l_nc = l_2
    cond = .TRUE.
  end if
end subroutine ev_verification
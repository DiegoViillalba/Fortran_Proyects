! Proyecto : Ecuación secular de un sistema de perturbaciones con cargas

! Diego Antonio Villalba Gonzalez
! Jonathan Alexis Hernández Cuevas
! Hirám Noé López Sánchez

! Programa : Eigen values / Eigen vector calculation

!!--- Instrucciones-----
! El programa lee una matriz almacenada en un archivo con el nombre m_i.dat, en este caso lo haremos para i = 2
! Solo debe de estar en el mismo directorio para que funcione, n controla el tamaño de la matrix, en este caso n=2
! Obtendremos los valores propios (modos normales) de nuestra matriz de potencial
program p_2

! Declare Variables
  parameter(n=2)  ! Matrix size
  real :: A(1:n,1:n) ! Main Matrix
  real :: M_aux(1:n,1:n), M_inv(1:n,1:n) ! Auxiliary Matrix
  real :: V_eval(1:n), M_evec(1:n,1:n) ! Eigenvalues vector and matrix
  real :: U(1:n,1:n), v(1:n) ! Support martix 
  logical:: cond ! Logical condition

  ! Read and print Matrix
  open(1, file='m_2.dat')
  do i = 1, n
    read(1, *) (A(i, j), j = 1, n)
  end do
  close(1)

  write(*,*) 'The matrix A is:'
  do i = 1, n
    write(*, *) (A(i, j), j = 1, n)
  end do
  write(*,*)

  ! An identity matrix  U  is created.
  U = 0.0
  do i=1,n
    U(i,i) = 1.0
  end do

  ! Maximum Eigenvalue Calculation:
  call power_method(n,A,vapropmax,M_evec(1,1:n))
  V_eval(1) = vapropmax

  ! Set an step size according to the maximum eigenvalue
  h = ABS(vapropmax)/1000.0
  k = 1
  
  do i=1,10000
    b=-ABS(vapropmax)+i*h
    M_aux=A-b*U
    call inverse(n,M_aux,M_inv)
    call power_method(n,M_inv,x,v)
    call ev_verification(n,b,x,v,A,vpc,cond)

    if(cond.AND.(ABS(vpc-V_eval(k)).gt.1.0E-3)) then
      k=k+1
      V_eval(k)=vpc
      M_evec(k,1:n)=v
    end if

    if(k.eq.n) exit
  end do


  ! Print results 
  do i=1,n
    write(*,*) 'λ(',i,')' ,V_eval(i)
    write(*,*) '[', M_evec(i,1:n),']'
    write(*,*)
  end do
end program p_2


!----- SUBROUTINES ------

subroutine power_method(n,A,l,v)
    real :: l, tol,err,norm
    real, dimension(n, n) :: A
    real, dimension(n) :: v, c
    real :: random_vector(2)

    ! Initialize our vector
    call generate_random_vector(random_vector)
    v = random_vector

    ! Declare variables
    l = 10.0
    tol = 1.0e-5
    err = 1.0

    ! power method iterarion
    do while(ABS(err).gt.tol)
      
      ! Matrix * Guess product
      c=MATMUL(A,v)

      ! Normalize veector
      norm = sqrt(DOT_PRODUCT(c,c))
      v=(c/norm)
      
      ! Error update
      err = l - DOT_PRODUCT(v,MATMUL(A,v))

      l= DOT_PRODUCT(v,MATMUL(A,v))
    end do

  ! Sign Correction
  if(ABS(DOT_PRODUCT(A(1,1:n),v)-l*v(1)).lt.1E-3) then
    l=-l
  end if
end subroutine

subroutine generate_random_vector(v)
  implicit none

  ! Declare variables
  real, intent(out) :: v(2)  ! Output vector
  integer :: i               ! Loop variable
  real :: rand_val           ! Temporary random value

  ! Seed the random number generator
  call random_seed()

  ! Generate random numbers for each element of the vector
  do i = 1, 2
    call random_number(rand_val)  ! Generate a random number in [0, 1)
    v(i) = rand_val
  end do
end subroutine generate_random_vector


subroutine inverse(n,A,M_inv)
  real, dimension(n, n) :: A, b, M_inv
  real, dimension(n,2*n) :: AEXT
  AEXT=0.0
  error=1E-7

  do i=1,n
    do j=1,n
      AEXT(i,j)=A(i,j)
    end do
  end do

  do i=1,n
      AEXT(i,n+i)=1.0
  end do

  !Matriz triangular inferior
  do j=n-1,1,-1
      if(ABS(AEXT(j+1,j+1)).lt.(error)) then
          do m=j,1,-1 
              if(ABS(AEXT(m,j+1)).gt.error) then
                  b(m,:)=AEXT(j+1,:)
                  AEXT(j+1,:)=AEXT(m,:)
                  AEXT(m,:)=b(m,:)
                  goto 200
              end if
          end do
      end if
      
      200 continue
      do i=j,1,-1
          AEXT(i,:)=AEXT(i,:) - (AEXT(i,j+1)/AEXT(j+1,j+1))*AEXT(j+1,:)
      end do
  end do

  !Matriz triangular superior
  do j=2,n
      if(ABS(AEXT(j-1,j-1)).lt.(error)) then
          do m=j,n 
              if(ABS(AEXT(m,j-1)).gt.error) then
                  b(m,:)=AEXT(j-1,:)
                  AEXT(j-1,:)=AEXT(m,:)
                  AEXT(m,:)=b(m,:)
                  goto 100
              end if
          end do
      end if
      
      100 continue
      do i=j,n
          AEXT(i,:)=AEXT(i,:) - (AEXT(i,j-1)/AEXT(j-1,j-1))*AEXT(j-1,:)
      end do
  end do
  
  do i=1,n
      AEXT(i,:)= (1.0/AEXT(i,i))*AEXT(i,:)
  end do

  do i=1,n
    do j=1,n
      M_inv(i,j)=AEXT(i,n+j)
    end do
  end do

end subroutine

subroutine ev_verification(n,b,l,v,A,l_nc,cond)
  logical:: cond
  real:: l, l_nc, b, l_1, l_2
  real, dimension(n, n) :: A
  real, dimension(n) :: v
  
  cond=.FALSE.
  l_nc=0.0
  l_1=(1/l)+b
  l_2=b-(1/l)

  conf1=DOT_PRODUCT(A(1,1:n),v)
  conf2=l_1*v(1)
  conf3=l_2*v(1)

  if(ABS(conf1-conf2).lt.1E-3) then
    l_nc=l_1
    cond= .TRUE.
  end if

  if(ABS(conf1-conf3).lt.1E-3) then
    l_nc=l_2 
    cond=.TRUE.
  end if

end subroutine


! Program : Tarea 5
! Diego Antonio Villalba Gonzalez
! Ejercicio 1
program e1
implicit none

  ! Declaración de variables
  real :: A(3, 3), A_1(3,3), U(3,3)
  real :: det
  real :: x(0:3), y(0:3), diff(0:3,0:3)
  integer :: i, j, k, n,l

  real :: fy,p,s,q,h


  ! Abrimos el archivo y leemos la matriz
  open(unit=10, file='matriz_t5.dat', status='old', action='read')
  do i = 1, 3
    read(10, *) (A(i, j), j = 1, 3)
  end do
  close(10)

  ! Mostramos la matriz leída
  print *, 'La matriz leída es:'
  do i = 1, 3
    print *, (A(i, j), j = 1, 3)
  end do

  ! Calculamos 4 matrices y su determinante, para concoer los piunto del polinomio característico
  print *, 'Puntos para interpolación'


  open(unit=11, file='interpolation_e1.dat')
  ! Creamos la matriz diagonal con los calores de λ a restar
  do k=1,4
  U = 0.0
  do i=1,3
    U(i,i) = real(k) - 0.5
  end do
  A_1 = A - U
  ! Calculamos su determinante
  call calculate_determinant(A_1,det)

  ! Guardamos en un archivo.dat
  write(11,*) k-0.5,det

  ! Imprimos en la terminal
  print *, k-0.5,det
  end do
  close(11)
  
  ! Realizamos interpolación de Newton
  n = 3

  open(unit=15,file= 'interpolation_e1.dat', status='OLD')
    do i=0,3
        read(15,*) x(i),y(i)
    enddo
    close(15)

    do i=0,3
        print *,i,x(i),y(i)
    enddo

    print *,'Newton interpolation matrix'
    

    !---- Compute of divided differences -----

    !-- All the first column are initial values
    do i = 0, n
     diff(i, 0) = y(i)
    end do

    !--- Divided diferences algorithm
    do j=1,n
        do i=0,n-j

            diff(i,j)= (diff(i+1, j-1) - diff(i, j-1)) / (x(i+j) - x(i))

        enddo
    enddo


    do i=0,n
        print *, diff(i,:)
    enddo


    print *,'Coeficientes de interpolación de newton'
    
    do i=0,n
        print *, 'a(',i,')',diff(0,i)
    enddo


    print *, 'Polynomial result'

    open(unit=12,file='e1_1.dat')

    do l=0,200
    s = 0.
    p=1.

    h = real(l)
    h = h*0.025

        !Sumation
        do i=0,n
            q = diff(0,i)
            p = 1.
            if(i.gt.0) then
                !product
                do j=0,i-1
                    !print *, p,'*',k,'-',x(j)
                    p=p*(h-x(j))
                enddo
            endif
            s = s+(q*p)
        enddo
        fy = s

    write(12,*) h, fy
    enddo

    close(12)
    call system('gnuplot grafica.gp')


end program e1

! Subrutina para calcular el determinante de una matriz 3x3
subroutine calculate_determinant(A, det)
  implicit none
  real, intent(in) :: A(3, 3) ! Matriz de entrada
  real, intent(out) :: det   ! Determinante calculado

  ! Fórmula para el determinante de una matriz 3x3
  det = A(1, 1) * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) - A(1, 2) * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1)) + A(1, 3) * (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))

end subroutine calculate_determinant



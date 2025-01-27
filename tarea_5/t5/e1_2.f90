program fake_pos

implicit real (a-h, o-z) 
dimension x(0:100), fx(0:100)
    a=0.
    b=5.
    n=100 
    h=(b-a)/n 
    error=1.e-7 

    do i=0,n
        x(i)=a+h*i
        fx(i)=fun(x(i))
        !print *,i,x(i), fx(i) 
    end do 

    do i=0,n-1
   
        prod=fx(i)*fx(i+1)
        !print *, prod.LT.0.
        if(prod.LT.0.) then 
            print *, 'Raiz en',x(i)
            xa=x(i+1)-fx(i+1)/((fx(i+1)-fx(i))/(x(i+1)-x(i)))
                do j=1,20
                    fa=fun(xa)
                    xb=x(i+1)-fx(i+1)/((fx(i+1)-fa)/(x(i+1)-xa))
                    !print *,j,xb,fun(xb)
                    if(abs(fun(xb)).lt.error) exit
                    xa=xb
                end do
        end if

    end do
end

function fun(x)
    implicit real (a-h, o-z)
    real :: A(3, 3), A_1(3,3), U(3,3) 
    open(unit=10, file='matriz_t5.dat', status='old', action='read')
    do i = 1, 3
        read(10, *) (A(i, j), j = 1, 3)
    end do
    close(10)

    
    U = 0.0
    do i=1,3
        U(i,i) = real(x)
    end do
    A_1 = A - U

  ! Calculamos su determinante
  call calculate_determinant(A_1,det)
    fun = det

endfunction

subroutine calculate_determinant(A, det)
  implicit none
  real, intent(in) :: A(3, 3) ! Matriz de entrada
  real, intent(out) :: det   ! Determinante calculado

  ! Fórmula para el determinante de una matriz 3x3
  det = A(1, 1) * (A(2, 2) * A(3, 3) - A(2, 3) * A(3, 2)) - A(1, 2) * (A(2, 1) * A(3, 3) - A(2, 3) * A(3, 1)) + A(1, 3) * (A(2, 1) * A(3, 2) - A(2, 2) * A(3, 1))

end subroutine calculate_determinant
program e21
   
    real :: x(0:100), y(0:100)
    ! Lectura de los datos de entrada
    n=5
    open(unit=31,file = 'cp.dat', status='OLD')
    do i=0,n
        read(31,*) x(i),y(i)
    enddo
    close(31)

do i=0,n
write(*,*) x(i), y(i)
enddo

do k=0,100
    xe=k
    !Establecemos la forma de nuestro polinomio
    Pol=0
    do i=0,n
        xi=x(i)
        producto=1
        do j=0,n
            if (j /= i) then
            factor = (xe-x(j))/(xi-x(j))
            producto = producto * factor
            print *, producto
            end if
        enddo
        Pol = Pol + y(i) * producto
        
    enddo !Aquí termina el establecimiento del polinomio
    print *, xe, Pol
enddo !Evaluamos los 100 ptos del polinomio interpolante
end

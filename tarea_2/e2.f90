! Interpolación de Newton
program e2

    implicit double precision (a-h, o-z)
    double precision k
    dimension x(0:5), y(0:5), diff(0:5,0:5)


    ! Debemos de evaluar el calor especifico para el aumento en cada grado, 
    ! con lo cual comenzamos cargando los datos del archivo

    n=5 !Numero de datos en el archivo

    do i = 0, n
     diff(i, :) = 0
    end do


    print *,'Temperature values'

    open(unit=10,file= 'cp.dat', status='OLD')
    do i=0,n
        read(10,*) x(i),y(i)
    enddo
    close(10)

    do i=0,n
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


    print *,'Newton interplation coefficients'
    
    do i=0,n
        print *, 'a(',i,')',diff(0,i)
    enddo

    print *, 'Polynomial result'

    open(unit=12,file='interpolation.dat')

    do l=0,200
    s = 0.
    p=1.

    k = real(l)
    k = k*0.5

        !Sumation
        do i=0,n
            a = diff(0,i)
            p=1.

            if(i.gt.0) then

                !product
                do j=0,i-1
                    !print *, p,'*',k,'-',x(j)
                    p=p*(k-x(j))
                enddo

            endif

            !print *,'a',a
                
            s = s+ (a*p)

            !print *,'s',s
        enddo
        fy = s

    write(12,*) k, fy
    print *,'y(',k,') = ' , fy

    enddo

    close(12)
    call system('gnuplot grafica.gp')

    !print *, a(y(1),y(2),x(1),x(2))

    print *, 'Difference between newton and lagrange interpolation'

    do l=0,200
    s = 0.
    p=1.

    k = real(l)
    k = k*0.5

        !Sumation
        do i=0,n
            a = diff(0,i)
            p=1.

            if(i.gt.0) then

                !product
                do j=0,i-1
                    !print *, p,'*',k,'-',x(j)
                    p=p*(k-x(j))
                enddo

            endif

            !print *,'a',a
                
            s = s+ (a*p)

            !print *,'s',s
        enddo
        fn = s
        fl =f(k)

        diference = abs(fn-fl)

    print *,k,'newton',fn,'lagrange' , fl,'diff',diference

    enddo
    


end

function a(y0,y1,x0,x1)

implicit double precision (a-h, o-z)

a = (y1-y0)/(x1-x0)

endfunction


! ----- LAGRANGE INTERPOLATION FUNCTION

function f(k)

    implicit double precision (a-h, o-z) 
    dimension x(0:6), y(0:6)
    double precision k


    ! Debemos de evaluar el calor especifico para el aumento en cada grado, 
    ! con lo cual comenzamos cargando los datos del archivo

    n=5 !Numero de datos en el archivo

    open(unit=10,file= 'cp.dat', status='OLD')
    do i=0,n
        read(10,*) x(i),y(i)
    enddo
    close(10)
    
    s=0.
    sum=0.
    p=1.
    prod=1.

        ! Sumation part
        do i=0,n 
            p=1.
            prod=1.

            ! Product part
            do j=0,n
                if(j.ne.i) then
                    p = (k-x(j))/(x(i)-x(j))
                    prod = prod*p
                endif
            enddo
            s = prod * y(i)
            sum = sum + s
        enddo
    f= sum

endfunction







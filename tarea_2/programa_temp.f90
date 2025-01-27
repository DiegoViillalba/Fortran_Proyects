program interpolacion
real :: x(0:10), y(0:10)
! Lectura de los datos de entrada
n=2
open(unit=31,file= 'datos_temperatura.dat')
do i=0,n
read(31,*) x(i),y(i)

enddo
close(31)

!do i=0,n
!write(*,*) x(i), y(i)
!enddo
!end
open(unit=12,file='tinterpolada.dat')

do j=0,400
xa= 14.0 + j*0.01
Pol=y(0)*(xa-x(1))*(xa-x(2))/((x(0)-x(1))*(x(0)-x(2)))+ &
y(1)*(xa-x(0))*(xa-x(2))/((x(1)-x(0))*(x(1)-x(2)))+ &
y(2)*(xa-x(0))*(xa-x(1)/((x(2)-x(0))*(x(2)-x(1))))
	
write(12,*) xa, Pol

enddo
close(12)

call system('gnuplot grafica_temp.gp')

end



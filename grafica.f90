program fbessel0
open(10,file='funcion.dat')
do i=1,200
x=i/10.
y=bessel_j0(x)
write(10,*) x,y
enddo
close(10)
call system('gnuplot grafica.gp')
end


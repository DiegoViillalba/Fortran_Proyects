
! open(unit=13,file='interpolation_derivative.dat')
!     do k =0,100
!     d =0.
!         ! Sumation part
!         do j=0,n 
!             ! Lagrange factor determination
!             res=0.
!             do l=0,n
!                 if(j.ne.l) then

!                     p = 1/(x(j)-x(l))
!                     !Product part

!                     do m=0,n
!                         if(m.ne.j.and.m.ne.l) then
!                             p = p*(k-x(m))/(x(j)-x(m))
!                             !print*,'p',p
!                         endif
!                     enddo
!                 endif
!                 res = res+p
                
!             enddo
!             !print*,'res',res
!             d = d + (res*y(j))
!             !print *,d
!         enddo
!         !d = -1*(d -433.20520104687523 )
!         !d = -1*(d - 4226.  )
!         print *,'y(',k,') = ' , d
!         write(13,*) k, d
!     end do 
!     close(13)
!     call system('gnuplot grafica2.gp')


!---------- CLASIC INTERPOLATION ALGORITM---

!  open(unit=12,file='interpolation.dat')
!     do k =0,100
!     s=0.
!     sum=0.
!     p=1.
!     prod=1.

!         ! Sumation part
!         do i=0,n 
!             p=1.
!             prod=1.

!             ! Product part
!             do j=0,n
!                 if(j.ne.i) then
!                     p = (k-x(j))/(x(i)-x(j))
!                     prod = prod*p
!                 endif
!             enddo
!             s = prod * y(i)
!             sum = sum + s
!         enddo
!         fy = sum
!         print *,'y(',k,') = ' , fy
!         write(12,*) k, fy
!     end do 
!     close(12)

!     call system('gnuplot grafica.gp')

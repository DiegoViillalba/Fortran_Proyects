

 program main
   implicit none
   integer :: myid, numprocs, ierr
   integer :: iter
   double precision   :: x, y, dcount, total
   integer, parameter :: niter = 1000000000
   integer,parameter :: seed = 86456
   real :: num, t1,t2
   !
   call cpu_time(t1)
   call init_random_seed(100)
   !
   dcount = 0d0
   do iter = 0, niter
     call RANDOM_NUMBER(num)
     x = num
     call RANDOM_NUMBER(num)
     y = num
     if(sqrt(x**2+y**2).le.1d0) dcount = dcount + 1
   end do 
   dcount = dcount / dble(niter)*4
   call cpu_time(t2)
   !
   print*,dcount, t2-t1
   !
 end program main

 subroutine init_random_seed(val)
   implicit none
   INTEGER :: i, n, clock, val
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed
   CALL RANDOM_SEED(size = n)
   ALLOCATE(seed(n))
   CALL SYSTEM_CLOCK(COUNT=clock)
   seed = clock + 37 * (/ (i - 1, i = 1, n) /) * val
   CALL RANDOM_SEED(PUT = seed)
   DEALLOCATE(seed)
 end subroutine init_random_seed



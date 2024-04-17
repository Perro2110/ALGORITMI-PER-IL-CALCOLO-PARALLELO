#DEFINE parral
Program pollo
    use OMP_LIB
    implicit none
    
    integer               :: i
    integer, parameter    :: N=1e3
    real                  :: dx,sum,f,pi,t0,t1,x
    
    i=1
    call omp_set_num_threads(4) !lancio con 4 ncpu
    

    dx  = 1.0/real(N)
    sum = 0.0

#ifdef parral
!$OMP PARALLEL
    t0 = omp_get_wtime()
!$omp do
    do i=1,N
        x =dx*(real(i)-0.5)
        f= 4./(1.+x**2)
        sum =sum + f
    enddo
    pi =sum*dx
 !$omp end do
    t1=OMP_GET_WTIME()
!$OMP END PARALLEL
#endif

#ifdef gallina
call CPU_TIME(t0)

    do i=1,N
        x =dx*(real(i)-0.5)
        f= 4./(1.+x**2)
        sum =sum + f
    enddo
pi = sum*dx

call CPU_TIME(t1)    
#endif
        
    print*,"error = ",abs(pi-ACOS(-1.0))
    print*,"computetional time = ",t1-t0
    print*,"pi=",pi
    
    continue
    



end program pollo
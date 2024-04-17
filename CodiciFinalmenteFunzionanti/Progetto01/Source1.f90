SUBROUTINE Pi_num
#ifdef RUNPARALLEL  
  USE OMP_LIB
#endif  
  IMPLICIT NONE
  INTEGER :: i
  INTEGER :: Nh=1e6
  REAL    :: h, sum, pi, x, f, err
  REAL    :: t1, t2
  
  sum = 0.0
  h   = 1./REAL(Nh)
  
#ifdef OPTION1  
  
  ! Option 1: parallelize the DO loop

  !$OMP PARALLEL DO
  DO i =1, Nh
    x   = h*(REAL(i)-0.5)
    f   = 4. / (1.+x*x)
    sum = sum + f
  ENDDO  
  !$OMP END PARALLEL DO
  
#elif OPTION2  
  
  ! Option 2: parallelize the DO loop with CRITICAL sum

  !$OMP PARALLEL DO
  DO i =1, Nh
    x   = h*(REAL(i)-0.5)
    f   = 4. / (1.+x*x)
    !$OMP CRITICAL
    sum = sum + f
    !$OMP END CRITICAL
  ENDDO  
  !$OMP END PARALLEL DO  
  
#elif OPTION3  
  
  ! Option 3: parallelize the DO loop with CRITICAL sum and explicit f
  ! FALSE-PARALLELIZATION

  !$OMP PARALLEL DO
  DO i =1, Nh
    !$OMP CRITICAL
    sum = sum + 4. / (1.+(h*(REAL(i)-0.5))**2)
    !$OMP END CRITICAL
  ENDDO  
  !$OMP END PARALLEL DO  
  
#elif OPTION4  
  
  ! Option 4: parallelize the DO loop with TASK

  !$OMP PARALLEL  
  !$OMP TASK
  DO i =1, Nh
    x   = h*(REAL(i)-0.5)
    f   = 4. / (1.+x*x)
    !$OMP CRITICAL
    sum = sum + f
    !$OMP END CRITICAL
  ENDDO  
  !$OMP END TASK
  !$OMP END PARALLEL
  
#elif OPTION5  
  
  ! Option 5: parallelize the DO loop with CRITICAL sum and PRIVATE(x,f)

  t1 = OMP_GET_WTIME()
  !$OMP PARALLEL DO PRIVATE(x,f)
  DO i =1, Nh
    x   = h*(REAL(i)-0.5)
    f   = 4. / (1.+x*x)
    !$OMP CRITICAL
    sum = sum + f
    !$OMP END CRITICAL
  ENDDO 
  !$OMP END PARALLEL DO
  t2 = OMP_GET_WTIME()
  
#elif OPTION6  
  
  ! Option 6: parallelize the DO loop with REDUCTION sum and PRIVATE(x,f)

  t1 = OMP_GET_WTIME() 
  !$OMP PARALLEL DO PRIVATE(x,f) REDUCTION(+:sum)
  DO i =1, Nh
    x   = h*(REAL(i)-0.5)
    f   = 4. / (1.+x*x)
    sum = sum + f
  ENDDO  
  !$OMP END PARALLEL DO  
  t2 = OMP_GET_WTIME()
  
#else

  ! Serial code
  CALL CPU_TIME(t1)
  DO i =1, Nh
    x   = h*(REAL(i)-0.5)
    f   = 4. / (1.+x*x)
    sum = sum + f
  ENDDO  
  CALL CPU_TIME(t2)
  
#endif

  pi = h*sum
  
  PRINT *, ' Pi = ', pi
  PRINT *, ' Error = ', ABS(pi-ACOS(-1.0))
  PRINT *, ' Time  = ', t2-t1 
  CONTINUE
  
END SUBROUTINE Pi_num  
  
  
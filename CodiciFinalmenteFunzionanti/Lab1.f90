PROGRAM Lab1
  ! -------------------------------------------------- !
  USE vardef_mod
  USE StatisticOp_mod
  USE SortingOp_mod
  IMPLICIT NONE
  INTEGER  :: i, i_max, i_min
  REAL     :: x_av, sigma, x_max, x_min
  ! -------------------------------------------------- !
  
  ! Initialize input data vector x
  CALL RANDOM_NUMBER(x)
  WRITE(*,*) ' Initial values of x: '
  DO i = 1, N
      WRITE(*,'(f14.10)') x(i)  
  ENDDO  
  
  ! Compute the average
  x_av = Average(x,N)
  WRITE(*,*) ' '
  WRITE(*,'(a25,2x,f14.10)') ' Average value = ', x_av
  
  ! Compute the standard deviation
  sigma = StandardDeviation(x,N)
  WRITE(*,*) ' '
  WRITE(*,'(a25,2x,f14.10)') ' Standard deviation = ', sigma
  
  ! Find max and min values and location in array x
  CALL FindMaxMin(x,N,x_max,x_min,i_max,i_min)
  WRITE(*,*) ' '
  WRITE(*,*) ' Max value=', x_max
  WRITE(*,*) ' Min value=', x_min
  WRITE(*,*) ' Max loc  =', i_max
  WRITE(*,*) ' Min loc  =', i_min
  
  ! Order increasingly the entries of x
  CALL OrderPlus(x,N)
  WRITE(*,*) ' Ordered values of x: '
  DO i = 1, N
      WRITE(*,'(f14.10)') x(i)  
  ENDDO
  
END PROGRAM Lab1  
PROGRAM Laplace
  ! ------------------------------------------------------------------------------------------ !
  IMPLICIT NONE
  ! ------------------------------------------------------------------------------------------ !
  INTEGER, PARAMETER   :: N = 100            ! mesh size
  INTEGER, PARAMETER   :: maxIter = 5000     ! max number of iterations of Jacobi solver
  REAL,    PARAMETER   :: tol  = 1e-7        ! tolerance for stopping Jacobi solver
  REAL,    PARAMETER   :: TBND = 100         ! Boundary condition
  !
  REAL,    ALLOCATABLE :: T(:,:), Tnew(:,:)  ! Current and next solution
  INTEGER              :: iter, i, j
  REAL                 :: res
  ! ------------------------------------------------------------------------------------------ !
  
  ! Allocate variables
  ALLOCATE( T(0:N+1,0:N+1)    )
  ALLOCATE( Tnew(0:N+1,0:N+1) )
  
  ! Initialize variables
  T(0:N,0:N) = 0.0
  T(N+1,1:N) = (/ (i,i=1,N) /) * (TBND/REAL(N+1))  ! right BC
  T(1:N,N+1) = (/ (i,i=1,N) /) * (TBND/REAL(N+1))  ! top BC
  
  Tnew(:,:)  = T(:,:)
  
  ! Jacobi method for solving the Laplace equation
  
  iter = 0
  res  = 1e12
  DO WHILE( res.GT.tol .AND. iter.LE.maxIter )
    
    iter = iter + 1
    res  = 0.
    DO j = 1, N
      DO i = 1, N
        Tnew(i,j) = 0.25*( T(i-1,j) + T(i+1,j) + T(i,j-1) + T(i,j+1) )
        res       = MAX( res, ABS(Tnew(i,j)-T(i,j)) )
      ENDDO
    ENDDO 
    
    ! overwrite current solution
    T = Tnew
    
    IF( MOD(iter,100).EQ.0 ) THEN
      PRINT *, '[iter,res]=',iter,res
    ENDIF  
    
  ENDDO  
  
  ! Write the final result
  OPEN(UNIT=100,FILE='Laplace2D.dat',STATUS='UNKNOWN',ACTION='WRITE')
  WRITE(100,*) N
  DO j = 1, N
    DO i = 1, N
      WRITE(100,*) T(i,j)
    ENDDO
  ENDDO
  CLOSE(100)
  
  
END PROGRAM Laplace  
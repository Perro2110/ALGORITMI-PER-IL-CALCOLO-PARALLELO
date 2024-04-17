PROGRAM ACP2
  ! -------------------------------------------------- !
  USE OMP_LIB
  IMPLICIT NONE
  ! -------------------------------------------------- !
  INTEGER, PARAMETER :: N    = 500
  INTEGER, PARAMETER :: NCPU = 2
  !
  INTEGER            :: i, j, k
  INTEGER            :: NPRCS
  REAL               :: t0, t1, dt, norm
  REAL, ALLOCATABLE  :: A(:,:), B(:,:), C(:,:)
  ! -------------------------------------------------- !
  
  CALL RANDOM_SEED()  ! initialize random generator
  
  ALLOCATE( A(N,N) )
  ALLOCATE( B(N,N) )
  ALLOCATE( C(N,N) )
  
  DO j = 1, N
    DO i = 1, N
      CALL RANDOM_NUMBER(A(i,j))
      CALL RANDOM_NUMBER(B(i,j))
      C(i,j) = 0.0
    ENDDO
  ENDDO  
  
  ! matrix multiplication C=A*B
  CALL CPU_TIME(t0)
  DO i = 1, N
    DO j = 1, N
      DO k = 1, N
        C(i,j) = C(i,j) + A(i,k)*B(k,j)
      ENDDO  
      !C(i,j) = DOT_PRODUCT(A(i,:),B(:,j))
    ENDDO
  ENDDO  
  CALL CPU_TIME(t1)
  
  PRINT *, ' Computational time: ', t1-t0
  
  ! Print the error norm of the matrix C
  norm = 0.
  DO j = 1, N
    DO i = 1, N
      norm = norm + A(i,j)**2  
    ENDDO
  ENDDO  
  norm = SQRT(norm)
  
  PRINT *, ' Frobenius norm of C: ', norm
  CONTINUE
  
END PROGRAM  
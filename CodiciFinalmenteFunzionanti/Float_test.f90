PROGRAM Float_test
  ! -------------------------------------------------- !
  IMPLICIT NONE
  ! -------------------------------------------------- !
  INTEGER, PARAMETER   :: N=2000  ! array size
  
  INTEGER              :: i, j, k, flop
  REAL                 :: res, dt1, dt2, dt3, t0, t1
  REAL,    ALLOCATABLE :: v1(:), v2(:)
  REAL,    ALLOCATABLE :: A(:,:), B(:,:), C(:,:)
  ! -------------------------------------------------- !
  
  CALL RANDOM_SEED()
  
  ! scalar product
  ALLOCATE( v1(N) )
  ALLOCATE( v2(N) )
  DO i = 1, N
    CALL RANDOM_NUMBER(v1(i))
    CALL RANDOM_NUMBER(v2(i))
  ENDDO  
  
  flop = 0
  res  = 0.0
  DO i = 1, N
    res  = res  + v1(i)*v2(i)
    flop = flop + 2
  ENDDO  
  
  ! intrinsic function
  res = DOT_PRODUCT(v1,v2)
  
  DEALLOCATE( v1, v2 )
  
  
  ! matrix multiplication
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
  
  flop = 0
  DO i = 1, N
    DO j = 1, N
      DO k = 1, N
        C(i,j) = C(i,j) + A(i,k)*B(k,j)
        flop   = flop   + 2
      ENDDO
      !C(i,j) = C(i,j) + DOT_PRODUCT(A(i,:),B(:,j))
    ENDDO
  ENDDO  
  ! total cost of matrix-matrix multiplication: 2*N^3
  
  ! computational efficiency
  
  ! DO loop
  CALL CPU_TIME(t0)
  C = 0.0
  DO i = 1, N
    DO j = 1, N
      DO k = 1, N
        C(i,j) = C(i,j) + A(i,k)*B(k,j)
      ENDDO
    ENDDO
  ENDDO 
  CALL CPU_TIME(t1)
  dt1 = t1-t0
  
  ! matmul
  CALL CPU_TIME(t0)
  C = MATMUL(A,B)
  CALL CPU_TIME(t1)
  dt2 = t1-t0
  
  ! MKL (routine dgemm)
  CALL CPU_TIME(t0)
  CALL DGEMM('N','N',N,N,N,1.0,A,N,B,N,0.0,C,N)
  CALL CPU_TIME(t1)
  dt3 = t1-t0
  
  WRITE(*,*) ' DO loop: ', dt1
  WRITE(*,*) ' MATMUL:  ', dt2
  WRITE(*,*) ' dgemm:   ', dt3
  
  DEALLOCATE( A, B, C )
  
  
END PROGRAM Float_test  
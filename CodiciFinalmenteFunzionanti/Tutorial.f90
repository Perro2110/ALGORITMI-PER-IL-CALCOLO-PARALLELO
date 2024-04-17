PROGRAM Tutorial
  
  ! -------------------------------------------------- !
  ! PART I.   VARIABLE DECLARATION                     !
  ! -------------------------------------------------- !
  
  INTEGER              :: i             ! integer number
  REAL                 :: a             ! real number
  CHARACTER(LEN=100)   :: FileName      ! string
  CHARACTER(LEN=100)   :: FileName2     ! string
  INTEGER              :: stdUnit1=1    ! standard output unit  
  INTEGER              :: stdUnit2=2    ! standard output unit  
                       
  REAL                 :: B(2)          ! 1-rank array with size [2]
  REAL                 :: C(2,2)        ! 2-rank array with size [2x2]
  REAL                 :: D(2,2,2)      ! 3-rank array with size [2x2x2]
  REAL, ALLOCATABLE    :: v(:), x(:), y(:)    ! 1-rank array with size [?] 
  REAL, ALLOCATABLE, TARGET :: s(:,:)   ! 2-rank array with size [?x?] 
  REAL, POINTER        :: p(:,:)        ! 2-rank pointer
  
  INTEGER              :: age(10)       ! 1-rank array with size [10]
  INTEGER, ALLOCATABLE :: h(:)          ! 1-rank array with size [?]
  
  INTEGER, PARAMETER   :: N=100         ! integer parameter 
  REAL,    PARAMETER   :: Pi=ACOS(-1.0) ! Pi number 
  !REAL,    PARAMETER   :: Pi=3.14159265358979   
  
  REAL                 :: xL, xR, dx
  
  ! -------------------------------------------------- !
  ! PART II.  MAIN PROGRAM (COMPUTATION)               !
  ! -------------------------------------------------- !
  
  WRITE(*,*) ' Pi=', Pi
  
  ALLOCATE( s(5,2) )  ! 2-rank array with size [5x2]
  
  ALLOCATE( h(5)   )  ! 1-rank array with size [5] 
  
  p => s              ! 2-rank pointer with size [5x2]
  
  ! initialization
  D   = 1.0
  age = 0
  
  age = (/ 20, 25, 30, 35, 40, 45, 50, 55, 60, 65 /)
  
  DO i = 1, 10
    
    PRINT *, ' [i,age]=', i, age(i) 
    
    ! .GT. >
    ! .GE. >=
    ! .LT. <
    ! .LE. <=
    ! .EQ. ==
    ! .NE. =/
    
    IF(age(i).GT.45) THEN
      PRINT *, ' You are getting old! '
    ELSE
      PRINT *, ' Enjoy your life! '
    ENDIF 
    
    IF(age(i)>=60) THEN
      CONTINUE  
    ENDIF  
    
  ENDDO  
  CONTINUE
  
  FileName  = 'Quadratic_f.dat'
  FileName2 = 'Cubic_f.dat'
  
  ALLOCATE( v(N) )  ! 1-rank array with size [N]
  ALLOCATE( y(N) )  ! 1-rank array with size [N]
  ALLOCATE( x(N) )  ! 1-rank array with size [N]
  
  xL = -1.0
  xR =  1.0
  dx = (xR-xL)/REAL(N-1)   ! INT(xL)
  DO i = 1, N
    x(i) = xL + (i-1)*dx
    v(i) = x(i)**2
    y(i) = x(i)**3
  ENDDO  
  
  OPEN(UNIT=stdUnit1,FILE=TRIM(FileName),ACTION='WRITE')
  OPEN(UNIT=stdUnit2,FILE=TRIM(FileName2),ACTION='WRITE')
  
  ! Quadratic_f
  WRITE(stdUnit1,*) N
  DO i = 1, N
    WRITE(stdUnit1,*) x(i)
  ENDDO  
  DO i = 1, N
    WRITE(stdUnit1,*) v(i)
  ENDDO  
    
  ! Cubic_f
  WRITE(stdUnit2,*) N
  DO i = 1, N
    WRITE(stdUnit2,*) x(i)
  ENDDO  
  DO i = 1, N
    WRITE(stdUnit2,*) y(i)
  ENDDO  
  
  CLOSE(stdUnit1)
  CLOSE(stdUnit2)
  
  ! -------------------------------------------------- !
  ! PART III. FINALIZATION                             !
  ! -------------------------------------------------- !
  
  DEALLOCATE( v )
  DEALLOCATE( s )
  
  DEALLOCATE( h )
  
  CONTINUE
  
END PROGRAM Tutorial  
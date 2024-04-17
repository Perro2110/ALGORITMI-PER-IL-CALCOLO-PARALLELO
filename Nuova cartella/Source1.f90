PROGRAM Heat2D_OpenMP
  ! ---------------------------------------------------------------------- !
#ifdef RUNPARALLEL
  USE OMP_LIB 
#endif
  IMPLICIT NONE
  INTEGER :: i, j, n, timestep
  INTEGER :: IMAX, JMAX   ! total number of cells in x and y directions
  REAL    :: xL, xR, yB, yT  ! left, right, bottom, and top coords of the domain
  REAL    :: dx, dy, dx2, dy2 ! mesh space (and its squared value) in x and y directions
  REAL    :: CFL     ! CFL number (<=1 for stability of EXPLICIT SCHEMES) DUBBIO
  REAL    :: time    ! current time
  REAL    :: dt      ! time step
  REAL    :: tend    ! final time of the simulation
  REAL    :: TL, TR, TB, TT  ! boundary conditions
  REAL    :: kappa   ! heat conduction coefficient 
  !
  REAL, ALLOCATABLE  :: T(:,:), Tnew(:,:), x(:), y(:)
  INTEGER, PARAMETER :: NMAX=1e6  ! max number of time steps
  
#ifdef RUNPARALLEL    
  INTEGER, PARAMETER :: NCPU = 4        ! number of threads for parallel simulations
  INTEGER            :: NPRCS           ! number of available threads
#endif   

#ifdef IMPLICIT_SCHEME
  REAL               :: tmp
  REAL, ALLOCATABLE  :: av(:,:), bv(:,:), cv(:,:), rhs(:,:) 
#endif
  ! ---------------------------------------------------------------------- !
  
#ifdef RUNPARALLEL
    !$ WRITE(*,*) ' Parallel simulation with OpenMP directives. '
    NPRCS = OMP_GET_NUM_PROCS()
    CALL OMP_SET_NUM_THREADS(NCPU)
    !$ WRITE(*,*) ' Total number of available CPUs: ', NPRCS
    !$ WRITE(*,*) ' Total number of used CPUs:      ', NCPU
#else
    WRITE(*,*) ' Serial simulation. '
#endif

#ifdef IMPLICIT_SCHEME
    CFL = 5.0
    WRITE(*,*) ' IMPLICIT discretization of the 2D heat equation. '
    WRITE(*,*) ' CFL number = ', CFL
#else
    CFL = 0.9
    WRITE(*,*) ' EXPLICIT discretization of the 2D heat equation. '
    WRITE(*,*) ' CFL number = ', CFL
#endif

  ! Domain definition
  IMAX = 100
  JMAX = 100
  xL   = 0.0
  xR   =  2.0
  yB   = 0.0
  yT   =  2.0
  ! Boundary conditions
  TL   = 100.
  TR   = 50.
  TB   = 75. ! dubbio
  TT   = 25. !
  
  time = 0.0
  tend = 0.05
  
  kappa= 1.0
  
  ALLOCATE( x(IMAX)    )
  ALLOCATE( y(JMAX)    )
  ALLOCATE( T(IMAX,JMAX)    )
  ALLOCATE( Tnew(IMAX,JMAX) )
#ifdef IMPLICIT_SCHEME
  ALLOCATE( av(IMAX,JMAX)   )
  ALLOCATE( bv(IMAX,JMAX)   )
  ALLOCATE( cv(IMAX,JMAX)   )
  ALLOCATE( rhs(IMAX,JMAX)  )
#endif
  
  WRITE(*,*) ' Building the computational domain... '
  
  dx = (xR-xL)/REAL(IMAX-1)
  dy = (yT-yB)/REAL(JMAX-1)
  dx2= dx**2
  dy2= dy**2
  
  x(1) = xL
  DO i = 1, IMAX-1
    x(i+1) = x(i) + dx
  ENDDO  
  y(1) = yB
  DO j = 1, JMAX-1
    y(j+1) = y(j) + dy
  ENDDO
  
  WRITE(*,*) ' Assigning the initial condition... '
  
  DO i = 1, IMAX
    DO j = 1, JMAX
      IF(x(i).LE.0.0 .AND. y(j).LE.0.0) THEN
        T(i,j) = TL
      ELSE IF (x(i).GT.0.0 .AND. y(j).LE.0.0) THEN
        T(i,j) = TR
      ELSE IF (x(i).LE.0.0 .AND. y(j).GT.0.0) THEN
        T(i,j) = TB
      ELSE
        T(i,j) = TT
      ENDIF
    ENDDO
  ENDDO
  CALL PlotOutput(IMAX,JMAX,x,y,T,0)
  
  WRITE(*,*) ' START OF THE COMPUTATION '

#ifdef RUNPARALLEL  
  !$OMP PARALLEL
#endif
  DO n = 1, NMAX   ! main loop in time
    
    IF(time.GE.tend) EXIT

#ifdef RUNPARALLEL    
    !$OMP SINGLE
#endif
    dt = 0.5*CFL*MIN(dx2,dy2)/kappa
    IF(time+dt.GT.tend) THEN
      dt = tend-time  ! adjust the last time step in order to exactly match tend
    ENDIF 
#ifdef RUNPARALLEL    
    !$OMP END SINGLE
    !$OMP BARRIER
#endif
    
    ! NUMERICAL SCHEME
    
#ifdef IMPLICIT_SCHEME

    ! IMPLICIT finite difference method
    
    ! assemble the vectors of the tridiagonal linear system
    tmp = kappa*dt/MIN(dx2,dy2)
#ifdef RUNPARALLEL    
    !$OMP WORKSHARE
#endif
     av(:,:)  = -tmp
     bv(:,:)  = 1.+2.*tmp
     cv(:,:)  = -tmp
     rhs(:,:) = T(:,:)
#ifdef RUNPARALLEL    
    !$OMP END WORKSHARE
#endif 
    ! add boundary conditions
#ifdef RUNPARALLEL
    !$OMP SINGLE
#endif
    rhs(:,1)    = rhs(:,1)    - av(:,1)*TB
    rhs(:,JMAX) = rhs(:,JMAX) - cv(:,JMAX)*TT
    rhs(1,:)    = rhs(1,:)    - av(1,:)*TL
    rhs(IMAX,:) = rhs(IMAX,:) - cv(IMAX,:)*TR
#ifdef RUNPARALLEL
    !$OMP END SINGLE
#endif

    ! solve the system using Thomas algorithm
#ifdef RUNPARALLEL
    !$OMP SINGLE
#endif
    !CALL Thomas2D(IMAX,JMAX,Tnew,av,bv,cv,rhs)
    CALL CG2D(IMAX,JMAX,Tnew,rhs,kappa,dt,dx2,dy2)   
#ifdef RUNPARALLEL
    !$OMP END SINGLE
#endif

#else

    ! EXPLICIT finite difference method
#ifdef RUNPARALLEL
    !$OMP DO
#endif
    DO i = 2, IMAX-1
      DO j = 2, JMAX-1
        Tnew(i,j) = T(i,j) + kappa*dt*(((T(i+1,j)-2.*T(i,j)+T(i-1,j))/dx2) + ((T(i,j+1)-2.*T(i,j)+T(i,j-1))/dy2))
      ENDDO
    ENDDO
#ifdef RUNPARALLEL    
    !$OMP END DO
#endif
    ! Update boundary conditions
    Tnew(:,1)    = TB
    Tnew(:,JMAX) = TT
    Tnew(1,:)    = TL
    Tnew(IMAX,:) = TR
    
#endif

    ! Update time and current solution
#ifdef RUNPARALLEL
    !$OMP SINGLE
#endif
    time = time + dt
    T(:,:) = Tnew(:,:)
    timestep = n
#ifdef RUNPARALLEL    
    !$OMP END SINGLE
#endif

  ENDDO !n  
#ifdef RUNPARALLEL  
  !$OMP END PARALLEL
#endif  
  WRITE(*,*) ' END OF THE COMPUTATION '
  
  CALL PlotOutput(IMAX,JMAX,x,y,T,timestep)
  
  ! Empty memory
  DEALLOCATE( T, Tnew, x, y )
#ifdef IMPLICIT_SCHEME
  DEALLOCATE( av, bv, cv, rhs ) 
#endif
  
END PROGRAM Heat2D_OpenMP

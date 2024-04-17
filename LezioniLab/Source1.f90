PROGRAM Source1
USE omp_lib

INTEGER :: a         !integer number
INTEGER :: b         !integer number
INTEGER :: c         !integer number
INTEGER :: d         !integer number
INTEGER :: e         !integer number
INTEGER :: f         !integer number
INTEGER :: g         !integer number
INTEGER :: h         !integer number
REAL    :: start,fin,start2,fin2,delta,delta2,delta3,start3,fin3 !real number
INTEGER :: NPRCS 

NPRCS = OMP_GET_NUM_PROCS()

! $ OMP PARALLEL
strat=OMP_GET_WTIME()
    ! $ OMP SECTIONS 
        a = 2 
        b = 3 
    ! $ OMP SECTION
        c = 4
        d = 5
    ! $ OMP SECTION 
        e = 6 
        f = 7
    ! $ OMP SECTION 
        g = 8
        h = 9
    ! $ OMP END SECTIONS 
fin=OMP_GET_WTIME()   
strat3=OMP_GET_WTIME()
! $ OMP WORKSHARE
        a = 2 
        b = 3 
        c = 4
        d = 5
        e = 6 
        f = 7
        g = 8
        h = 9
! $ OMP END WORKSHARE
fin3=OMP_GET_WTIME()



! $ OMP END PARALLEL


start2=OMP_GET_WTIME()
        a = 2 
        b = 3 
        c = 4
        d = 5
        e = 6 
        f = 7
        g = 8
        h = 9
fin2=OMP_GET_WTIME()

delta=fin-start
delta2=fin2-start2
delta3=fin3-start3
        PRINT *,NPRCS,delta,delta2,delta3
        CONTINUE


    

END PROGRAM  Source1
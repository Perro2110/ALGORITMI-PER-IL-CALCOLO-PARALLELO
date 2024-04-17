        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 19 10:01:31 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MATOP_HEAT1D__genmod
          INTERFACE 
            SUBROUTINE MATOP_HEAT1D(N,AP,P,KAPPA,DT,DX2)
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: AP(N)
              REAL(KIND=4) :: P(N)
              REAL(KIND=4) :: KAPPA
              REAL(KIND=4) :: DT
              REAL(KIND=4) :: DX2
            END SUBROUTINE MATOP_HEAT1D
          END INTERFACE 
        END MODULE MATOP_HEAT1D__genmod

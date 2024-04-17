        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 19 10:01:31 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THOMAS__genmod
          INTERFACE 
            SUBROUTINE THOMAS(N,X,A,B,C,D)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(OUT) :: X(N)
              REAL(KIND=4) :: A(N)
              REAL(KIND=4) :: B(N)
              REAL(KIND=4) :: C(N)
              REAL(KIND=4) :: D(N)
            END SUBROUTINE THOMAS
          END INTERFACE 
        END MODULE THOMAS__genmod

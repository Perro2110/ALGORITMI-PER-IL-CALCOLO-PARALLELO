MODULE StatisticOp_mod
  PUBLIC 
  
  CONTAINS

REAL FUNCTION Average(a,dim)
  IMPLICIT NONE
  INTEGER :: dim
  REAL    :: a(dim)
  INTEGER :: i
  
  !! Option 1
  !Average = 0.0
  !DO i = 1, dim
  !  Average = Average + a(i)
  !ENDDO  
  !Average = Average / REAL(dim)
  
  ! Option 2
  Average = SUM(a)/REAL(dim)
  
END FUNCTION Average

REAL FUNCTION StandardDeviation(a,dim)
  IMPLICIT NONE
  INTEGER :: dim
  REAL    :: a(dim)
  INTEGER :: i
  REAL    :: av
  
  StandardDeviation = 0.0
  av = Average(a,dim)
  
  DO i = 1, dim
    StandardDeviation = StandardDeviation + (a(i)-av)**2
  ENDDO  
  StandardDeviation = SQRT(StandardDeviation/REAL(dim))
  
END FUNCTION StandardDeviation
  
END MODULE StatisticOp_mod  
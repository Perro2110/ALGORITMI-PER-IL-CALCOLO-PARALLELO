MODULE SortingOp_mod
  PUBLIC 
  
  CONTAINS
  
SUBROUTINE FindMaxMin(a,dim,a_max,a_min,i_max,i_min)  
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: dim
  REAL,    INTENT(IN)  :: a(dim)
  REAL,    INTENT(OUT) :: a_max, a_min
  INTEGER, INTENT(OUT) :: i_max, i_min
  INTEGER              :: i 
  

  ! Option 1
  a_max = -1e20
  a_min =  1e20
  DO i = 1, dim
    IF(a(i)<a_min) THEN
      a_min = a(i)
      i_min = i
    ENDIF
    IF(a(i)>a_max) THEN
      a_max = a(i)
      i_max = i
    ENDIF
  ENDDO  
  
  ! Option 2
  a_max = MAXVAL(a)
  a_min = MINVAL(a)
  i_max = MAXLOC(a,1)
  i_min = MINLOC(a,1)

END SUBROUTINE FindMaxMin

SUBROUTINE OrderPlus(a,dim)  
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: dim
  REAL                 :: a(dim)
  INTEGER              :: i, j
  REAL                 :: temp
  
  DO i = 2, dim
    temp = a(i)
    DO j = i-1,1,-1
      IF(a(j)<temp) THEN
        EXIT
      ENDIF  
      a(j+1) = a(j)
    ENDDO  
    a(j+1) = temp 
  ENDDO  
  
END SUBROUTINE OrderPlus  
  
END MODULE SortingOp_mod  
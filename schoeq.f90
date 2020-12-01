IMPLICIT NONE
REAL*8 :: Y(3), DT = 0.0, Time = 0.0, K = 0.0, Psi = 0.0, NPsi = 0.0
INTEGER :: I 
CHARACTER (20) :: FileName
PRINT *, "What should the ouptput file be called?"
READ *, FileName
OPEN(20, File = FileName)
K = 1.0
! Initial conditions
   Y(1) = 1.0	
    Y(2) = 0.01
     Y(3) = 0.0
! Time step
DT = 0.01
DO I = 1, 400
    Psi = Y(1) * exp(- Y(3) * Y(3) / 2.)
   NPsi = Y(1) * exp(- Y(3) * Y(3) / 2.)
    Time = REAL((I - 1)) * DT
   WRITE (*,*) Time, Y(1), Y(2), Y(3), Psi, NPsi
   WRITE (20,*) Time, -Time, Y(1), Y(2), Y(3), Psi, Npsi
   CALL RK4 (Y, Time, DT)
END DO
STOP
CONTAINS
! ------------------------------------------------------------------------------
SUBROUTINE FDT(Y, Time, F)
REAL*8 :: Y(3), F(3), Time
       F(1) = Y(2)
       F(2) = 2. * Y(3) * Y(2) - (K - 1.) * Y(1)
       F(3) = 1.0

RETURN
END SUBROUTINE FDT
! ------------------------------------------------------------------------------
SUBROUTINE RK4 (Y, Time, DT)
REAL*8 :: YH(3), Y(3), DT, Y1DT(3), Y2DT(3), Y3DT(3), Y4DT(3), Time
INTEGER :: L
! Calculate k1 = FDT(t, y(t))
CALL FDT(Y, Time, Y1DT)
! ------------------------------------------------------------------------------
! Calculate k2 = FDT(t + DT, y(t)+ 0.5*DT K1)
DO L = 1, 3
   YH(L) = Y(L) + 0.5 * DT * Y1DT(L)
END DO
CALL FDT(YH, Time + DT / 2., Y2DT)
! ------------------------------------------------------------------------------
! Calculate k3 = FDT(t + DT/2, y(t)+ 0.5*DT K2)
DO L = 1, 3
   YH(L) = Y(L) + 0.5 * DT * Y2DT(L)
END DO
CALL FDT(YH, Time + DT / 2., Y3DT)
! ------------------------------------------------------------------------------
! Calculate k4 = FDT(t + DT, y(t) + dt K3)
DO L = 1, 3
   YH(L) = Y(L) + DT * Y3DT(L)
END DO
CALL FDT(YH, Time + DT, Y4DT)
! ------------------------------------------------------------------------------
! RK4 step
DO L = 1, 4
   Y(L) = Y(L) + (Y1DT(L) + 2. * Y2DT(L) + 2. * Y3DT(L) + Y4DT(L)) * DT / 6.
END DO
RETURN
END SUBROUTINE RK4
END PROGRAM
	PROGRAM stellar_model
!
! Purpose:
!
!	This program compares stellar models from the linear model assumed in 
!	the homework and the more realistic model from Appendix L data sets from C&O.
!       
!
! History:
!
!    Version   Programmer         Date       Description
!    -------   ----------         --------   -----------
!    1.0       J. Mckenzie        09/27/15   created
!
! Notes:
!
!	None.
!
! ------------------ Variable declarations -----------------------------

	IMPLICIT NONE
	INTEGER :: k, i
	REAL :: R, m_r_over_M, r_over_R, rho_over_rhoc, dr, dm_r, P_over_Pc
	REAL, DIMENSION(1000) :: M_CO, r_CO, P_CO, rho_CO
	REAL, DIMENSION(1000) :: normalized_r, normalized_M, normalized_P, normalized_rho

! ------------------- Parameter Declarations ---------------------------

	REAL, PARAMETER :: pi = ACOS(-1.0)
	 
! ------------------ Input/Output files --------------------------------

	OPEN ( UNIT=10, FILE='M_vs_R.dat', STATUS='UNKNOWN' )
	OPEN ( UNIT=20, FILE='rho_vs_R.dat', STATUS='UNKNOWN' )
	OPEN ( UNIT=30, FILE='C_and_O.dat', STATUS='OLD' )
	OPEN ( UNIT=40, FILE='CO_M_vs_R.dat', STATUS='UNKNOWN' )
	OPEN ( UNIT=50, FILE='P_over_Pc.dat', STATUS='UNKNOWN' )
	OPEN ( UNIT=60, FILE='CO_P_vs_R.dat', STATUS='UNKNOWN' )
	OPEN ( UNIT=70, FILE='CO_rho_vs_R.dat', STATUS='UNKNOWN' )


! ------------------- Code ---------------------------------------------


!  Initializing mass and radius fractions for central values equal to zero


	m_r_over_M = 0. ; dm_r = .0001
	r_over_R = 0.; R = 1. ; dr = .0001



!  Read loop to sort through Appendix L data from C&O and obtain
!  values for radius, mass, pressure, and density for the sunlike star

	
	k = 1 
100	read (30,200, end=300) r_CO(k), M_CO(k), P_CO(k), rho_CO(k)
200	format (7x,ES9.3,13x,ES9.3,24x,ES9.3,2x,ES9.3)


!  Normalizing values for radius, mass, density and pressure so that
!  they be used as fractions, and writing to respective output files


	normalized_r(k) = r_CO(k) / ( 1.05991 * 6.96E8)
	normalized_M(k) = 1. - M_CO(k)
	
	normalized_rho(k) = rho_CO(k) / 7.531E04
	normalized_P(k) = P_CO(k) / 1.531E16
		
	write (40,*) normalized_r(k), normalized_M(k)
	write (60,*) normalized_r(k), normalized_P(k) 
	write (70,*) normalized_r(k), normalized_rho(k)

	k = k + 1
	
	  GO TO 100

300	CONTINUE


!  DO WHILE loop to calculate mass, pressure, and density fractions derived from
!  the linear stellar model

	  
	DO WHILE ( r_over_R < R ) 
	 
	  m_r_over_M = 12. * ( (1./3.)*(r_over_R)**3 - (1./4.)*(r_over_R)**4 )
	  write (10,*) r_over_R, m_r_over_M

	  P_over_Pc = ( 1. - (4./25.)* ( 24.*(r_over_R)**2 + 9.*(r_over_R)**4 - 28.*(r_over_R)**3 ) ) 
	  write (50,*) r_over_R, P_over_Pc

	  rho_over_rhoc = 1. - r_over_R
	  write (20,*) r_over_R, rho_over_rhoc
		
!  Updating radius (mass shell layer)

	  r_over_R = r_over_R + dr
		
	END DO 

	CLOSE (10); CLOSE(20); CLOSE(30); CLOSE(40); CLOSE(50)
	 
	STOP
	 
	END PROGRAM stellar_model

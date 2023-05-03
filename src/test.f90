
!===============================================================================

module test_m

	use cali_m
	implicit none

contains

!===============================================================================

end module test_m

!===============================================================================

program main

	use test_m

	print *, 'hello from test main'
	print *, ''

	call exit(EXIT_FAILURE)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================


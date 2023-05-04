
!===============================================================================

module test_m

	use cali_m
	implicit none

contains

!===============================================================================

subroutine test_ppm_1(npass, nfail)

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: ppm_filename = "build/test-1.ppm"
	integer(kind = 4), allocatable :: cv(:,:), cv2(:,:)

	allocate(cv(3,2))

	cv(1,1) = new_color(int(z'ff0000ff',8))
	cv(2,1) = new_color(int(z'00ff00ff',8))
	cv(3,1) = new_color(int(z'0000ffff',8))

	cv(1,2) = new_color(int(z'ffff00ff',8))
	cv(2,2) = new_color(int(z'ffffffff',8))
	cv(3,2) = new_color(int(z'000000ff',8))

	call write_img(cv, ppm_filename)
	cv2 = read_img(ppm_filename)
	if (all(cv == cv2)) then
		npass = npass + 1
	else
		nfail = nfail + 1
	end if

end subroutine test_ppm_1

!===============================================================================

end module test_m

!===============================================================================

program main

	use test_m
	implicit none

	integer :: npass, nfail

	write(*,*) repeat("=", 50)
	write(*,*) "starting cali test suite ..."
	write(*,*)

	npass = 0
	nfail = 0

	call test_ppm_1(npass, nfail)

	write(*,*)
	write(*,*) "********"
	write(*,*) str(npass)//" tests passed"
	write(*,*) str(nfail)//" tests failed"
	write(*,*) "********"
	write(*,*)

	if (nfail == 0) then
		write(*,*) FG_BRIGHT_GREEN//"success!"//COLOR_RESET
		write(*,*) repeat("=", 50)
		call exit(EXIT_SUCCESS)
	else
		write(*,*) FG_BOLD_BRIGHT_RED//"failure :("//COLOR_RESET
		write(*,*) repeat("=", 50)
		call exit(EXIT_FAILURE)
	end if

end program main

!===============================================================================


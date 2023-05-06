
!===============================================================================

module test_m

	use cali_m
	implicit none

contains

!===============================================================================

subroutine test_ppm_1(npass, nfail)

	! Round-trip write/read test for a simple 3x2 pixel image

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

subroutine test_ppm_2(npass, nfail)

	! Round-trip write/read test for a larger image of overlapping rectangles

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: ppm_filename = "build/test-2.ppm"
	integer(kind = 4), allocatable :: cv(:,:), cv2(:,:)

	allocate(cv(300,200))

	cv                     = new_color(int(z'232323ff',8))
	cv( 10: 200,  10: 150) = new_color(int(z'bc4523ff',8))
	cv(100: 290,  50: 190) = new_color(int(z'12fe09ff',8))
	cv(110: 190,  70: 130) = new_color(int(z'7638adff',8))

	call write_img(cv, ppm_filename)
	cv2 = read_img(ppm_filename)
	if (all(cv == cv2)) then
		npass = npass + 1
	else
		nfail = nfail + 1
	end if

end subroutine test_ppm_2

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
	call test_ppm_2(npass, nfail)

	! TODO:
	! - utf tests
	! - typesetting tests after API is stable

	write(*,*)
	write(*,*) "********"
	write(*,*) to_str(npass)//" tests passed"
	write(*,*) to_str(nfail)//" tests failed"
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


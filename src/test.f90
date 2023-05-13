
!===============================================================================

module test_m

	use cali_m
	use utf_m

	implicit none

contains

!===============================================================================

subroutine test_utf(npass, nfail)

	! Round-trip utf-8 to utf-32 and back to utf-8 testing

	integer, intent(inout) :: npass, nfail

	!********

	!character(len = *, dimension = *) :: strs = &
	character(len = *), parameter :: strs(*) = &
		[ &
			"Hello             ", &
			"021324354657      ", &
			"Καλλι        "     , &
			"∮ E⋅da        "    , &
			"გთხოვთ"            , &
			"Привет      "        &
		]

	integer :: i

	write(*,*) "testing round-trip utf ..."

	do i = 1, size(strs, 1)
		!print *, 'str = ', strs(i)

		if (strs(i) == to_utf8(to_utf32(strs(i)))) then
			npass = npass + 1
		else
			nfail = nfail + 1
		end if

	end do

end subroutine test_utf

!===============================================================================

subroutine test_to_utf_32(npass, nfail)

	! One-way utf-8 to utf-32 testing

	integer, intent(inout) :: npass, nfail

	!********

	integer(kind = 4), allocatable  :: str32(:), expect(:)

	character(len = :), allocatable :: str

	write(*,*) "testing one-way utf ..."

	! Test characters from https://rosettacode.org/wiki/UTF-8_encode_and_decode#C
	str = "AöЖ€𝄞"
	str32 = to_utf32(str)
	!print *, 'str32 = '
	!print '(z0)', str32

	expect = &
		[ &
			int(z'41'   ), &
			int(z'F6'   ), &
			int(z'416'  ), &
			int(z'20AC' ), &
			int(z'1D11E')  &
		]

	if (all(str32 == expect)) then
		npass = npass + 1
	else
		nfail = nfail + 1
	end if

end subroutine test_to_utf_32

!===============================================================================

subroutine test_ppm_1(npass, nfail)

	! Round-trip write/read test for a simple 3x2 pixel image

	integer, intent(inout) :: npass, nfail

	!********

	character(len = *), parameter :: ppm_filename = "build/test-1.ppm"
	integer(kind = 4), allocatable :: cv(:,:), cv2(:,:)

	cv = new_canvas(3, 2, new_color(int(z"000000ff",8)))

	cv(1,1) = new_color(int(z"ff0000ff",8))
	cv(2,1) = new_color(int(z"00ff00ff",8))
	cv(3,1) = new_color(int(z"0000ffff",8))

	cv(1,2) = new_color(int(z"ffff00ff",8))
	cv(2,2) = new_color(int(z"ffffffff",8))
	cv(3,2) = new_color(int(z"000000ff",8))

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

	cv = new_canvas(300, 200, new_color(int(z"232323ff",8)))

	cv( 10: 200,  10: 150) = new_color(int(z"bc4523ff",8))
	cv(100: 290,  50: 190) = new_color(int(z"12fe09ff",8))
	cv(110: 190,  70: 130) = new_color(int(z"7638adff",8))

	call write_img(cv, ppm_filename)
	cv2 = read_img(ppm_filename)
	if (all(cv == cv2)) then
		npass = npass + 1
	else
		nfail = nfail + 1
	end if

end subroutine test_ppm_2

!===============================================================================

subroutine test_cm(npass, nfail)

	! Test computer modern font, roman and italic

	integer, intent(inout) :: npass, nfail

	!********

	character(len = :), allocatable :: str
	character(len = *), parameter :: sfx = "cm"
	character(len = *), parameter :: ppm_filename = "build/test-"//sfx//".ppm"

	double precision :: pix_per_em

	integer :: height, width, line_height, lmargin
	integer(kind = 4) :: fg, fg2, fg3, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:), cv2(:,:)

	type(ttf_t)  :: ttf, ttfi

	ttf  = read_ttf('./fonts/computer-modern/cmunrm.ttf')
	ttfi = read_ttf('./fonts/computer-modern/cmunti.ttf')  ! italic

	! foreground/background colors
	fg  = new_color(int(z'000000ff',8))
	fg2 = new_color(int(z'ffffffff',8))
	fg3 = new_color(int(z'2a7fffff',8))
	bg  = new_color(int(z'e8e6cbff',8))
	bg2 = fg3

	cv = new_canvas(800, 945, bg)
	cv(:, 631:) = bg2

	! TODO: make these global parameters shared by multiple testing routines
	pix_per_em = 75.d0
	line_height = nint(1.2 * pix_per_em)
	lmargin = 20

	! TODO: remove str var from tests, hard-code literal for brevity
	str = "Computer Modern"
	call draw_str(cv, fg, ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "Aa Ee Rr"
	call draw_str(cv, fg, ttf , str, lmargin, 2 * line_height, pix_per_em)
	call draw_str(cv, fg, ttfi, str, lmargin, 3 * line_height, pix_per_em)

	str = "t"
	call draw_str(cv, fg2, ttf, str, 600, 4 * line_height, 5 * pix_per_em)

	str = "matrix"
	call draw_str(cv, fg3, ttf, str, 450, 5 * line_height, pix_per_em)

	! TODO: increase spacing for remaining strs
	str = "abcdefghijklm"
	call draw_str(cv, fg, ttf , str, lmargin, 8 * line_height, pix_per_em)

	str = "nopqrstuvwxyz"
	call draw_str(cv, fg, ttf , str, lmargin, 9 * line_height, pix_per_em)

	str = "0123456789"
	call draw_str(cv, fg2, ttf , str, 200, 10 * line_height, pix_per_em)

	call write_img(cv, ppm_filename)

	cv2 = read_img("./data/test-cm.ppm")
	if (all(cv == cv2)) then
		npass = npass + 1
	else
		nfail = nfail + 1
	end if

end subroutine test_cm

!===============================================================================

subroutine test_ubuntu(npass, nfail)

	! Test computer modern font, roman and italic

	integer, intent(inout) :: npass, nfail

	!********

	character(len = :), allocatable :: str
	character(len = *), parameter :: sfx = "ubuntu"
	character(len = *), parameter :: ppm_filename = "build/test-"//sfx//".ppm"

	double precision :: pix_per_em

	integer :: height, width, line_height, lmargin
	integer(kind = 4) :: fg, fg2, fg3, fg4, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:), cv2(:,:)

	type(ttf_t)  :: ttf, ttfi

	ttf  = read_ttf('./fonts/ubuntu/Ubuntu-Regular.ttf')
	ttfi = read_ttf('./fonts/ubuntu/Ubuntu-Italic.ttf')

	! foreground/background colors
	fg  = new_color(int(z'333333ff',8))
	fg2 = new_color(int(z'717073ff',8))
	fg3 = new_color(int(z'f47421ff',8))
	fg4 = new_color(int(z'ffffffff',8))
	bg  = fg4
	bg2 = new_color(int(z'621a4bff',8))

	cv = new_canvas(800, 945, bg)
	cv(:, 631:) = bg2

	! TODO: make these global parameters shared by multiple testing routines
	pix_per_em = 75.d0
	line_height = nint(1.2 * pix_per_em)
	lmargin = 20

	str = "Ubuntu"
	call draw_str(cv, fg, ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "Aa Ee Rr"
	call draw_str(cv, fg2, ttf , str, lmargin, 2 * line_height, pix_per_em)
	call draw_str(cv, fg2, ttfi, str, lmargin, 3 * line_height, pix_per_em)

	str = "a"
	call draw_str(cv, fg2, ttf, str, 600, 4 * line_height, 5 * pix_per_em)

	str = "Narwhal"
	call draw_str(cv, fg3, ttf, str, 450, 5 * line_height, pix_per_em)

	! TODO: increase spacing for remaining strs
	str = "abcdefghijklm"
	call draw_str(cv, fg4, ttf , str, lmargin, 8 * line_height, pix_per_em)

	str = "nopqrstuvwxyz"
	call draw_str(cv, fg4, ttf , str, lmargin, 9 * line_height, pix_per_em)

	str = "0123456789"
	call draw_str(cv, fg4, ttf , str, 200, 10 * line_height, pix_per_em)

	call write_img(cv, ppm_filename)

end subroutine test_ubuntu

!===============================================================================

subroutine test_garamond(npass, nfail)

	! Test computer modern font, roman and italic

	integer, intent(inout) :: npass, nfail

	!********

	character(len = :), allocatable :: str
	character(len = *), parameter :: sfx = "garamond"
	character(len = *), parameter :: ppm_filename = "build/test-"//sfx//".ppm"

	double precision :: pix_per_em

	integer :: height, width, line_height, lmargin
	integer(kind = 4) :: fg, fg2, fg3, fg4, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:), cv2(:,:)

	type(ttf_t)  :: ttf, ttfi

	ttf  = read_ttf('./fonts/cormorant-garamond/CormorantGaramond-Regular.ttf')
	ttfi = read_ttf('./fonts/cormorant-garamond/CormorantGaramond-Italic.ttf')

	! foreground/background colors
	fg  = new_color(int(z'414142ff',8))
	fg2 = new_color(int(z'000000ff',8))
	fg3 = new_color(int(z'ffffffff',8))
	fg4 = new_color(int(z'929497ff',8))
	bg  = new_color(int(z'e9e4d1ff',8))
	bg2 = new_color(int(z'd7a676ff',8))

	cv = new_canvas(800, 945, bg)
	cv(:, 631:) = bg2

	! TODO: make these global parameters shared by multiple testing routines
	pix_per_em = 75.d0
	line_height = nint(1.2 * pix_per_em)
	lmargin = 20

	str = "Garamond"
	call draw_str(cv, fg, ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "Aa Qq Rr"
	call draw_str(cv, fg2, ttf , str, lmargin, 2 * line_height, pix_per_em)
	call draw_str(cv, fg2, ttfi, str, lmargin, 3 * line_height, pix_per_em)

	str = "á"
	call draw_str(cv, fg2, ttf, str, 600, 4 * line_height, 5 * pix_per_em)

	str = "TRIANON"
	call draw_str(cv, fg3, ttf, str, 350, 5 * line_height, pix_per_em)

	! TODO: increase spacing for remaining strs
	str = "abcdefghijklm"
	call draw_str(cv, fg4, ttf , str, lmargin, 8 * line_height, pix_per_em)

	str = "nopqrstuvwxyz"
	call draw_str(cv, fg4, ttf , str, lmargin, 9 * line_height, pix_per_em)

	! TODO ???
	str = "0123456789"
	call draw_str(cv, fg4, ttf , str, 200, 10 * line_height, pix_per_em)

	call write_img(cv, ppm_filename)

end subroutine test_garamond

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

	call test_ppm_1    (npass, nfail)
	call test_ppm_2    (npass, nfail)
	call test_utf      (npass, nfail)
	call test_to_utf_32(npass, nfail)
	call test_cm       (npass, nfail)
	call test_ubuntu   (npass, nfail)
	call test_garamond (npass, nfail)
	! TODO: bodoni after verifying specimen in existing tests.  add cmd arg to
	! baseline tests

	! TODO:
	! - to_char8/32 test
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


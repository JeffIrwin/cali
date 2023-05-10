
!===============================================================================

module app_m

	use cali_m
	implicit none

	type args_t
		character(len = :), allocatable :: ttf_file
	end type args_t

contains

function parse_args() result(args)

	type(args_t) :: args

	!********

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc, io

	argc = command_argument_count()
	!print *, 'argc = ', argc
	if (argc /= 1) then
		write(*,*) error//'bad cmd args'
		write(*,*) 'Usage:'
		write(*,*) '	cali path/to/font.ttf'
		call exit(EXIT_FAILURE)
	end if

	call get_command_argument(1, buffer, status = io)
	if (io /= EXIT_SUCCESS) then
		write(*,*) error//'cannot get cmd arg'
		call exit(EXIT_FAILURE)
	end if
	argv = trim(buffer)
	!print *, 'argv = ', argv

	! Positional arg
	args%ttf_file = argv

end function parse_args

end module app_m

!===============================================================================

program main

	use app_m
	use cali_m
	!use utf8_m

	implicit none

	character(len = :), allocatable :: str

	double precision :: pix_per_em

	integer :: height, width, line_height, lmargin
	integer(kind = 4) :: fg, fg2, fg3, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:) ! canvas

	type(args_t) :: args
	type(ttf_t)  :: ttf, ttfi

	write(*,*) 'cali 0.0.1'
	write(*,*)

	args = parse_args()
	ttf  = read_ttf(args%ttf_file)
	ttfi = read_ttf('./fonts/computer-modern/cmunti.ttf')

	! foreground/background colors
	fg  = new_color(int(z'000000ff',8))
	fg2 = new_color(int(z'ffffffff',8))
	fg3 = new_color(int(z'2a7fffff',8))
	bg  = new_color(int(z'e8e6cbff',8))
	bg2 = fg3

	cv = new_canvas(800, 945, bg)
	cv(:, 631:) = bg2

	pix_per_em = 100.d0
	line_height = nint(1.2 * pix_per_em)
	lmargin = 20

	!str = "a b!"
	!call draw_str(cv, fg , ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "Computer Modern"
	!str = "Καλλι"
	!str = "Привет"
	call draw_str(cv, fg , ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "Aa Ee Rr"
	!str = "Aa 🔥Ee Rr"
	call draw_str(cv, fg , ttf , str, lmargin, 2 * line_height, pix_per_em)

	str = "Aa Ee Rr"
	call draw_str(cv, fg , ttfi, str, lmargin, 3 * line_height, pix_per_em)

	str = "t"
	call draw_str(cv, fg2, ttf, str, 600, 4 * line_height, 5 * pix_per_em)

	str = "Matrix"
	call draw_str(cv, fg3, ttf, str, 450, 5 * line_height, pix_per_em)

	call write_img(cv, 'test.ppm')

	write(*,*) FG_BRIGHT_GREEN//'Finished cali'//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================



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

	implicit none

	type(args_t) :: args
	type(ttf_t)  :: ttf

	write(*,*) 'cali 0.0.1'
	write(*,*)

	args = parse_args()
	ttf  = read_ttf(args%ttf_file)

	call draw_glyph(ttf%glyphs(69))
	!call draw_glyph(ttf%glyphs(98))

	!print *, 'num_tables  = ', ttf%num_tables
	!print *, 'tag 1       = ', ttf%tables(1)%tag
	!print *, 'glyf table  = ', ttf%get_table('glyf')

	write(*,*) FG_BRIGHT_GREEN//'Finished cali'//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================


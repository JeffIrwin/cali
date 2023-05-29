
!===============================================================================

module app_m

	use cali_m
	implicit none

	! ISO 639-1 codes
	!
	! TODO: move this to lib?
	character(len = *), parameter :: langs(*) = &
		[ &
			"de", &  ! german
			"el", &  ! greek
			"en", &  ! english
			"fr", &  ! french
			"ru", &  ! russian
			"zu"  &  ! zulu
		]

	type args_t
		character(len = :), allocatable :: ttf_file
		logical :: waterfall = .false.
		integer :: language
	end type args_t

contains

!===============================================================================

!function get_lang(char_code_iso_619_1) result(code)
!	character(len = *), intent(in) :: char_code_iso_619_1
!	integer :: code
!end function get_lang

!===============================================================================

function parse_args() result(args)

	type(args_t) :: args

	!********

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: i, io, argc, ipos

	logical :: lerror = .false.

	argc = command_argument_count()
	!print *, "argc = ", argc

	ipos = 0
	do i = 1, argc

		call get_command_argument(i, buffer, status = io)
		if (io /= EXIT_SUCCESS) then
			write(*,*) ERROR//"cannot get cmd arg"
			call exit(EXIT_FAILURE)
		end if
		argv = trim(buffer)
		!print *, "argv = ", argv

		select case (argv)
		case ("-w", "--waterfall")
			args%waterfall = .true.

		case default
			! Positional arg
			ipos = ipos + 1

			if (ipos == 1) then
				args%ttf_file = argv
			else
				write(*,*) ERROR//"unknown positional arg `"//argv//"`"
				lerror = .true.
			end if

		end select

	end do

	if (ipos < 1) then
		write(*,*) ERROR//"ttf file not defined"
		lerror = .true.
	end if

	if (lerror) then
		write(*,*) "Usage:"
		write(*,*) "	cali <path/to/font.ttf>"
		write(*,*) "	cali -w <path/to/font.ttf>"
		write(*,*)
		write(*,*) "Options:"
		write(*,*) "	-w --waterfall  waterfall specimen"
		write(*,*)
		call exit(EXIT_FAILURE)
	end if

end function parse_args

!===============================================================================

end module app_m

!===============================================================================

program main

	use app_m
	use cali_m

	implicit none

	type(args_t) :: args

	write(*,*) "cali "// &
		to_str(CALI_MAJOR)//"."// &
		to_str(CALI_MINOR)//"."// &
		to_str(CALI_PATCH)
	write(*,*)

	args = parse_args()

	if (args%waterfall) then
		call waterfall(args%ttf_file, 12.d0, 64.d0, 8)
	else
		call specimen(args%ttf_file)
	end if

	write(*,*) FG_BRIGHT_GREEN//"Finished cali"//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================


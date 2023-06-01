
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
			"hi", &  ! hindi
			"ru", &  ! russian
			"zu"  &  ! zulu
		]

	type args_t

		character(len = :), allocatable :: ttf_file
		character(len = :), allocatable :: language

		logical :: &
			waterfall = .false., &
			version   = .false., &
			help      = .false.

	end type args_t

contains

!===============================================================================

function parse_args() result(args)

	type(args_t) :: args

	!********

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: i, io, argc, ipos

	logical :: lerror = .false.

	! Defaults
	args%language = "en"

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = 0
	ipos = 0
	do while (i < argc)
		i = i + 1

		call get_command_argument(i, buffer, status = io)
		if (io /= EXIT_SUCCESS) then
			write(*,*) ERROR//"cannot get cmd arg"
			call exit(EXIT_FAILURE)
		end if
		argv = trim(buffer)
		!print *, "argv = ", argv

		! TODO: more args:
		!   - color randomizer seed, relative salt or absolute
		!   - output filename/dir

		select case (argv)
		case ("-h", "--help")
			args%help = .true.

		case ("-l", "--language")
			! TODO: fn.  helpful error if i > argc
			i = i + 1
			call get_command_argument(i, buffer, status = io)
			if (io /= EXIT_SUCCESS) then
				write(*,*) ERROR//"cannot get cmd arg"
				call exit(EXIT_FAILURE)
			end if

			args%language = trim(buffer)
			! TODO: check language is in langs

		case ("--version")
			args%version = .true.

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

	if (ipos < 1 .and. .not. (args%help .or. args%version)) then
		write(*,*) ERROR//"ttf file not defined"
		lerror = .true.
	end if

	if (lerror .or. args%help) then

		write(*,*) "Usage:"
		write(*,*) "	cali <path/to/font.ttf> [-l <lang>] [-w]"
		write(*,*) "	cali -h | --help"
		write(*,*) "	cali --version"
		write(*,*)
		write(*,*) "Options:"
		write(*,*) "	-h --help             Show this help"
		write(*,*) "	-l --language <lang>  Language code ISO 639-1"
		write(*,*) "	--version             Show version"
		write(*,*) "	-w --waterfall        Waterfall specimen"
		write(*,*)

		if (.not. args%help) call exit(EXIT_FAILURE)
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
	if (args%version .or. args%help) call exit(EXIT_SUCCESS)

	if (args%waterfall) then
		call waterfall(args%ttf_file, 12.d0, 64.d0, 8, args%language)
	else
		! TODO: lang arg
		call specimen(args%ttf_file)
	end if

	write(*,*) FG_BRIGHT_GREEN//"Finished cali"//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================


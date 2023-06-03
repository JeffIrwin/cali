
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
			"it", &  ! italian
			"ru", &  ! russian
			"zu"  &  ! zulu
		]

	type args_t

		character(len = :), allocatable :: ttf_file, language, out_file

		logical :: &
			waterfall = .false., &
			version   = .false., &
			lout_file = .false., &
			help      = .false.

	end type args_t

contains

!===============================================================================

subroutine get_next_arg(i, argv0, argv)
	integer, intent(inout) :: i
	character(len = :), allocatable, intent(out) :: argv
	character(len = *), intent(in) :: argv0
	!********
	character(len = 1024) :: buffer
	integer :: io

	!i = 0
	!argv = ""
	!io = 0

	i = i + 1
	call get_command_argument(i, buffer, status = io)
	if (io /= EXIT_SUCCESS) then
		write(*,*) ERROR//"cannot get cmd arg after """//argv0//""""
		call exit(EXIT_FAILURE)
	end if
	argv = trim(buffer)
	!print *, "argv = ", argv

end subroutine get_next_arg

!===============================================================================

function parse_args() result(args)

	type(args_t) :: args

	!********

	character(len = :), allocatable :: argv, argv0

	integer :: i, argc, ipos

	logical :: lerror = .false.

	! Defaults
	args%language = "en"

	argc = command_argument_count()
	!print *, "argc = ", argc

	i = -1
	ipos = 0
	call get_next_arg(i, "", argv)
	argv0 = argv

	do while (i < argc)
		call get_next_arg(i, argv0, argv)
		argv0 = argv

		! TODO: more args:
		!   - color randomizer seed, relative salt or absolute
		!   - output filename/dir

		select case (argv)
		case ("-h", "--help", "-help")
			args%help = .true.

		case ("-l", "--language")
			call get_next_arg(i, argv0, argv)
			argv0 = argv
			args%language = argv
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
			else if (ipos == 2) then
				args%lout_file = .true.
				args%out_file  = argv
			else
				write(*,*) ERROR//"unknown argument `"//argv//"`"
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

	args = parse_args()
	if (args%version .or. args%help) then
		write(*,*) "Homepage: github.com/JeffIrwin/cali"
		write(*,*)
		call exit(EXIT_SUCCESS)
	end if
	write(*,*)

	if (args%waterfall) then
		call waterfall(args%ttf_file, 12.d0, 64.d0, 8, args%language)
	else
		! TODO: lang arg
		call specimen(args%ttf_file)
	end if

	if (args%lout_file) print *, "out_file = ", args%out_file

	write(*,*) FG_BRIGHT_GREEN//"Finished cali"//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================



!===========================================================================

module utf_m

	! This was mechanically translated from the C example on Rosetta code:
	!
	!     https://rosettacode.org/wiki/UTF-8_encode_and_decode#C

	implicit none

	character, parameter :: LINE_FEED = char(10)

	integer, parameter :: NUTF_T = 5

	! Char data will undergo bitwise and with this
	integer(kind = 2), parameter :: &
		utf_mask(NUTF_T) =        &
		[                &
			int(b'00111111',2), &
			int(b'01111111',2), &
			int(b'00011111',2), &
			int(b'00001111',2), &
			int(b'00000111',2)  &
		]

	! Start bytes of current char in utf-8 encoded character
	integer(kind = 2), parameter :: &
		utf_lead(NUTF_T) =        &
		[                &
			int(b'10000000',2), &
			int(b'00000000',2), &
			int(b'11000000',2), &
			int(b'11100000',2), &
			int(b'11110000',2)  &
		]

	! Beginning of codepoint range
	integer(kind = 4), parameter :: &
		utf_beg(NUTF_T) =        &
		[               &
			int(o'0000000',4), &
			int(o'0000000',4), &
			int(o'0000200',4), &
			int(o'0004000',4), &
			int(o'0200000',4) &
		]

	! End of codepoint range
	integer(kind = 4), parameter :: &
		utf_end(NUTF_T) =         &
		[                &
			int(o'00000000',4), &
			int(o'00000177',4), &
			int(o'00003777',4), &
			int(o'00177777',4), &
			int(o'04177777',4)  &
		]

	! The number of bits from the codepoint that fits in char
	integer, parameter :: &
		utf_bits(NUTF_T) = &
		[      &
			6, &
			7, &
			5, &
			4, &
			3  &
		]

contains

!===========================================================================

! A codepoint or cp is basically utf32

function utf32_len(char32) result(len)
	integer(kind = 4), intent(in) :: char32
	integer :: len
	!********
	integer :: i
	len = 0
	do i = 1, NUTF_T
		if (char32 >= utf_beg(i) .and. char32 <= utf_end(i)) exit
		len = len + 1
	end do
	if (len > 4) then
		write(*,*) 'Error: out of bounds'
		call exit(-2)
	end if
end function utf32_len

!********

function utf8_len(char8) result(len)
	character, intent(in) :: char8
	integer :: len
	!********
	integer :: i
	len = 0
	do i = 1, NUTF_T
		if (iand(iachar(char8), int(ieor(utf_mask(i), int(z'ff',2)),4)) == utf_lead(i)) then
			exit
		end if
		len = len + 1
	end do
	if (len > 4) then
		write(*,*) 'Error: malformed leading byte'
		call exit(-2)
	end if
end function utf8_len

!********

function to_char8(char32) result(ret)
	! Convert 1 character from utf32 to utf8
	integer(kind = 4), intent(in) :: char32
	character(len = :), allocatable :: ret
	!********
	integer :: i, bytes, shift
	bytes = utf32_len(char32)
	allocate(character(len = bytes) :: ret)
	shift = utf_bits(1) * (bytes - 1)
	ret(1:1) =     achar(ior(iand(ishft(char32, -shift), int(utf_mask(bytes+1),4)), &
			int(utf_lead(bytes+1),4)))
	do i = 2, bytes
		shift = shift - utf_bits(1)
		ret(i:i) = achar(ior(iand(ishft(char32, -shift), int(utf_mask(1),4)), &
			int(utf_lead(1),4)))
	end do
end function to_char8

!********

function to_char32(char8) result(codep)
	! Convert 1 character from utf8 to utf32
	character(len = *), intent(in) :: char8
	integer(kind = 4) :: codep
	!********
	integer :: i, bytes, shift

	bytes = utf8_len(char8(1:1))
	!print * , 'bytes = ', bytes
	if (bytes == 0) then
		! TODO: parameterize -1 as bad codepoint.  Maybe 0 instead?
		codep = -1
		return
	end if
	shift = utf_bits(1) * (bytes - 1)
	codep = ishft(iand(iachar(char8(1:1)), int(utf_mask(bytes+1),4)), shift)

	do i = 2, bytes
		shift = shift - utf_bits(1)
		codep = ior(codep, ishft(iand(iachar(char8(i:i)), int(utf_mask(1),4)), shift))
	end do

end function to_char32

!********

function to_utf32(utf8) result(utf32)
	! Convert utf8 string to utf32 string
	character(len = *), intent(in) :: utf8
	integer(kind = 4), allocatable :: utf32(:)
	!********
	integer :: i, j
	integer(kind = 4) :: char32

	!print *, 'starting to_utf32()'
	!print *, 'len = ', len(utf8)
	!print *, ''

	allocate(utf32(len(utf8)))  ! worst-case size (all ASCII)

	i = 1
	j = 1
	do while (i <= len(utf8))
		char32 = to_char32(utf8(i:))
		utf32(j) = char32

		! If we changed to_char32() to return len, this extra len call wouldn't be
		! required
		i = i + utf32_len(char32)

		j = j + 1
	end do
	utf32 = utf32(1: j-1) ! trim

end function to_utf32

!********

function to_utf8(utf32) result(utf8)
	integer(kind = 4), intent(in) :: utf32(:)
	character(len = :), allocatable :: utf8
	!********
	character(len = :), allocatable :: char8
	integer :: i, j

	! Worst-case size (all 4-byte chars)
	allocate(character(len = 4 * size(utf32)) :: utf8)
	!utf8 = repeat(' ', 4 * size(utf32))

	j = 1
	do i = 1, size(utf32)
		char8 = to_char8(utf32(i))
		utf8(j: j + len(char8) - 1) = char8
		j = j + len(char8)
	end do
	utf8 = utf8(1: j-1) ! trim

end function to_utf8

!********

end module utf_m

!===========================================================================


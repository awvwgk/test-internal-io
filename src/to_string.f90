! SPDX-Identifier: MIT
module to_string
   implicit none
   private

   public :: to_string_man, to_string_iio
   public :: i1, i2, i4, i8


   !> Char length for integers
   integer, parameter :: i1 = selected_int_kind(2)

   !> Short length for integers
   integer, parameter :: i2 = selected_int_kind(4)

   !> Length of default integers
   integer, parameter :: i4 = selected_int_kind(9)

   !> Long length for integers
   integer, parameter :: i8 = selected_int_kind(18)


   interface to_string_man
      module procedure :: integer_i1_to_string_man
      module procedure :: integer_i2_to_string_man
      module procedure :: integer_i4_to_string_man
      module procedure :: integer_i8_to_string_man
   end interface to_string_man

   interface to_string_iio
      module procedure :: integer_i1_to_string_iio
      module procedure :: integer_i2_to_string_iio
      module procedure :: integer_i4_to_string_iio
      module procedure :: integer_i8_to_string_iio
   end interface to_string_iio


contains


pure function integer_i1_to_string_man(val) result(string)
   integer(i1), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i1) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i1) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i1)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i1
   end do
   if (val < 0_i1) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i1_to_string_man


pure function integer_i2_to_string_man(val) result(string)
   integer(i2), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i2) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i2) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i2)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i2
   end do
   if (val < 0_i2) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i2_to_string_man


pure function integer_i4_to_string_man(val) result(string)
   integer(i4), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i4) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i4) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i4)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i4
   end do
   if (val < 0_i4) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i4_to_string_man


pure function integer_i8_to_string_man(val) result(string)
   integer(i8), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i8) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i8) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i8)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i8
   end do
   if (val < 0_i8) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i8_to_string_man


pure function integer_i1_to_string_iio(val) result(string)
   integer(i1), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer

   write(buffer, '(i0)') val
   string = trim(buffer)
end function integer_i1_to_string_iio


pure function integer_i2_to_string_iio(val) result(string)
   integer(i2), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer

   write(buffer, '(i0)') val
   string = trim(buffer)
end function integer_i2_to_string_iio


pure function integer_i4_to_string_iio(val) result(string)
   integer(i4), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer

   write(buffer, '(i0)') val
   string = trim(buffer)
end function integer_i4_to_string_iio


pure function integer_i8_to_string_iio(val) result(string)
   integer(i8), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer

   write(buffer, '(i0)') val
   string = trim(buffer)
end function integer_i8_to_string_iio


end module to_string

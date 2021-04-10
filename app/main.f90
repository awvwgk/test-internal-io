! SPDX-Identifier: MIT
program demo
    use to_string
    use iso_fortran_env, only : error_unit
    implicit none
    integer(i1), parameter :: n1 = huge(1_i1), s1 = 19_i1
    integer(i2), parameter :: n2 = huge(1_i2), s2 = 219_i2
    integer(i4), parameter :: n4 = huge(1_i4), s4 = 25519_i4
    integer(i8), parameter :: n8 = huge(1_i8), s8 = int(n4, i8) * 25519_i8
    integer, parameter :: rep = 10
    integer :: i
    integer(i1) :: j1
    integer(i2) :: j2
    integer(i4) :: j4
    integer(i8) :: j8
    real :: ts, te
    real, save :: man = 0.0, iio = 0.0

    do i = 1, rep
        write(error_unit, '(i0, "/", i0)') i, rep
        print *, "man"
        call cpu_time(ts)
        do j1 = -n1, n1, s1
            print *, to_string_man(j1)
        end do
        do j2 = -n2, n2, s2
            print *, to_string_man(j2)
        end do
        do j4 = -n4, n4, s4
            print *, to_string_man(j4)
        end do
        do j8 = -n8, n8, s8
            print *, to_string_man(j8)
        end do
        call cpu_time(te)
        man = man + te - ts
        print *, "---"

        print *, "iio"
        call cpu_time(ts)
        do j1 = -n1, n1, s1
            print *, to_string_iio(j1)
        end do
        do j2 = -n2, n2, s2
            print *, to_string_iio(j2)
        end do
        do j4 = -n4, n4, s4
            print *, to_string_iio(j4)
        end do
        do j8 = -n8, n8, s8
            print *, to_string_iio(j8)
        end do
        call cpu_time(te)
        iio = iio + te - ts
        print *, "==="
    end do

    write(error_unit, *) "man", man
    write(error_unit, *) "iio", iio
end program demo

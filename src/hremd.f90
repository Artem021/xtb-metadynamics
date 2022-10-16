! This file is part of xtb.
!
! Copyright (C) 2019-2020 Sebastian Ehlert
!
! xtb is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! xtb is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with xtb.  If not, see <https://www.gnu.org/licenses/>.


! methods for Hamiltonian replica exchange implementation
module hremd_tools
    use xtb_mctc_accuracy, only : wp
    use mctc_io_read_xyz, only : read_xyz,read_coord
    use mctc_env, only : error_type
    implicit none

    public :: hremd, read_set
    private
    type :: hremd
      logical :: do_hremd
      logical :: external
      logical :: err
      real(wp), allocatable :: xyz(:, :, :)
      real(wp) :: alpha
      integer :: nstruc
    end type hremd
    
 contains

   subroutine test(arg1,arg2)
   logical, intent(in) :: arg1
   logical, intent(out) :: arg2
   write(*,*) "argument in: ", arg1
   write(*,*) "argument out, init: ", arg2
   if (arg2) then
      arg2=.false.
   else
      arg2=.true.
   end if
   write(*,*) "argument out, new: ", arg2
   end subroutine test

   subroutine read_set(unit,err,hremd_type)
      integer, intent(in) :: unit
      type(error_type), allocatable,intent(inout) :: err
      type(hremd),intent(inout) :: hremd_type
      integer :: nat
      real(wp), allocatable :: structures(:,:,:)
      integer :: n=0

      call read_coord(structures,unit,err,n)


      hremd_type%xyz = structures
      hremd_type%nstruc = n

      ! write(*,*) 'xyz: ', structures
      ! write(*,*) 'nframes: ', n
      ! nat = min(size(struc%xyz, 1), size(struc%xyz, 2), size(struc%xyz, 3))
      ! nat = size(struc%xyz,dim=2)

      ! allocate(xyz(3, nat))
      ! allocate(xyz(nat, 3))
      ! xyz = struc%xyz
      ! write(*,*) 'xyz: ', xyz

      
   end subroutine read_set

 
 
 end module hremd_tools
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

!> TODO
module xtb_io_writer
   use mctc_env, only : error_type
   use mctc_io, only : structure_type, write_structure
   use xtb_io_writer_turbomole, only : writeResultsTurbomole
   use xtb_mctc_accuracy, only : wp
   use xtb_mctc_filetypes, only : fileType
   use xtb_mctc_version, only : version
   use xtb_type_molecule, only : TMolecule, assignment(=)
   implicit none
   private

   public :: writeMolecule,writeMolecule2


contains


subroutine writeMolecule(self, unit, format, energy, gnorm, number)
   class(TMolecule), intent(in) :: self
   integer, intent(in) :: unit
   integer, intent(in), optional :: format
   real(wp), intent(in), optional :: energy
   real(wp), intent(in), optional :: gnorm
   integer, intent(in), optional :: number
   character(len=:), allocatable :: comment_line
   character(len=20) :: energy_line
   character(len=20) :: gnorm_line
   type(structure_type) :: struc
   type(error_type), allocatable :: error
   integer :: ftype
   if (present(format)) then
      ftype = format
   else
      ftype = self%ftype
   endif

   comment_line = ''
   if (present(energy)) then
      write(energy_line, '(f20.12)') energy
      comment_line = comment_line // " energy: " // trim(adjustl(energy_line))
   endif
   if (present(gnorm)) then
      write(gnorm_line, '(f20.12)') gnorm
      comment_line = comment_line // " gnorm: " // trim(adjustl(gnorm_line))
   endif
   comment_line = comment_line // " xtb: " // version

   struc = self
   struc%comment = trim(comment_line)
   call write_structure(struc, unit, ftype, error)

   ! Flush file so that the output file can be visualized during optimization
   flush(unit)
end subroutine writeMolecule

subroutine writeMolecule2(self, unit, format, energy, gnorm, vbias)
   class(TMolecule), intent(in) :: self
   integer, intent(in) :: unit
   integer, intent(in), optional :: format
   real(wp), intent(in), optional :: energy
   real(wp), intent(in), optional :: gnorm
   real(wp), intent(in), optional :: vbias
   ! integer, intent(in), optional :: number
   character(len=:), allocatable :: comment_line
   character(len=20) :: energy_line
   character(len=20) :: gnorm_line
   character(len=20) :: vbias_line
   type(structure_type) :: struc
   type(error_type), allocatable :: error
   integer :: ftype
   if (present(format)) then
      ftype = format
   else
      ftype = self%ftype
   endif

   comment_line = ''
   if (present(energy)) then
      write(energy_line, '(f20.12)') energy
      comment_line = comment_line // " energy: " // trim(adjustl(energy_line))
   endif
   ! if (present(gnorm)) then
   !    write(gnorm_line, '(f20.12)') gnorm
   !    comment_line = comment_line // " gnorm: " // trim(adjustl(gnorm_line))
   ! endif
   if (present(vbias)) then
      write(vbias_line, '(f20.12)') vbias
      comment_line = comment_line // " V_BIAS: " // trim(adjustl(vbias_line))
   endif

   comment_line = comment_line // " xtb: " // version

   struc = self
   struc%comment = trim(comment_line)
   call write_structure(struc, unit, ftype, error)

   ! Flush file so that the output file can be visualized during optimization
   flush(unit)
end subroutine writeMolecule2

end module xtb_io_writer

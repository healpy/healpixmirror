!-----------------------------------------------------------------------------
!
!  Copyright (C) 1997-2013 Krzysztof M. Gorski, Eric Hivon,
!                          Benjamin D. Wandelt, Anthony J. Banday, 
!                          Matthias Bartelmann, Hans K. Eriksen, 
!                          Frode K. Hansen, Martin Reinecke
!
!
!  This file is part of HEALPix.
!
!  HEALPix is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 2 of the License, or
!  (at your option) any later version.
!
!  HEALPix is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with HEALPix; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!
!  For more information about HEALPix see http://healpix.sourceforge.net
!
!-----------------------------------------------------------------------------
module extension
  !
  ! defines in F90 some C commands
  ! These extensions are not completely standard in all F90/95 compilers
  !
  ! getEnvironment   : emulates getenv
  ! getArgument      : emulates getarg
  ! nArguments       : emulates iargc
  !
  ! written by Eric Hivon, Nov 2001
  !
  ! exit_with_status : verbose and clean exit, added by M.R.
  ! 2005-08: edited for Gfortran

#ifdef NAG
  USE f90_unix, ONLY : iargc, getarg, exit
#endif
!VF  USE dflib, ONLY : nargs, getarg

  USE healpix_types, ONLY : I4B
  IMPLICIT none

#if ((!defined(NAG)) && (!defined(GFORTRAN)))
interface
  function iargc()
    integer iargc
  end function

  subroutine getarg (num, res)
    integer, intent(in) :: num
    character(len=*), intent(out) :: res
  end subroutine
end interface
#endif

  integer(kind=I4B), private :: arg_shift = 0
!VF  integer(kind=I4B), private :: arg_shift = 1

  private
  public :: getEnvironment, getArgument, nArguments, exit_with_status

  contains

#if (defined (GFORTRAN))

    ! ===========================================================
    function iargc ()
    ! ===========================================================
       integer iargc
       ! ===========================================================

       iargc=command_argument_count()
    end function

    ! ===========================================================
    subroutine getarg(num, res)
    ! ===========================================================
       integer, intent(in) :: num
       character(len=*), intent(out) :: res
       integer l, err
       ! ===========================================================
       call get_command_argument(num,res,l,err)
    end subroutine

#endif

    ! ===========================================================
    function nArguments() result(narg)
      ! ===========================================================
      integer(kind=I4B) :: narg
      ! ===========================================================

      narg = iargc() - arg_shift
!VF      narg = nargs() - arg_shift

      return
    end function nArguments
    ! ===========================================================
    subroutine getEnvironment(name, value)
      ! ===========================================================
      character(len=*), intent(in) :: name
      character(len=*), intent(out) :: value
      integer(kind=I4B) :: inull, lnstr
!       character(len=200) :: name1
      ! ===========================================================
      ! call C routine after adding a trailing NULL to input
      value = ""
      call cgetenvironment(trim(adjustl(name))//char(0), value)
      ! remove trailing NULL (\0) created by C routine on output
      lnstr = len(value)
      inull = index(value, char(0), back=.true.)
      if (inull > 0 .and. inull < lnstr) value(inull:inull) = " "

      return
    end subroutine getEnvironment
    ! ===========================================================
    subroutine getArgument(index, argument)
      ! ===========================================================
      integer(kind=I4B), intent(in) :: index
      character(len=*), intent(out) :: argument
      integer(kind=I4B) :: i1
      ! ===========================================================
      i1 = index + arg_shift
      call getarg(i1, argument)

      return
    end subroutine getArgument

    ! ===========================================================
    subroutine exit_with_status (code, msg)
      ! ===========================================================
      integer, intent(in) :: code
      character (len=*), intent(in), optional :: msg
      ! ===========================================================

      if (present(msg)) print *,trim(msg)
      print *,'program exits with exit code ', code

#if (defined (RS6000))
      call exit_ (code)
#else
      call exit (code)
#endif
    end subroutine exit_with_status

end module extension

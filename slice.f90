module makeslice
  use pointsort
  implicit none
  public ptlist, point, surface, createlist, destroylist, getSurfacefromShell, slice
   type ptlist
      type(ptlist), pointer:: prev
      real(8), dimension(:, :), allocatable:: pts
      integer num_of_pts
      type(ptlist), pointer:: next
   end type ptlist
   type point
      real loc(3)
   end type point
   type surface
      type(point), dimension(:), pointer:: surface
      integer length
   end type surface
contains
   function createlist(data)
      implicit none
      type(ptlist), pointer:: createlist, nx, pv
      integer dimx, dimy, IOstatus, block, sumary
      character(100),intent(IN):: data
      allocate (createlist)
      nullify (nx); nullify (pv); sumary = 0
      open (12, FILE=trim(data), ACTION="READ")
      read (12, *, IOSTAT=IOstatus) dimx, dimy
      block = 1
      dimx = dimx*dimy; createlist%num_of_pts = dimx
      allocate (createlist%pts(dimx, 3))
      createlist%prev => null()
      createlist%next => null()
      do dimy = 1, dimx
         read (12, *, IOSTAT=IOstatus) createlist%pts(dimy, 1:3)
      end do
      print *,"read",dimx,"pts in block",block
      sumary = sumary + dimx
      pv => createlist
      do
         read (12, *, IOSTAT=IOstatus) dimx, dimy
         if (IOstatus .ne. 0) then
            exit
         end if
         block= block + 1
         allocate (nx)
         dimx = dimx*dimy; nx%num_of_pts = dimx
         allocate (nx%pts(dimx, 3))
         nx%prev => pv
         pv%next => nx
         nx%next => null()
         do dimy = 1, dimx
            read (12, *, IOSTAT=IOstatus) nx%pts(dimy, 1:3)
         end do
         print *,"read",dimx,"pts in block",block
         sumary = sumary + dimx
         pv => nx
      end do
      close(12)
      print *,"read totally",sumary,"pts in",block,"blocks"
   end function createlist

   function destroylist(headnode)
      implicit none
      type(ptlist), pointer:: headnode, p
      integer, pointer:: destroylist
      do while (associated(headnode))
         p => headnode%next
         deallocate (headnode%pts)
         deallocate (headnode)
         headnode => p
      end do
      allocate (destroylist)
      destroylist = 0
   end function destroylist

   function getSurfacefromShell(headnode, ctpt, tol, i)
      implicit none
      type(ptlist), pointer:: headnode, p
      real(8) ctpt, tol, X, pin, t
      integer i, num, runner
      type(surface), pointer:: getSurfacefromShell
      logical isnear
      isnear(X, pin, t) = abs(X - pin) .lt. t
      allocate (getSurfacefromShell)
      p => headnode
      open (21, FILE='temp.dat', status="REPLACE")
      num = 0
      do while (associated(p))
         do runner = 1, p%num_of_pts
            if (isnear(p%pts(runner, i), ctpt, tol)) then
               write (21, *) p%pts(runner, 1:3)
               num = num + 1
            end if
         end do
         p => p%next
      end do
      close(21)
      open (21, FILE='temp.dat', status="OLD")
      allocate (getSurfacefromShell%surface(num))
      do runner = 1, num
         read (21, *) getSurfacefromShell%surface(runner)%loc(1:3)
      end do
      close(21)
      getSurfacefromShell%length = num
   end function getSurfacefromShell

   subroutine slice(data, dim, cut, N, tol)
      implicit none
      integer N, emmm, num, Nxx
      character(100),intent(IN):: data
      real(8),dimension(:,:),allocatable:: onesec
      character dim
      character(100):: filename
      real(8) tol, cut(N)
      integer i, j, k, now
      type(ptlist), pointer:: headnode
      type(surface),pointer:: Surfaces
      print *,"creating list..."
      headnode => createlist(data)
      print *,"finish creating list"
      if (dim .eq. 'X') then
         i = 1;j = 2;k = 3
      else if (dim .eq. 'Y') then
         i = 2;j = 1;k = 3
      else if (dim .eq. 'Z') then
         i = 3;j = 1;k = 2
      else
         print *, "wrong parameter"
         N = destroylist(headnode)
         return
      end if
      do now = 1, N
         print *,"cutting No.",now,"pt",cut(now),"on axis",dim
         Surfaces => getSurfacefromShell(headnode, cut(now), tol, i)
         num = Surfaces%length
         print *,"get",num,"pts"
         if (num.eq.0) then
            cycle
         end if
         
         allocate(onesec(num,2))
         do Nxx=1,num
            onesec(Nxx,1) = Surfaces%surface(Nxx)%loc(j)
            onesec(Nxx,2) = Surfaces%surface(Nxx)%loc(k)
         end do
         write (filename, '("slicedata",I2.2,".dat")') now
         call SortSection(onesec,num,filename)
         print *,"done writing ",trim(filename)
         deallocate(onesec)
      end do
      N = destroylist(headnode)
   end subroutine slice

end module makeslice


module pointsort
  use insertpts
  use cal_statics
  implicit none
  public JudgeIntersection,SortSection
  contains
    logical function JudgeIntersection(line)
      implicit none
      real(8),intent(IN):: line(4, 2)
      real(8) x1, x2, y1, y2, a, b, c, d, e, x, y
      x1 = line(1, 1); y1 = line(1, 2); x2 = line(2, 1); y2 = line(2, 2)
      a = y2 - y1; b = (x1 - x2)/a; c = (x2*y1 - x1*y2)/a;
      x1 = line(3, 1); y1 = line(3, 2); x2 = line(4, 1); y2 = line(4, 2)
      a = y2 - y1; d = (x1 - x2)/a; e = (x2*y1 - x1*y2)/a;
      y = (e - c)/(b - d); x = -c - b*y
      if (((x .GE. line(1, 1)) .AND. (x .LE. line(2, 1))) .OR. &
           ((x .GE. line(3, 1)) .AND. (x .LE. line(4, 1)))) then
         JudgeIntersection = .TRUE.
      else
         JudgeIntersection = .FALSE.
      end if
    end function JudgeIntersection

    subroutine SortSection(onesec, num, name)
      implicit none
      integer,intent(IN):: num
      real(8):: onesec(0:num-1, 0:1)
      integer i, j, pp1, pp2, istart, n, pp3
      real(8),dimension(:,:),allocatable::  part1, part2, part3
      real(8) temp(0:1), len1, len2, a, b, c, d, cos_theta, threshold, theta, xmax, xtol, chord
      real(8) line(4, 0:1), e, f, g, h, len3, len4, area, thickness
      character(100),intent(IN):: name
      allocate(part1(0:num*2-1, 0:1))
      allocate(part2(0:num*2-1, 0:1))
      allocate(part3(0:num*2-1, 0:1))
      threshold = cos(40/57.296)
      do i = 0, num-2
         do j = i + 1, num-1
            if (onesec(i, 0) .gt. onesec(j, 0)) then
               temp(0:1) = onesec(i, 0:1)
               onesec(i, 0:1) = onesec(j, 0:1)
               onesec(j, 0:1) = temp(0:1)
            end if
         end do
      end do
      chord = onesec(num-1, 0) - onesec(0, 0)
      xtol = 1.0e-4
      xmax = onesec(num-1, 0)
      do i = num-1, num - 29, -1
         if (xmax - onesec(i, 0) .gt. xtol*chord) then
            exit
         end if
      end do
      i = i + 1
      do while (i .lt. num-1)
         do j = i + 1, num-1
            if (onesec(i, 1) .lt. onesec(j, 1)) then
               temp(0:1) = onesec(i, 0:1)
               onesec(i, 0:1) = onesec(j, 0:1)
               onesec(j, 0:1) = temp(0:1)
            end if
         end do
         i = i + 1
      end do
      do i = 1, 3
         if (abs(onesec(i, 0) - onesec(0, 0)) .gt. 1.0e-8) then
            exit
         end if
      end do
      n = i
      do i = 0, n-2
         do j = i + 1, n-1
            if (onesec(i, 1) .gt. onesec(j, 1)) then
               temp(0:1) = onesec(i, 0:1)
               onesec(i, 0:1) = onesec(j, 0:1)
               onesec(j, 0:1) = temp(0:1)
            end if
         end do
      end do
      part1(0, 0:1) = onesec(0, 0:1); part2(0, 0:1) = onesec(0, 0:1)
      pp1 = 2; pp2 = 1
      do i = 1, num-1
         if (onesec(i, 1) .gt. onesec(0, 1)) then
            exit
         else
            part2(pp2, 0:1) = onesec(i, 0:1)
            pp2 = pp2 + 1
         end if
      end do
      part1(1, 0:1) = onesec(i, 0:1)
      istart = i; i = i + 1
      do while (i .lt. num)
         if (i .ge. 337) then
            print *, "i=", i
         end if
         if (part1(pp1 - 1, 0) - part1(0, 0) .lt. chord*0.1) then
            threshold = cos(50/57.269)
         else
            threshold = cos(10/57.269)
         end if
         a = part1(pp1 - 1, 0) - part1(pp1 - 2, 0); b = part1(pp1 - 1, 1) - part1(pp1 - 2, 1)
         len1 = sqrt(a*a + b*b); istart = istart + 1
128      do j = istart, num-1
            c = onesec(j, 0) - part1(pp1 - 1, 0); d = onesec(j, 1) - part1(pp1 - 1, 1)
            len2 = sqrt(c*c + d*d)
            if (abs(len2) .lt. 1.0e-10) then
               exit
            end if
            cos_theta = (a*c + b*d)/len1/len2
            if (cos_theta .gt. 1.0) then
               cos_theta = 0.9999997
            end if
            theta = acos(cos_theta)*57.293
            if (i .gt. num - 10) then
               print *, "theta=", theta
            end if
            if (cos_theta .gt. threshold) then
               if ((j .gt. num - 30) .and. (j .lt. num-1)) then
                  e = part2(pp2 - 1, 0) - part2(pp2 - 2, 0)
                  f = part2(pp2 - 1, 1) - part2(pp2 - 2, 1)
                  len3 = sqrt(e*e + f*f)
                  g = onesec(j, 0) - part2(pp2 - 1, 0)
                  h = onesec(j, 1) - part2(pp2 - 1, 1)
                  len4 = sqrt(g*g + h*h)
                  cos_theta = (e*g + f*h)/len3/len4;
                  if (cos_theta .gt. threshold) then
                     line(1, 0:1) = part1(pp1 - 1, 0:1)
                     line(2, 0:1) = onesec(j, 0:1)
                     line(3, 0:1) = part2(pp2 - 1, 0:1)
                     line(4, 0:1) = onesec(j + 1, 0:1)
                     if (JudgeIntersection(line)) then
                        part2(pp2, 0:1) = onesec(j, 0:1)
                        pp2 = pp2 + 1
                        cycle
                     end if
                  end if
               end if
               part1(pp1, 0:1) = onesec(j, 0:1)
               pp1 = pp1 + 1; i = j - 1; istart = j
               exit
            else
               part2(pp2, 0:1) = onesec(j, 0:1)
               pp2 = pp2 + 1
            end if
         end do
         if ((j .eq. num) .and. &
              (cos_theta .lt. threshold) .and. &
              (onesec(num-1, 1) .gt. part1(pp1 - 1, 1))) then
            pp1 = pp1 - 1; pp2 = pp2 - 1
            a = part1(pp1 - 1, 0) - part1(pp1 - 2, 0)
            b = part1(pp1 - 1, 1) - part1(pp1 - 2, 1)
            len1 = sqrt(a*a + b*b)
            print *, "wrong"
            goto 128
         else if (j .eq. num ) then
            exit
         end if
      end do
      
      part2(pp2, 0:1) = onesec(num-1, 0:1); pp2 = pp2 + 1
      do i = 0, pp2-2
         do j = i + 1, pp2-1
            if (part2(i, 0) .gt. part2(j, 0)) then
               temp(0:1) = part2(i, 0:1)
               part2(i, 0:1) = part2(j, 0:1)
               part2(j, 0:1) = temp(0:1)
            end if
         end do
      end do
      xtol=1.0e-4
      xmax=part2(pp2-1,0)
      do i=pp2-1,pp2-29,-1
         if(xmax-part2(i,0).gt.xtol*chord) then
            exit
         end if
      end do
      i=i+1
      do while(i<pp2-1)
         do j = i + 1, pp2-1
            if (part2(i, 1) .gt. part2(j, 1)) then
               temp(0:1) = part2(i, 0:1)
               part2(i, 0:1) = part2(j, 0:1)
               part2(j, 0:1) = temp(0:1)
            end if
         end do
         i=i+1
      end do
      
      
      part3(0, 0:1) = part2(0, 0:1)
      part3(1, 0:1) = part2(1, 0:1)
      pp3 = 2; istart = 1
      
      i = 0
      do while (i .lt. pp2)
         if (part3(pp3 - 1, 0) - part3(0, 0) .lt. chord*0.1) then
            threshold = cos(50/57.269)
         else
            threshold = cos(10/57.269)
         end if
         a = part3(pp3 - 1, 0) - part3(pp3 - 2, 0); b = part3(pp3 - 1, 1) - part3(pp3 - 2, 1)
         len1 = sqrt(a*a + b*b); istart = istart + 1
256      do j = istart, pp2-1
            c = part2(j, 0) - part3(pp3 - 1, 0); d = part2(j, 1) - part3(pp3 - 1, 1)
            len2 = sqrt(c*c + d*d)
            if (abs(len2) .lt. 1.0e-10) then
               exit
            end if
            cos_theta = (a*c + b*d)/len1/len2
            if (cos_theta .gt. 1.0) then
               cos_theta = 0.9999997
            end if
            theta = acos(cos_theta)*57.293
            if (i .gt. pp2 - 5) then
               print *, "theta=", theta
            end if
            if (cos_theta .gt. threshold) then
               part3(pp3, 0:1) = part2(j, 0:1)
               pp3 = pp3 + 1; i = j - 1; istart = j
               exit
            end if
         end do
         if ((j .eq. pp2) .and. &
              (cos_theta .lt. threshold) .and. &
              (part2(pp2 - 1, 1) .lt. part3(pp3 - 1, 1))) then
            pp3 = pp3 - 1
            a = part3(pp3 - 1, 0) - part3(pp3 - 2, 0)
            b = part3(pp3 - 1, 1) - part3(pp3 - 2, 1)
            len1 = sqrt(a*a + b*b)
            goto 256
         else if (j .eq. num) then
            exit
         end if
         i = i + 1
      end do
      print *,"sorting done!"
      pp1=pp1-1;pp3=pp3-1
      call complete(part1,pp1,part3,pp3,num*2)
      open(666,file=trim(name),status='replace')
      do i = 0, pp1-1
         write (666, "(2f15.8)") part1(i, 0:1)
      end do
      write(666,*);write(666,*);write(666,*);write(666,*)
      do i = pp3-1, 0, -1
         write (666, "(2f15.8)") part3(i, 0:1)
      end do
      write(666,*);write(666,*);write(666,*);write(666,*)
      area=cal_area(part1,pp1,part3,pp3,num*2)
      thickness=cal_thickness(part1,pp1,part3,pp3,num*2)
      write (666, *) "Area = ",area
      write (666, *) "thickness = ",thickness
      close(666)
      
      deallocate(part1)
      deallocate(part2)
      deallocate(part3)
    end subroutine SortSection
  end module pointsort

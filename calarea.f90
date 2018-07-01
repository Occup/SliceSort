module cal_statics
  implicit none
  public cal_area,cal_thickness
contains
  function cal_area(ulst,ulim,llst,llim,num)
    integer,intent(IN):: ulim,llim,num
    real(8),intent(IN):: ulst(num,2),llst(num,2)
    real(8):: cal_area
    integer::i,j
    if (ulim .ne. llim) then
       print *,"something wrong with sorted data,cannot get area..."
       return
    end if
    cal_area = 0
    do i=1,ulim-1
       do j=i+1,ulim
          if (ulst(j,1)-ulst(i,1).gt.1e-8.and.ulst(j,1).eq.llst(j,1)) then
             exit
          end if
       end do
       if (j.gt.ulim) then
          exit
       end if
       cal_area = cal_area + &
               (abs(ulst(i,2)-llst(i,2))+abs(ulst(j,2)-llst(j,2))) * abs(ulst(j,1)-ulst(i,1)) / 2
    end do
    print *,"Area = ",cal_area,"m^2"
  end function cal_area

  function cal_thickness(ulst,ulim,llst,llim,num)
    integer,intent(IN):: ulim,llim,num
    real(8),intent(IN):: ulst(num,2),llst(num,2)
    real(8):: cal_thickness
    integer::i
    if (ulim .ne. llim) then
       print *,"something wrong with sorted data,cannot get area..."
       return
    end if
    cal_thickness = 0
    do i = 1,ulim
       if (ulst(i,1).eq.llst(i,1)) then
          if (cal_thickness.lt.abs(ulst(i,2)-llst(i,2))) then
             cal_thickness = abs(ulst(i,2)-llst(i,2))
          end if
       end if
    end do
    print *,"thickness = ",cal_thickness,"m"
  end function cal_thickness
end module cal_statics

    

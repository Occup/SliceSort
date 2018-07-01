module insertpts
  implicit none
  public insertion, complete, addnode
contains
  subroutine addnode(list,num,length,pos,node)
    integer,intent(IN):: num,pos
    integer:: length
    real(8),intent(IN):: node(2)
    real(8):: list(num,2)
    integer::i,j   
    do i=length,pos,-1
       list(i+1,1:2) = list(i,1:2)
    end do
    length = length+1
    list(pos,1:2) = node(1:2)
  end subroutine addnode
  
    
  subroutine insertion(lista,na,listb,nb,num)
    integer:: na,nb,num
    real(8):: listb(num,2),lista(num,2),temp(2)
    integer:: i,j

    do i=2,na
       do j=2,nb
          if (lista(i,1).lt.listb(j,1)) then
             if (lista(i,1).eq.listb(j-1,1)) then
                exit
             end if
             
             temp(1)=lista(i,1)
             temp(2)=listb(j-1,2)+(listb(j,2)-listb(j-1,2))/(listb(j,1)-listb(j-1,1))*(lista(i,1)-listb(j-1,1))
             call addnode(listb,num,nb,j,temp)
             exit
          end if
       end do
       if (j.gt.nb.and.lista(i,1).gt.listb(j-1,1)) then
          temp(1)=lista(i,1)
          temp(2)=listb(j-2,2)+(listb(j-1,2)-listb(j-2,2))/(listb(j-1,1)-listb(j-2,1))*(lista(i,1)-listb(j-2,1))
          call addnode(listb,num,nb,j,temp)
       end if
    end do
  end subroutine insertion

  subroutine complete(lista,na,listb,nb,num)
    integer:: na,nb,num
    real(8):: listb(num,2),lista(num,2)
    print *,"inserting nodes for lowerside..."
    call insertion(lista,na,listb,nb,num)
    print *,"done!"
    print *,"inserting nodes for upperside..."
    call insertion(listb,nb,lista,na,num)
    print *,"done!"
  end subroutine complete
end module insertpts

    

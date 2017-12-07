module mod_detect_nearest_neighbor
    use mod_read_gmsh, only: nbelm,nbnode,coord_nodes,id_nodes
    use mod_cell_2D
    implicit none

    contains
      subroutine detect_neighbor
      implicit none
      integer(4)            :: i,j,k,t
      integer(4)            :: idnode1,idnode2
      integer(4)            :: cnt
      type(cell_2D),pointer :: pc => null()
      type(cell_2D),pointer :: pr => null()

!      do i = 1, nbelm
!        do k = 1, 4
!            write(*,*) cell(i)%p%vertex(k)%ident
!        end do
!        write(*,*)
!      end do

      do i = 1, nbelm
          idnode1 = cell(i)%p%vertex(1)%ident
          idnode2 = cell(i)%p%vertex(2)%ident
          pc => cell(i)%p

          do j = 1, nbelm
            pr => cell(j)%p
            cnt = 0
            if (associated(pc,pr)) cycle
            do t = 1, 4
              if (idnode1 == pr%vertex(t)%ident) cnt = cnt + 1
              if (idnode2 == pr%vertex(t)%ident) cnt = cnt + 1
            end do
            if (cnt == 2) then
              pc%neighbor1 => pr
            end if
          end do

       end do

       do i = 1, nbelm

          idnode1 = cell(i)%p%vertex(2)%ident
          idnode2 = cell(i)%p%vertex(3)%ident
          pc => cell(i)%p

          do j = 1, nbelm
            pr => cell(j)%p
            cnt = 0
            if (associated(pc,pr)) cycle
            do t = 1, 4
              if (idnode1 == pr%vertex(t)%ident) cnt = cnt + 1
              if (idnode2 == pr%vertex(t)%ident) cnt = cnt + 1
            end do
            if (cnt == 2) then
              pc%neighbor2 => pr
            end if
           end do

      end do

      do i = 1, nbelm

          idnode1 = cell(i)%p%vertex(3)%ident
          idnode2 = cell(i)%p%vertex(4)%ident
          pc => cell(i)%p

          do j = 1, nbelm
            pr => cell(j)%p
            cnt = 0
            if (associated(pc,pr)) cycle
            do t = 1, 4
              if (idnode1 == pr%vertex(t)%ident) cnt = cnt + 1
              if (idnode2 == pr%vertex(t)%ident) cnt = cnt + 1
            end do
            if (cnt == 2) then
              pc%neighbor3 => pr
            end if
           end do

       end do

       do i = 1, nbelm


           idnode1 = cell(i)%p%vertex(4)%ident
           idnode2 = cell(i)%p%vertex(1)%ident

           pc => cell(i)%p
           do j = 1, nbelm
             pr => cell(j)%p
             cnt = 0
             if (associated(pc,pr)) cycle
             do t = 1, 4
               if (idnode1 == pr%vertex(t)%ident) cnt = cnt + 1
               if (idnode2 == pr%vertex(t)%ident) cnt = cnt + 1
             end do
             if (cnt == 2) then
               pc%neighbor4 => pr
             end if
            end do

      end do
      end subroutine

end module

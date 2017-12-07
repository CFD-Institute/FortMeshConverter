module mod_cell_2D
    use mod_read_gmsh
    implicit none

    type face
        type(point) :: p1,p2
    end type face

    type cell_2D
        integer(4)  :: ident = 0
        type(point) :: vertex(4)

        type(face),dimension(4) :: faces

        type(cell_2D),pointer :: neighbor1 => null()
        type(cell_2D),pointer :: neighbor2 => null()
        type(cell_2D),pointer :: neighbor3 => null()
        type(cell_2D),pointer :: neighbor4 => null()


    end type cell_2D

    type list_cell_2D
        type(cell_2D),pointer :: p
    end type

    type(list_cell_2D),dimension(:),allocatable :: cell

    contains
       subroutine construct_cells
       implicit none

       integer(4) :: i,j
       integer(4) :: idnode

       allocate(cell(1:nbelm))

       do i = 1, nbelm
          allocate(cell(i)%p)
       end do

       ! Assign vertexes of a cell

       do i = 1, nbelm
         cell(i)%p%ident = i
         cell(i)%p%vertex(1)%ident = id_nodes(i)%pn%id_node(6)
         idnode = cell(i)%p%vertex(1)%ident
         do j = 1, nbnode
            if (idnode == coord_nodes(j)%p%ident) then
                cell(i)%p%vertex(1)%x = coord_nodes(j)%p%x
                cell(i)%p%vertex(1)%y = coord_nodes(j)%p%y
            end if
         end do

         cell(i)%p%vertex(2)%ident = id_nodes(i)%pn%id_node(7)
         idnode = cell(i)%p%vertex(2)%ident
         do j = 1, nbnode
            if (idnode == coord_nodes(j)%p%ident) then
                cell(i)%p%vertex(2)%x = coord_nodes(j)%p%x
                cell(i)%p%vertex(2)%y = coord_nodes(j)%p%y
            end if
         end do

         cell(i)%p%vertex(3)%ident = id_nodes(i)%pn%id_node(8)
         idnode = cell(i)%p%vertex(3)%ident
         do j = 1, nbnode
            if (idnode == coord_nodes(j)%p%ident) then
                cell(i)%p%vertex(3)%x = coord_nodes(j)%p%x
                cell(i)%p%vertex(3)%y = coord_nodes(j)%p%y
            end if
         end do

         cell(i)%p%vertex(4)%ident = id_nodes(i)%pn%id_node(9)
         idnode = cell(i)%p%vertex(4)%ident
         do j = 1, nbnode
            if (idnode == coord_nodes(j)%p%ident) then
                cell(i)%p%vertex(4)%x = coord_nodes(j)%p%x
                cell(i)%p%vertex(4)%y = coord_nodes(j)%p%y
            end if
         end do

       end do

       ! Assign faces for each cell

       do i = 1, nbelm
         cell(i)%p%faces(1)%p1%x = cell(i)%p%vertex(1)%x
         cell(i)%p%faces(1)%p1%y = cell(i)%p%vertex(1)%y

         cell(i)%p%faces(1)%p2%x = cell(i)%p%vertex(2)%x
         cell(i)%p%faces(1)%p2%y = cell(i)%p%vertex(2)%y

         cell(i)%p%faces(2)%p1%x = cell(i)%p%vertex(2)%x
         cell(i)%p%faces(2)%p1%y = cell(i)%p%vertex(2)%y

         cell(i)%p%faces(2)%p2%x = cell(i)%p%vertex(3)%x
         cell(i)%p%faces(2)%p2%y = cell(i)%p%vertex(3)%y

         cell(i)%p%faces(3)%p1%x = cell(i)%p%vertex(3)%x
         cell(i)%p%faces(3)%p1%y = cell(i)%p%vertex(3)%y

         cell(i)%p%faces(3)%p2%x = cell(i)%p%vertex(4)%x
         cell(i)%p%faces(3)%p2%y = cell(i)%p%vertex(4)%y

         cell(i)%p%faces(4)%p1%x = cell(i)%p%vertex(4)%x
         cell(i)%p%faces(4)%p1%y = cell(i)%p%vertex(4)%y

         cell(i)%p%faces(4)%p2%x = cell(i)%p%vertex(1)%x
         cell(i)%p%faces(4)%p2%y = cell(i)%p%vertex(1)%y

       end do

       end subroutine
end module

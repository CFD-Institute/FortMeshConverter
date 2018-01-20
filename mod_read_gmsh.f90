module mod_read_gmsh

    integer(4) :: nbnode
    integer(4) :: nbel_msh
    integer(4) :: nbelm

    type point
        integer(4) :: ident
        real(8)    :: x = 0.0d0
        real(8)    :: y = 0.0d0
        real(8)    :: z = 0.0d0
    end type

    type list_point
        type(point),pointer :: p => null()
    end type

    type(list_point),dimension(:),allocatable :: coord_nodes

    type node_ident
        integer(4),dimension(:),allocatable:: id_node
    end type
    
    type,extends(node_ident) :: node_ident_msh
        integer(4) :: ident
        integer(4) :: elem_typ
    end type 

    type ptr_node_ident
        type(node_ident),pointer :: pn => null()
    end type
    
    type ptr_node_ident_msh
        type(node_ident_msh),pointer :: pn => null()
    end type

    type(ptr_node_ident),dimension(:),allocatable :: id_nodes
    type(ptr_node_ident_msh),dimension(:),allocatable :: id_nodes_msh

    character(300), save :: fname

    contains
       subroutine read_mesh
       implicit none
       namelist /mesh_file/ fname
       character(300) :: finput
       character(30) :: cline
       integer(4) :: i
       integer(4) :: elem_typ

       open(unit = 100, file = 'input.dat')
       read(100, nml = mesh_file)
       close(100)
       
       finput = trim(fname)//'.msh'
       
       write(6,'(a)'), 'Input mesh file: ',finput

       open(unit = 20, file = finput, status = 'old')
       read(20,*)
       read(20,*)
       read(20,*)
       read(20,*)
       read(20,*) nbnode

       allocate(coord_nodes(1:nbnode))
       do i = 1, nbnode
         allocate(coord_nodes(i)%p)
       end do

       do i = 1, nbnode
        read(20,*) coord_nodes(i)%p%ident,coord_nodes(i)%p%x,coord_nodes(i)%p%y,coord_nodes(i)%p%z
       end do



       read(20,*)
       read(20,*)

       read(20,*) nbel_msh

       allocate(id_nodes_msh(1:nbel_msh))

       do i = 1, nbel_msh
        allocate(id_nodes_msh(i)%pn)
       end do
       
       nbelm = 0

       do i = 1, nbel_msh
        read(20,*) id_nodes_msh(i)%pn%ident,id_nodes_msh(i)%pn%elem_typ 
        elem_typ = id_nodes_msh(i)%pn%elem_typ
        select case (elem_typ) 
            case (1) ! 2-node line.
                allocate(id_nodes_msh(i)%pn%id_node(1:7))
            case (3) ! 4-node quadrangle.
                allocate(id_nodes_msh(i)%pn%id_node(1:9))
                nbelm = nbelm + 1
            case (15) ! 1-node point.
                allocate(id_nodes_msh(i)%pn%id_node(1:6))
            case (37) ! 5-node edge quadrangle.
                allocate(id_nodes_msh(i)%pn%id_node(1:30))
                nbelm = nbelm + 1
            case (27) ! boundary 5-node edge.
                allocate(id_nodes_msh(i)%pn%id_node(1:10))
            case default
                print*,'comming soon !'
                stop
        end select
       end do

       close(unit = 20)
       
       open(unit = 20, file = finput, status = 'old')
       do
           read(20,*) cline
           cline = trim(cline)
           if (cline == '$Elements') exit
       enddo
       
       read(20,*)
       do i = 1, nbel_msh
           read(20,*) id_nodes_msh(i)%pn%id_node(:)
       enddo

       close(unit = 20)
       
       end subroutine
       
       subroutine construct_id_nodes
       implicit none 
       integer(4) :: i,j
       
       allocate(id_nodes(1:nbelm))
       do i = 1, nbelm
           allocate(id_nodes(i)%pn)
           
           if (id_nodes_msh(i)%pn%elem_typ == 3) then
                allocate(id_nodes(i)%pn%id_node(1:9))
           endif
           
           if (id_nodes_msh(i)%pn%elem_typ == 37) then
                allocate(id_nodes(i)%pn%id_node(1:30))
           endif
           
       enddo 
       
       j = 0
       do i = 1, nbel_msh
           if (id_nodes_msh(i)%pn%elem_typ == 3) then
               j = j + 1
               id_nodes(j)%pn%id_node(:) = id_nodes_msh(i)%pn%id_node(:)
           endif 
           
           if (id_nodes_msh(i)%pn%elem_typ == 37) then
               j = j + 1
               id_nodes(j)%pn%id_node(:) = id_nodes_msh(i)%pn%id_node(:)
           endif
       enddo 
       
       end subroutine

end module

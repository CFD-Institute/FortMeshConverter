module mod_read_gmsh

    integer(4) :: nbnode
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
        integer(4),dimension(9) :: id_node
    end type

    type ptr_node_ident
        type(node_ident),pointer :: pn => null()
    end type

    type(ptr_node_ident),dimension(:),allocatable :: id_nodes

    character(300), save :: fname

    contains
       subroutine read_mesh
       implicit none
       namelist /mesh_file/ fname
       character(300) :: finput
       integer(4) :: i

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

       read(20,*) nbelm

       allocate(id_nodes(1:nbelm))

       do i = 1, nbelm
        allocate(id_nodes(i)%pn)
       end do

       do i = 1, nbelm
        read(20,*) id_nodes(i)%pn%id_node(:)
       end do

       close(unit = 20)

       end subroutine

end module

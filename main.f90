
program Gmsh_to_VTK
    use mod_read_gmsh
    use mod_write_vtk
    use mod_cell_2D
    use mod_detect_nearest_neighbor
    implicit none


    call read_mesh

    call construct_cells

    call detect_neighbor

    call write_mesh_vtk



end program Gmsh_to_VTK


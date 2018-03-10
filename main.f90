
program Gmsh_to_VTK
    use mod_read_gmsh
    use mod_write_vtk
    use mod_cell_2D
    use mod_detect_nearest_neighbor
    implicit none

    real :: t1, t2
    
    call CPU_TIME(t1)
    
    call read_mesh
    
    call construct_id_nodes 

    call construct_cells

    call detect_neighbor

    call write_mesh_vtk
    
    call CPU_TIME(t2)
    
    write(*,*) t2-t1, "seconds."

    call write_mesh_tecplot 

end program Gmsh_to_VTK


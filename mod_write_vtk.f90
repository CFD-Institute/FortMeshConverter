module mod_write_vtk
    use mod_read_gmsh, only: nbelm, nbnode, coord_nodes, id_nodes, fname
    use mod_cell_2D
    use mod_detect_nearest_neighbor
    implicit none

    contains
      subroutine write_mesh_vtk
      implicit none

      integer(4) :: i
      character(300) :: foutput
      
      foutput = trim(fname)//'.vtk'

      open(unit = 21, file = foutput, status = 'replace')

      write(21,'(a)') '# vtk DataFile Version 2.0'
      write(21,'(a)') 'VTK format for unstructured mesh'
      write(21,'(a)') 'ASCII'
      write(21,'(a)') 'DATASET POLYDATA'
      write(21,1) nbnode

      do i = 1, nbnode
        write(21,*) coord_nodes(i)%p%x, coord_nodes(i)%p%y, coord_nodes(i)%p%z
      end do

      write(21,2) nbelm, 5*nbelm

      do i = 1, nbelm
        write(21,*) 4,id_nodes(i)%pn%id_node(6:9)-1
      end do

      write(21,3) nbelm
      write(21,'(a)') 'SCALARS CELL_IDENT integer 1'
      write(21,'(a)') 'LOOKUP_TABLE default '

      do i = 1, nbelm
        write(21,*) id_nodes(i)%pn%id_node(1)
      end do

      write(21,'(a)') 'SCALARS NEIGHBOR1 integer 1'
      write(21,'(a)') 'LOOKUP_TABLE default '

      do i = 1, nbelm
          if (associated(cell(i)%p%neighbor1)) then
              write(21,*) cell(i)%p%neighbor1%ident
          else
              write(21,*) 0 
          endif 
      end do

      write(21,'(a)') 'SCALARS NEIGHBOR2 integer 1'
      write(21,'(a)') 'LOOKUP_TABLE default '

      do i = 1, nbelm
          if (associated(cell(i)%p%neighbor2)) then
              write(21,*) cell(i)%p%neighbor2%ident
          else
              write(21,*) 0 
          endif 
      end do

      write(21,'(a)') 'SCALARS NEIGHBOR3 integer 1'
      write(21,'(a)') 'LOOKUP_TABLE default '

      do i = 1, nbelm
          if (associated(cell(i)%p%neighbor3)) then
              write(21,*) cell(i)%p%neighbor3%ident
          else
              write(21,*) 0 
          endif 
      end do

      write(21,'(a)') 'SCALARS NEIGHBOR4 integer 1'
      write(21,'(a)') 'LOOKUP_TABLE default '

      do i = 1, nbelm
          if (associated(cell(i)%p%neighbor4)) then
              write(21,*) cell(i)%p%neighbor4%ident
          else
              write(21,*) 0 
          endif 
      end do

      close(unit = 21)

1     format('POINTS',i9,' float')
2     format('POLYGONS ',2i9)
3     format('CELL_DATA',i9)

      end subroutine
      
      subroutine write_mesh_tecplot
      implicit none

      integer(4) :: i
      character(300) :: foutput
      
      foutput = trim(fname)//'.dat'

      open(unit = 21, file = foutput, status = 'replace')

      !write(21,'(a)') 'VARIABLES=X,Y,CELL_IDENT,NEIGHBOR1,NEIGHBOR2,NEIGHBOR3,NEIGHBOR4' ! coming soon
      write(21,'(a)') 'VARIABLES=X,Y'
      write(21,'(a)') 'ZONE T="UNSTRUCTURED-COUNTOUR"'
      write(21,'(a)') 'ZONETYPE=FEPOLYGON'
      write(21,*) 'NODES=',nbnode
      write(21,*) 'ELEMENTS=',nbelm
      write(21,*) 'FACES=',nbelm*4
      write(21,*) 'NumConnectedBoundaryFaces=0'
      write(21,*) 'TotalNumBoundaryConnections=0'

      do i = 1, nbnode
        write(21,*) coord_nodes(i)%p%x
      end do

      do i = 1, nbnode
        write(21,*) coord_nodes(i)%p%y
      end do 
      
      ! Node indexes
      do i = 1, nbelm
        write(21,*) id_nodes(i)%pn%id_node(6), id_nodes(i)%pn%id_node(7)
        write(21,*) id_nodes(i)%pn%id_node(7), id_nodes(i)%pn%id_node(8)
	    write(21,*) id_nodes(i)%pn%id_node(8), id_nodes(i)%pn%id_node(9)
	    write(21,*) id_nodes(i)%pn%id_node(9), id_nodes(i)%pn%id_node(6)
      enddo
      
      do i = 1, nbelm
        write(21,*) i,i,i,i
      enddo  
  
      do i = 1, nbelm
        write(21,*) 0,0,0,0
      enddo

      
      close(unit = 21)

      end subroutine


end module

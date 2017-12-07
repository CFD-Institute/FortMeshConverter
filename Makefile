CF         = gfortran
FFLAGS     = -O3 -fbounds-check -g
LD         = gfortran
LDFLAGS    = 
PREPROC    = 

OBJS =  mod_read_gmsh.o  \
        mod_cell_2D.o \
	mod_detect_nearest_neighbor.o  \
	mod_write_vtk.o  \
	main.o \


.SUFFIXES: .o .f90 .f
.f90.o:
	$(LD) -c $(FFLAGS) $<
.f.o:
	$(LD) -c $(FFLAGS) $<

gmshtovtk :$(OBJS) 
	$(LD) $(LDFLAGS) -o $@ $(OBJS)

clean :
	rm -f gmshtovtk *.o core *.mod


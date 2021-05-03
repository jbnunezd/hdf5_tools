hdf5_tools/hdf5_tools.o: hdf5_tools/hdf5_tools.f90 

main.o: main.f90 \
	hdf5_tools/hdf5_tools.o 

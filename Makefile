all : slicedata01.dat
	./plot.sh
slicedata01.dat : try surfacenew.dat
	./try
	rm temp.dat
try : insert.f90 calarea.f90 pointsort.f90 slice.f90 Frame.f90
	gfortran -fbounds-check insert.f90 calarea.f90 pointsort.f90 slice.f90 Frame.f90 -w -g -o try

clean:
	rm insertpts.mod makeslice.mod pointsort.mod try cal_statics.mod slicedata*.dat *.svg

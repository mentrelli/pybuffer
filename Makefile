#
# makefile - by Undy, 17/10/2015
#

o      = 3
openmp = no

FLAG_O = -O$(o)

ifeq ($(openmp),yes)
FLAG_OPENMP = -fopenmp
else
FLAG_OPENMP = 
endif

SHELL = /bin/sh

# source directory
DIR_SRC = src/

# build directories
DIR_BUILD = build/
DIR_OBJ = $(DIR_BUILD)obj/
DIR_MOD = $(DIR_BUILD)mod/

# executable name and directory
NAME_EXE = f_mc_buffer.exe
DIR_EXE  = ./

# source files 
SOURCES = lib_qsort.f90 lib_pdistr.f90 montecarlo_buffer.f90

OBJECTS = $(patsubst %.f90, $(DIR_OBJ)%.o, $(SOURCES))
MODULES = $(patsubst %.f90, $(DIR_MOD)%.mod, $(filter-out %montecarlo_buffer.f90, $(SOURCES)))

EXE = $(DIR_EXE)$(NAME_EXE)

vpath %.f90 $(DIR_SRC) 

# compiler and linker settings 
FC     = gfortran
FFLAGS = -J$(DIR_MOD) $(FLAG_O) -std=f2008 -pedantic -cpp $(FLAG_OPENMP)
#FFLAGS = -J$(DIR_MOD) $(FLAG_O) -cpp $(FLAG_OPENMP)
LFLAGS = $(FFLAGS) $(INCL) $(LIBS)

main : $(EXE)

# rule for executable #$(wildcard $(LSMLIB_PATH_OBJ)lsm*.o) 
## $(DIR_OBJ)"*.o" $(OBJECTS) $(wildcard $(LSMLIB_PATH_OBJ)lsm*.o)
$(EXE) : what-exe mkdir-obj mkdir-mod mkdir-exe $(OBJECTS) 
	@echo "...linking   :" $(LFLAGS) 
	@$(FC) $(OBJECTS) $(LFLAGS) -o $@
	@echo ...executable: $(EXE)

# display basic info at the beginning
what-exe:
	@echo "...target    : "$(EXE)
	@echo "...gcc flags : "$(FFLAGS)
	
all : main

# create directories, if needed
mkdir-obj :
	@if [ ! -d "$(DIR_OBJ)" ] ; then mkdir -p $(DIR_OBJ) && echo "...creating  : directory '"$(DIR_OBJ)"'" ; fi

mkdir-mod :
	@if [ ! -d "$(DIR_MOD)" ] ; then mkdir -p $(DIR_MOD) && echo "...creating  : directory '"$(DIR_MOD)"'" ; fi
	
mkdir-exe :
	@if [ ! -d "$(DIR_EXE)" ] ; then mkdir -p $(DIR_EXE) && echo "...creating  : directory '"$(DIR_EXE)"'" ; fi
	
# manage dependencies
$(DIR_OBJ)montecarlo_buffer.o : montecarlo_buffer.f90 $(DIR_OBJ)lib_qsort.o $(DIR_OBJ)lib_pdistr.o
	@echo ...compiling : $<
	@$(FC) $(FFLAGS) -c $< -o $@

$(DIR_OBJ)lib_qsort.o : lib_qsort.f90
	@echo ...compiling : $<
	@$(FC) $(FFLAGS) -c $< -o $@
	
$(DIR_OBJ)lib_pdistr.o : lib_pdistr.f90
	@echo ...compiling : $<
	@$(FC) $(FFLAGS) -c $< -o $@	

			
# utilities
.PHONY : main mkdir-obj mkdir-mod mkdir-exe what-exe run clean spotless

run : $(EXE) 
	@echo "...running   :" $(EXE) ...
	$(EXE)
	
clean :
	@rm -f $(DIR_OBJ)*.o $(DIR_MOD)*.mod $(EXE)
	@echo "...removing: "$(DIR_OBJ)*.o" "$(DIR_MOD)*.mod $(EXE)

spotless:
	@rm -f $(DIR_OBJ)*.o $(DIR_MOD)*.mod *.pyc $(EXE)
	@if [ -d $(DIR_OBJ) ] ; then rmdir $(DIR_OBJ) ; fi
	@if [ -d $(DIR_MOD) ] ; then rmdir $(DIR_MOD) ; fi
	@if [ -d $(DIR_BUILD) ] ; then rmdir $(DIR_BUILD) ; fi
	@echo "...removing: "$(DIR_BUILD) $(EXE) 

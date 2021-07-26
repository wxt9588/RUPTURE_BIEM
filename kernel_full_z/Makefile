#######################################################################
#     list source files and the target names                          #
#######################################################################
SRC_OWN= discrete_kernels.f90 main.f90
EXE_OWN := FZ.out

MODDIR := ./modules
OBJDIR := ./OBJ
MOD_COM := $(MODDIR)/nrtype.o $(MODDIR)/mod_string.o 
OBJ_OWN :=  $(foreach file,$(SRC_OWN),$(OBJDIR)/$(file:.f90=.o))

FC := /public/software/intel/bin/ifort
FFLAGS := -O3
#######################################################################
#                        the target                                   #
#######################################################################
all : $(EXE_OWN)
$(EXE_OWN) : $(MOD_COM) $(OBJ_OWN) 
	$(FC) -o $(EXE_OWN) $(OBJ_OWN) $(MOD_COM)

$(OBJDIR)/%.o : %.f90
	$(FC) $(FFLAGS) -o $(OBJDIR)/$(@F) -c $(<F) -module $(OBJDIR) -I$(MODDIR)

$(MODDIR)/%.o : $(MODDIR)/%.f90
	$(FC) $(FFLAGS) -o $(MODDIR)/$(@F) -c $(MODDIR)/$(<F) -module $(MODDIR)


RM := rm
clean:
	$(RM) -f $(OBJDIR)/* 

cleanall: clean
	$(RM) -f $(MODDIR)/*.o $(MODDIR)/*.mod 
	$(RM) -f *.out

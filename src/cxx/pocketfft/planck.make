PKG:=pocketfft

SD:=$(SRCROOT)/$(PKG)
OD:=$(BLDROOT)/$(PKG)

FULL_INCLUDE+= -I$(SRCROOT)

HDR_$(PKG):=$(SD)/*.h
LIB_$(PKG):=$(LIBDIR)/libpocketfft.a
OBJ:=pocketfft.o
OBJ:=$(OBJ:%=$(OD)/%)

ODEP:=$(HDR_$(PKG))

$(OBJ): $(ODEP) | $(OD)_mkdir
$(LIB_$(PKG)): $(OBJ)

all_hdr+=$(HDR_$(PKG))
all_lib+=$(LIB_$(PKG))

SUBDIRS=CCode CodeGen

all: $(SUBDIRS)

$(SUBDIRS): 
	make -C $@

# This seems a bit hacky... Is there a more makey way of doing it?
clean: 
	-for d in $(SUBDIRS); do make -C $$d clean; done

.PHONY: all clean $(SUBDIRS)

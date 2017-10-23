ROOT_DIR:=..
AUTO_DIRS:=
CLEANED:=
MADE_CLEAN:=static dynamic partial
include $(ROOT_DIR)/shared.mk 

BINS:= $(foreach CONFIG, $(GRIFT_CONFIGS), $(addprefix $(CONFIG)/, $(BINS)))

OUTS:= $(foreach CONFIG, $(GRIFT_CONFIGS), $(addprefix $(CONFIG)/, $(OUTS)))

.PHONEY: all
all : $(BINS) $(OUTS)

static/%:
	$(MAKE) -C static $*

dynamic/%:
	$(MAKE) -C dynamic $*



DISK=vic20emu.dsk
ROMS=chargen basic2 kernal

.PHONY: clean

$(DISK): vic20emu.asm $(ROMS)
	pyz80.py --exportfile=vic20emu.sym vic20emu.asm

clean:
	rm -f $(DISK) vic20emu.sym

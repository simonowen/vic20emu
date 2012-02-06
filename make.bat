@echo off

if "%1"=="clean" goto clean

pyz80.py --exportfile=vic20emu.sym vic20emu.asm
goto end

:clean
if exist vic20emu.dsk del vic20emu.dsk vic20emu.sym

:end

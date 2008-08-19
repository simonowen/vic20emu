#!/usr/bin/perl -w
#
# Generates look-up table for VIC bass note to SAA 1099 octave+note
#
# Used by the VIC-20 emulator, available from:
#
#     http://simonowen.com/sam/vic20emu/

# Loop over 8 octaves
for ($o = 0 ; $o < 8 ; $o++)
{
    # Loop over 256 note numbers
    for ($n = 0 ; $n < 256 ; $n++)
    {
        # Determine and store the frequency for the octave/note combination
        my $f = (15625 << $o) / (511-$n);
        $f{$f} = [$o,$n];
    }
}

# Build a list of frequency values (unsorted)
@f = keys %f;

open FILE, ">sound.dat" and binmode FILE or die "$!\n";

$notes = $octaves = '';

# Loop over sound values from 0 to 127, for voice 0
for ($n = 0 ; $n <= 127 ; $n++)
{
    my $f = 4329 / (127 - (($n == 127) ? -1 : $n));

    # Find the closest frequency in the list built earlier
    my $f2 = (sort { abs($f-$a) <=> abs($f-$b) } @f)[0];

    $octaves .= pack "C", $f{$f2}[0];
    $notes .= pack "C", $f{$f2}[1]
}

# Write the corresponding octave and note values to the output file
print FILE $notes . $octaves;

close FILE;

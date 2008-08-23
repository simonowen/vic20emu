#!/usr/bin/perl -w
#
# Generates look-up table for VIC bass note to SAA 1099 and AY-3-8912
#
# Used by the VIC-20 emulator, available from:
#
#     http://simonowen.com/sam/vic20emu/

$bass = 4329;       # VIC clock for bass voice
$alto = $bass*2;    # VIC clock for alto voice
$soprano = $alto*2; # VIC clock for soprano voice
$ayfreq = 1773400;  # AY-3-8912 frequency for Spectrum
$aydivider = 16;    # AY clock divider for tone values

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

open FILE, ">saa1099.dat" and binmode FILE or die "$!\n";

$notes = $octaves = '';

# Loop over sound values from 0 to 127, for the bass voice
for ($n = 0 ; $n <= 127 ; $n++)
{
    my $f = $bass / (127 - (($n == 127) ? -1 : $n));

    # Find the closest frequency in the list built earlier
    my $f2 = (sort { abs($f-$a) <=> abs($f-$b) } @f)[0];

    $octaves .= pack "C", $f{$f2}[0];
    $notes .= pack "C", $f{$f2}[1]
}

# Write the corresponding octave and note values to the output file
print FILE $notes . $octaves;
close FILE;


open FILE, ">ay38912.dat" and binmode FILE or die "$!\n";

$fine = $coarse = '';

# Loop over sound values from 0 to 127, for the soprano voice
for ($n = 0 ; $n <= 127 ; $n++)
{
    # Determine the AY note value
    my $x = int((127 - (($n == 127) ? -1 : $n)) * ($ayfreq / $aydivider / $soprano) + 0.5);

    $fine .= pack "C", $x%256;
    $coarse .= pack "C", $x/256;
}

print FILE $coarse . $fine;
close FILE;

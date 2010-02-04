#!/usr/bin/perl -w
#
# Calculates opcode addresses for the inline instruction decoding table
#
# Used by the VIC-20 emulator, available from:
#
#     http://simonowen.com/sam/vic20emu/

$source = 'vic20emu.asm';
$codeend = 0xc000;

# Assemble, outputting the symbols containing opcode implementation lengths
$_ = `pyz80.py -s op_.*_len $source`;

# Create include file for definitions
my $outfile = 'opdefs.inc';
open(FILE, ">$outfile") or die "$!";
print FILE "; Opcode table positions (auto-generated by opdefs.pl)\n\n";

# Assembly failed?
if ($?)
{
    # Create dummy offset list to allow lengths to be calculated
    for (0..255) {
        printf FILE "op_%02x: equ &%04x\n", $_, $codeend-0x1000;
    }

    print "Assembly error, creating dummy definitions!\n";
    exit;
}

# Create hash from opcode to length
while (/OP_(..)_LEN': (\d+)/g) {
    $len{hex $1} = $2;
}

# Position in order of size, largest first
@todo = reverse sort { $len{$a} <=> $len{$b} } keys %len;

my($size,$used) = (0,0);

OPCODE:
foreach $op (@todo)
{
MSB:
    # Work up through MSB values until we find a space
    for ($msb = 0; ; $msb++)
    {
        # Determine the extent of the opcode in the current MSB
        my $start = ($msb << 8) | $op;
        my $end = $start + $len{$op}-1;

        # Check against what we've already positioned
        foreach (keys %off)
        {
            # Determine extent of existing item
            my $start2 = $off{$_};
            my $end2 = $start2 + $len{$_}-1;

            # Reject MSB if new position would overlap
            next MSB unless ($start > $end2 || $end < $start2);
        }

        # Assign to the free spot we've found
        $off{$op} = $start;

        # Update size stats
        $used += $len{$op};
        if ($end > $size) { $size = $end; }

        next OPCODE;
    }
}

# Position base so code finishes at the required point
$base = $codeend - (($size + 0xff) & ~0xff);

print "Size = $size, used = $used, slack = ", $size-$used, "\n";

# Output sorted list of calculated positions
foreach (sort { $a <=> $b } @todo)
{
    my $offset = $base + $off{$_};
    printf FILE "op_%02x:         equ  &%04x ; +$len{$_}\n", $_, $base+$off{$_};
}

close FILE;

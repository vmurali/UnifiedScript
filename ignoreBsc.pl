#!/usr/bin/perl

# -------------------------
# Captures the output of compiling with BSC so that benign warnings can be
# ignored.  It writes out a log the full output and the filtered output,
# which is also printed to stdout.
# -------------------------

# Directory where the log files should go
$outdir = "./";

# The name of the full log file
$fulllog = "compile_log_full.out";

# The name of the filtered log file
$filtlog = "compile_log_filtered.out";

# Message numbers to ignore
@ignorelist = ( "G0006", "G0015", "G0066");


# -------------------------
# Functions

sub is_elem
{
    local($val) = shift;
    local(@arr) = @_;

    foreach $elem (@arr) {
	if ($val eq $elem) {
	    return 1;
	}
    }

    return 0;
}


# -------------------------
# main

open(FULL,">$outdir$fulllog") || die ("Cannot open $outdir$fulllog: $!");

open(FILT,">$outdir$filtlog") || die ("Cannot open $outdir$filtlog: $!");

$in_msg = 0;
$ignore_msg = 0;

while ($line = <STDIN>) {
    print FULL $line;
    if ( $line =~ /^(Error|Warning|Message)\: (.*)\: \((\w\d\d\d\d)\)$/ ) {
        $in_msg  = 1;
        $msgtype = $1;
        $msgpos  = $2;
        $msgnum  = $3;

        if (&is_elem($msgnum, @ignorelist)) {
            $ignore_msg = 1;
        } else {
            $ignore_msg = 0;
        }
    } elsif ($in_msg) {
        # if the line is not indented, we're no longer in a message
        if ($line =~ /^\S/) {
            $in_msg = 0;
        }
    }

    if ((! $in_msg) || (! $ignore_msg)) {
        print FILT $line;
        print $line;
    }
}

close FILT;
close FULL;


# -------------------------


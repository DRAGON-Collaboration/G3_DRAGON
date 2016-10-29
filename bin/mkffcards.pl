#!/usr/bin/perl -w
# $Id: mkffcards.pl,v 1.4 2004-03-17 19:20:06 jslater Exp $
#
# Generator of ffcard files for GEANT "scans".
#
# A.Gaponenko, September 2002
use English;
use Getopt::Long;
use File::Basename;
use Fcntl;


sub usage {
    my $self = basename($0);
    print "
Usage: $self ffcard-file firstrun nruns [--debug, -r]
Or:    $self ffcard-file firstrun nruns 'CARD\[INDEX\]' PARMIN PARMAX ...

Produces files dragon<N>.ffcards from the given template with
appropriate run numbers and random seeds.

If CARD arguments are given, also varies linearly the given parameters
between their limits from the first to the last run.

EXAMPLES:

To prepare ffcards for 100 \"identical\" GEANT runs 6000 to 6099:

	$self e614.ffcards 6000 100

To prepare ffcars files for 11 runs 6100 to 6110 changing gas degrader
setting in 10% steps from 0 to 100%:

	$self e614.ffcards 6100 11 'GABS[2]' 0. 1.

Use the -q option to setup a subdirectory and submit a PBS job for each
ffcards file. 

IMPORTANT: the script uses the same format for a variable parameter as
the original ffcard file has, including the number of digits after the
decimal point. For the example above to work correctly the original
e614.ffcards should give GABS[2] with at least one digit after the
point (e.g. 0.0, not just 0.).  The number from the original file is
discarded, but the format is copied.
"
;
    exit 1;
}

################################################################
sub prepare_run($$\%) {
    my $ffname = shift;
    my $run = shift;
    my $substitutions = shift;
    if($debug){print "Filename $ffname , Run $run , Subs $substitutions \n;"}
    #--------------------------------------------------------
    # Prepare FFCARDS from the template
    local $FFIN;
    open(FFIN, "<${ffname}") 
	or die "Error opening ${ffname}: $!\n";

    my $outname = sprintf "dragon%05d.ffcards", $run;

    local $FFOUT;
    sysopen(FFOUT, $outname, O_WRONLY | O_EXCL | O_CREAT)
	or die "Error opening $outname: $!\n";

    my $line; 
    
    my %stats; # 

    my $current_key = "";
    my $offset = 0;

    while($line = <FFIN>) {
	# print $line;

	if($line =~ /^\s*(\w+)\s/) {
	    my $linekey = $1;
	    if($linekey eq "C") {
		# print ">> unsetting current_key\n";
		$current_key = "";
		$offset = 0;
	    }
	    elsif(defined($$substitutions{$linekey})) {
		# print ">> setting current_key = $linekey\n";
		$current_key = $linekey;
		$offset = 0;
	    }
	}

	if($current_key) {
	    # print "Analyzing for key $current_key\n";
	    my %keysub = %{$$substitutions{$current_key}};

	    my @fields = split(' ', $line);

	    my $ifirst = 0;
	    if($fields[0] =~ /^$current_key$/) {
		# print "First line with the key $current_key\n";
		$ifirst = 1;
		$offset = 0;
	    }

	    my $minindex = $offset + 1;
	    my $maxindex = $minindex + $#fields - $ifirst;

	    #print "On the current line: $current_key\[$minindex\]"
	    # ." to $current_key\[$maxindex\]\n";
	    
	    my $line_changed = 0;
	    foreach my $ind (keys %keysub) {
		# print "Checking index $ind\n";
		 if(($ind >= $minindex) && ($ind <= $maxindex)) {
		     $line_changed = 1;

		     # Preserve format: $keysub{$ind} might be an integer,
		     # so by default it would be printed out as an integer.
		     # However if the corresponding ffcard is a real number
		     # FORTRAN will screw it up. 
		     # Look at the old value of ffcard to determine the 
		     # necessary format.
		     my $old_val = $fields[$ind - $minindex + $ifirst];
		     my $new_val = $keysub{$ind};
		     if($old_val =~ /^(\d*)\.(\d*)$/) {
			 my $precision = length($2);
			 #print "REAL $current_key\[$ind\]. Precision $precision\n";
			 #print "Format: %.${precision}f\n";
			 #print "$new_val -> ";
			 $new_val = sprintf("%.${precision}f", $new_val);
			 #print "$new_val\n";
		     }
		     #else{
		     # print "DEFAULT FORMAT for $current_key\[$ind\]: $new_val\n";
		     #}

		     $fields[$ind - $minindex + $ifirst] = $new_val;

		     ${stats{$current_key}}{$ind} = 1;
		 }
	    }
	    $line = (join(' ', @fields) . "\n" ) if($line_changed);

	    $offset = $maxindex;
	}

	print FFOUT $line;
    }
# JS creates a directory structure
# Each ffcard file placed in directory with run number
# dssub.csh is run on each directory, submitting dsbatch to PBS
    if ($q) {
	system `mkdir d$run`;
	system `mv $outname d$run/dragon.ffcards`;
	$ENV{"OUTPATH"} = $ENV{"DSBIN"} . "/d" . $run;
	system `qsub dssub.csh`;
    }

    close FFOUT;
    close FFIN;


    # Check that all requested key/index pairs have been used
    
    foreach my $k (keys %$substitutions) {
	foreach my $ind (keys %{$$substitutions{$k}}) {
	    #print "key $k, ind $ind, stat = ". ${stats{$k}}{$ind} . "\n";
	    warn "Warning: no substitution for"
		." $k\[$ind\] has been performed.\n"
		    unless defined(${stats{$k}}{$ind});
	}
    }
}

################################################################
print "$#ARGV \n";
$#ARGV >= 2 or die usage();
my $ffname = shift;
my $firstrun = shift;
my $nruns = shift;
# Set default option values

$opt{'debug'}   = 0;
$opt{'q'} = 0;
# Then process command line opts.
die "\nError processing command line options.\n"
    if(!GetOptions(\%opt, "debug", "q")
       );

$debug=$opt{'debug'};
if($debug) {
    foreach $key(keys %opt) {
        print "opt: $key => $opt{$key}\n";
    }
}
$q=$opt{'q'};

if($debug){print "$debug\n";}
$firstrun =~ /^\d+$/ 
    or die "Non numeric firstrun\n";

$nruns =~ /^\d+$/ 
    or die "Non numeric nruns\n";
my @cardstruct;
my $arg;
while($key = shift) {
    my $start = shift;
    my $max = shift;
if($debug) {
    print "$key $start $max\n";}
    my $inc = ($nruns > 1)? ($max - $start)/($nruns - 1) : 0;

   die "Error: Not enough arguments for key $key\n"
	if(! defined($inc));

   die "Error: arg $key does not have the card[index] format\n"
	if($key !~ /^(\w+)\[(\d+)\]/);

    my $key = $1;
    my $index = $2;

if($debug) {    print "Got: key $key, index $index, start $start, inc $inc\n";}

    push @cardstruct, {'key' => $key, 
		       'index' => $index, 
		       'start' => $start,
		       'inc' => $inc
		   };
}

my $lastrun = $firstrun + $nruns - 1;
for(my $run = $firstrun; $run <= $lastrun; $run++) {
    print "Preparing run $run\n";

    my %substitutions;
    foreach my $i (@cardstruct) {
	my $value = $$i{'start'} + ($run - $firstrun)*$$i{'inc'};
	${$substitutions{$$i{'key'}}}{$$i{'index'}} = $value;
    }

    ${$substitutions{'RUNG'}}{1} = $run;
    ${$substitutions{'RNDM'}}{2} = $run;

    prepare_run($ffname, $run, %substitutions);
}

exit 0;
################################################################

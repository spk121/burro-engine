open(my $in,  "<",  "tones.txt")  or die "Can't open tones.txt: $!";           
open(my $out, ">",  "adsr.txt") or die "Can't open adsr: $!";

# This procedure reads the tones files and generates an adsr file
sub compute {
    while (<$in>) {
        # Parse the 6-column space-delimited tones.txt file
        chomp;
        my(@fields) = split(/ /);

        # Start time of the note: a float.  Seconds since the
        # beginning of the song.
        my $start_time = $fields[0];

        # The sound-engine channel designated for this note: 0 to 15
        my $channel = $fields[1];

        # For non-percussion, the next field is an integer that maps to
        # a type of instrument.
        my $patch = $fields[2];

        # For non-percussion, this number maps to the pitch.  For percussion,
        # this maps to a type of non-tuned instrument.
        my $note = $fields[3];
        
        # Duration of the note in seconds.
        my $duration = $fields[4];

        # Velocity aka volume. An integer.
        my $velocity = $fields[5];

        # When true, this is non-tuned percussions
        my $percussion = $fields[6];


# Write a bit of a header
print $out "# Project Burro song file\n\n";



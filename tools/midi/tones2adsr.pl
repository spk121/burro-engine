use warnings;
use strict;

my %tuned = (
    # D_attack - seconds
    # D_decay
    # D_sustain is always autocomputed
    # D_release
    # A_attack - a ratio where 1.0 is 100% volume
    # A_sustain - a ratio where 1.0 is 100% volume
    # F_initial - a ratio where 1.0 is the base frequency
    # F_attack - a rato
    # F_sustain is always 1.0
    # F_release
    # Waveform - 0 = square, 1 = sine
    # Duty - 
    'key'     => ['D_atk', 'D_dcy', 'D_rls', 'A_atk', 'A_stn', 'F_ini', 'F_atk', 'F_rls', 'Wav', 'Duty'],
    'square'  => [0.0,      0.0,    0.0,      1.0,    1.0,     1.0,     1.0,     1.0,     0,      0.5],
    'piano'   => [0.01,     0.6,    0.01,     1.0,    0.5,     1.02,    1.01,    1.0,     0,      0.5],
    'clavi'   => [0.01,     0.3,    0.01,     1.0,    0.3,     1.01,    1.00,    1.00,    1,      0.6],
    'organ'   => [0.01,     0.1,    0.05,     1.0,    0.9,     0.99,    1.00,    0.99,    1,      0.6],
    'string'  => [0.15,     0.1,    0.10,     1.0,    0.9,     1.02,    1.01,    1.00,    0,      0.5],
    'ensemb'  => [0.0,      0.0,    0.0,      1.0,    1.0,     1.0,     1.0,     1.0,     0,      0.5],
    'brass'   => [0.10,     0.1,    0.10,     1.0,    0.9,     0.99,    1.00,    0.99,    0,      0.5],
    'reed'    => [0.10,     0.1,    0.10,     1.0,    0.9,     0.99,    1.00,    0.99,    0,      0.5],
    'pipe'    => [0.10,     0.1,    0.10,     1.0,    0.9,     0.99,    1.00,    0.99,    1,      0.5],
    'lead'    => [0.0,      0.1,    0.0,      1.0,    0.6,     1.0,     1.0,     1.0,     0,      0.5],
    'pad'     => [0.0,      0.0,    0.2,      1.0,    1.0,     1.0,     1.0,     1.0,     0,      0.5],
    'other'   => [0.10,     0.2,    0.30,     1.0,    0.9,     1.2,     1.1,     0.9,     0,      0.8],
);

my %untuned = (
    # D_attack, D_decay, D_sustain, D_release, - seconds
    # A_attack, A_sustain - ratio
    # F_initial, F_attack, F_sustain, F_release - in Hz
    # Waveform - 0 = square, 1 = sine
    # Duty - ratio
    # Noise - 1 = noise, 0 = tone
    'kick'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,   150.0,    87.0,    70.0,    70.0, 1, 0.5, 1],
    'snare'    => [0.010, 0.030, 0.020, 0.030, 1.0, 0.3,   370.0,   360.0,   350.0,    70.0, 0, 0.8, 1],
    'tom1'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,    85.0,    75.0,    65.0,    55.0, 1, 0.5, 1],
    'tom2'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,   102.0,    92.0,    82.0,    72.0, 1, 0.5, 1],
    'tom3'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,   124.0,   114.0,   104.0,    94.0, 1, 0.5, 1],
    'tom4'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,   151.0,   141.0,   131.0,   121.0, 1, 0.5, 1],
    'tom5'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,   185.0,   175.0,   165.0,   155.0, 1, 0.5, 1],
    'tom6'     => [0.010, 0.030, 0.020, 0.030, 1.0, 0.5,   228.0,   218.0,   208.0,   198.0, 1, 0.5, 1],
    'hhclosed' => [0.010, 0.060, 0.060, 0.010, 1.0, 0.5, 10000.0, 10000.0, 10000.0, 10000.0, 0, 0.5, 1],
    'hhpedal'  => [0.010, 0.060, 0.060, 0.010, 1.0, 0.5, 12000.0, 12000.0, 12000.0, 12000.0, 0, 0.5, 1],
    'hhopen'   => [0.010, 0.060, 0.630, 0.010, 1.0, 0.5,  2900.0,  2900.0,  2900.0,  2900.0, 0, 0.5, 1],
    'crash'    => [0.010, 0.060, 0.630, 0.010, 1.0, 0.5,  3700.0,  3700.0,  3700.0,  3700.0, 0, 0.5, 1],
    'ride'     => [0.010, 0.060, 0.630, 0.010, 1.0, 0.2,  3700.0,  3700.0,  3700.0,  3700.0, 0, 0.5, 1],
    'other'    => [0.030, 0.060, 0.060, 0.030, 1.0, 1.0,  2000.0,  2000.0,  2000.0,  2000.0, 0, 0.5, 1],
    );

open(my $in,  "<",  "tones.txt")  or die "Can't open tones.txt: $!";           
open(my $out, ">",  "adsr.txt") or die "Can't open adsr: $!";

sub tone {
    my($start_time, $channel, $instrument, $note, $duration, $velocity) = @_;
    my $d_attack = $tuned{$instrument}[0];
    my $d_decay = $tuned{$instrument}[1];
    my $tmp_decay = $d_decay;
    my $d_sustain = 0;
    my $d_release = $tuned{$instrument}[2];
    my $freq = 440.0 * 2.0**(($note - 69) / 12);
    if ($duration <= $d_attack) {
	$tmp_decay = 0;
    } elsif ($duration <= $d_attack + $d_decay) {
	$tmp_decay = $duration - $d_attack;
    } else {
	$d_sustain = $duration - $d_attack - $d_decay;
    }
    $d_decay = $tmp_decay;
    my $a1 = $velocity * $tuned{$instrument}[3];
    my $a2 = $velocity * $tuned{$instrument}[4];
    my $f1 = $freq * $tuned{$instrument}[5];
    my $f2 = $freq * $tuned{$instrument}[6];
    my $f3 = $freq;
    my $f4 = $freq * $tuned{$instrument}[7];
    my $wav  = $tuned{$instrument}[8];
    my $duty = $tuned{$instrument}[9];
    
    print $out "($channel $start_time $d_attack $d_decay $d_sustain $d_release $f1 $f2 $f3 $f4 $a1 $a2 $wav $duty 0)\n";
}

sub noise {
    my($start_time, $channel, $instrument, $velocity) = @_;

    my $d_attack = $untuned{$instrument}[0];
    my $d_decay = $untuned{$instrument}[1];
    my $d_sustain = $untuned{$instrument}[2];
    my $d_release = $untuned{$instrument}[3];
    my $a1 = $velocity * $untuned{$instrument}[4];
    my $a2 = $velocity * $untuned{$instrument}[5];
    my $f1 = $untuned{$instrument}[6];
    my $f2 = $untuned{$instrument}[7];
    my $f3 = $untuned{$instrument}[8];
    my $f4 = $untuned{$instrument}[9];
    my $wav  = $untuned{$instrument}[10];
    my $duty = $untuned{$instrument}[11];
    print $out "($channel $start_time $d_attack $d_decay $d_sustain $d_release $f1 $f2 $f3 $f4 $a1 $a2 $wav $duty 1)\n";
}


# This procedure reads the tones files and generates an adsr file
sub compute {
    print "in compute";
    while (<$in>) {
        # Parse the 6-column space-delimited tones.txt file
        chomp;
        my(@fields) = split();

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

	if ($percussion == 0) {
	    my $instrument = 'other';
	    if ($patch < 8) {$instrument = 'piano'; }
	    elsif ($patch < 16) {$instrument = 'clavi'; }
	    elsif ($patch < 24) {$instrument = 'organ'; }
	    elsif ($patch < 32) {$instrument = 'string'; }
	    elsif ($patch < 40) {$instrument = 'ensemb'; }
	    elsif ($patch < 48) {$instrument = 'brass'; }
	    elsif ($patch < 56) {$instrument = 'reed'; }
	    elsif ($patch < 64) {$instrument = 'pipe'; }
	    elsif ($patch < 72) {$instrument = 'lead'; }
	    elsif ($patch < 80) { $instrument = 'pad'; }
	    tone($start_time, $channel, $instrument, $note, $duration, $velocity);
	} else {
	    my $instrument = 'other';
	    if ($note >= 35 && $note <= 36) {$instrument = 'kick'; }
	    elsif ($note == 38 || $note == 40) {$instrument = 'snare'; }
	    elsif ($note == 41) { $instrument = 'tom1';}
	    elsif ($note == 43) {$instrument = 'tom2'; }
	    elsif ($note == 45) {$instrument = 'tom3'; }
	    elsif ($note == 47) {$instrument = 'tom4'; }
	    elsif ($note == 48) {$instrument = 'tom5'; }
	    elsif ($note == 50) {$instrument = 'tom6'; }
	    elsif ($note == 42) {$instrument = 'hhclosed'; }
	    elsif ($note == 44) {$instrument = 'hhpedal'; }
	    elsif ($note == 49) {$instrument = 'crash'; }
	    elsif ($note == 51) {$instrument = 'ride'; }
	    noise($start_time, $channel, $instrument, $velocity);
	}
    }
}
# Write a bit of a header
print $out ";; Project Burro song file\n\n";

compute();


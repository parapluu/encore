# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# contributed by Rodrigo de Oliveira 29/mar/2010
# based on the original version by Jesse Millikian

use threads;
use threads::shared;
use Thread::Semaphore;

my @creature_colors = qw/blue red yellow/;
my $compl_dict      = {
    'blue' => {
        'blue'   => 'blue',
        'red'    => 'yellow',
        'yellow' => 'red'
    },
    'red' => {
        'blue'   => 'yellow',
        'red'    => 'red',
        'yellow' => 'blue'
    },
    'yellow' => {
        'blue'   => 'red',
        'red'    => 'blue',
        'yellow' => 'yellow'
    }
};

# reporting
sub check_complement {
    for my $a (@creature_colors) {
        for my $b (@creature_colors) {
            printf( "%s + %s -> %s\n", $a, $b, $compl_dict->{$a}->{$b} );
        }
    }
}

my @numbers = qw/zero one two three four
  five six seven eight nine/;

sub spellout {
    my $n = shift;
    return ' ' . join ' ', map { $numbers[$_] } split( '', $n );
}

sub meet {

    # Count, signal and mutex are all $meetings
    my $meetings : shared = shift;
    my @colors = @_;

    # creature creation
    my $id = 0;
    my @creatures : shared;
    @creatures = map {
        my %h : shared = (
            color    => $_,
            id       => $id++,
            met      => 0,
            self_met => 0
        );
        \%h
    } @colors;

    # block all creatures to fix startup unbalance
    my $ready = Thread::Semaphore->new;
    $ready->down;

    # Colour communication variables
    my $first : shared  = undef;
    my $second : shared = undef;

    # $_ is thread on the outer map and color on the inner loop
    my @thrs =
      map {

        # async starts a new thread running the block given
        async {
            my $creature = $_;
            my ( $id, $color, $other_color ) = ( $creature->{id}, $creature->{color}, undef );
            my $met      = 0;
            my $self_met = 0; # can't self meet, ever

            $ready->down;

            # with 'redo', loop until 'last' is called
            LIVE: {
                # Meeting place
                {
                    lock $meetings;

                    last LIVE
                      if ( $meetings <= 0 )
                      ;    # 'fade' by jumping out of the block

                    if ( defined $first ) {  # there's someone already waiting to meet
                        $other_color = $first;
                        $second      = $color;
                        cond_signal $meetings;
                        $meetings--;
                        $first = undef;
                    } else {  # nobody is here, wait...
                        $first = $color;
                        cond_wait $meetings;
                        $other_color = $second;
                        $color = $compl_dict->{$color}->{$other_color};
                    }
                }    # Unlock the meeting place

                $met++;
                redo;
            }

            # save my results
            $creature->{met}      = $met;
            $creature->{self_met} = $self_met;
            $creature->{color} = $color;  # last color, for the record
        }
      } @creatures;

    # let them wild
    $ready->up( scalar @creatures );

    # wait for threads to finish
    $_->join for @thrs;

    # report
    sub sum { my $sum = 0; map { $sum += $_ } @_; $sum; }
    print " " . join " ", @colors;
    print "\n";
    print $_->{met} . " " . spellout( $_->{self_met} ) . "\n" for @creatures;
    print spellout sum( map { $_->{met} } @creatures );
}

# main
my $meetings = $ARGV[0] || 600;
check_complement();
print "\n";
meet( $meetings, qw/blue red yellow/ );
print "\n\n";
meet(
    $meetings, qw/blue red yellow red yellow
      blue red yellow red blue/
);
print "\n\n";


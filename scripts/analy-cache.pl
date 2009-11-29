use warnings;
use strict;
no warnings 'uninitialized';

#
# To use uncomment "#define DUMP_PERFORMANCE_INFO" in peg.cpp and
# recompile.  Than run zl on a large set of inputs and redirect the
# results to a log file.  Than pipe all the log files into this
# script.  For example:
#   make -C test clean test
#   cat *-fct.log test/*.log | scripts/analy-cache.pl > res
# This prints a bunch of cache performace stats to stdout and creates
# a suggest grammer.hint file as "grammer.hint.new".
#

my %stat;

my $prod;
my %used_by;
my %persistent;
my %hits;
my @prods;

while (<>) {
    last if /^PARSING DONE/;
    if (/^PARSING (\w+) PROD: (.+)/) {
        $prod = $2;
        push @prods, $prod   if $1 eq 'NAMED';
        $used_by{$prod} = {} if $1 eq 'NAMED' && !defined $used_by{$prod};
    } elsif (/^PARSING    DEP: (.+)/) {
        my $dep = $1;
        $used_by{$dep}{$prod}++;
    } else {
        die "Bad Line: $_";
    }
}

my @category;

foreach my $k (sort keys %used_by) {
    print "$k:";
    my @d = sort keys %{$used_by{$k}};
    
    if (@d == 0) {
        push @{$category[0]}, $k;
    } elsif (@d == 1) {
        my $j = $d[0];
        if ($used_by{$k}{$j} == 1) {
            print " <TRANS>";
            push @{$category[1]}, $k;
        } else {
            print " <LOCAL>";
            push @{$category[2]}, $k;
        }
    } else {
        push @{$category[3]}, $k;
    }
    foreach my $j (@d) {
        print " [$j $used_by{$k}{$j}]";
    }
    print "\n";
}

foreach my $i (0..3) {
    print "CATEGORY $i:\n";
    foreach my $k (@{$category[$i]}) {
        print "  $k\n";
    }
}

while (<>) {
    last if /^PERSISTENT D?ONE/;
}

my %cache_sets;

print "===\n";

while (<>) {
    #print if (/^BEGIN/);
    next unless /^\s*NamedProd/;
    if (/^NamedProd CLEAR/) {
        my $prev;
        foreach my $k (sort keys %stat) {
            my $kd = hex $k;
            $prev = $kd unless defined $prev;
            my $s = '';
            my @d;
            foreach (sort keys %{$stat{$k}}) {
                my $d = $stat{$k}{$_}{res} eq 'FAIL' ? lc $_ : $_;
                $s .= "$d ";
                if ($stat{$k}{$_}{HIT}) {
                    $d .= "/$stat{$k}{$_}{HIT}";
                    $hits{$_}{HIT}++;
                    if (!$stat{$k}{$_}{MISS}) {
                        $d .= "*";
                        $hits{$_}{FIRST}++;
                    }
                    $hits{$_}{FAIL}++ if $stat{$k}{$_}{res} eq 'FAIL';
                }
                #print ">>>$d\n";
                push @d, $d;
            }
            chop $s;
            $cache_sets{$s}++;
            my $num = @d + 0;
            my $d = $kd - $prev;
            #print "$k $d $num: $s\n";
            $prev = $kd;
        }
        #print "---\n";
        %stat = ();
    } else {
        my ($what, $prod, $pos, $res) = /NamedProd (\w+) (\w+) (\w+) (?:\((\w+).*\))?/ or next;
        $stat{$pos}{$prod}{$what}++;
        $stat{$pos}{$prod}{res} = $res if defined $res;
    }
}

print "===\n";

foreach (sort keys %hits) {
    print "$_:";
    my $hit = $hits{$_}{HIT} - $hits{$_}{FAIL};
    print " $hit" if $hit;
    my $fail = $hits{$_}{FAIL};
    print " ($fail)" if $fail;
    my $not_first = $hits{$_}{HIT} - $hits{$_}{FIRST};
    print " $not_first<" if $not_first;
    print "\n";
}

print "===\n";

my @data;

foreach (sort keys %cache_sets) {
    my @d = split / /;
    my @usefull;
    my @fail;
    my @not;
    foreach my $k (@d) {
        my $K = uc $k;
        if (!$hits{$K}) {
            push @not, $k;
        } elsif ($k eq $K) {
            push @usefull, $k;
        } else {
            push @fail, $k;
        }
    }
    push @data, sprintf("%2d %2d %2d: %4d: %s / %s / %s\n", 
                        @usefull+0, @fail+0, @not+0, $cache_sets{$_},
                        "@usefull", "@fail", "@not");
}

foreach (sort {$b cmp $a} @data) {
    print "$_";
}

open F, ">grammer.hint.new";

foreach (@prods) {
    print F "$_:";
    print F " trans" unless $hits{$_};
    print F ";\n";
}

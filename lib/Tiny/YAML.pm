# ABSTRACT: YAML::Tiny Done Right

use 5.008001; # sane UTF-8 support
use strict;
use warnings;
package Tiny::YAML;
$Tiny::YAML::VERSION = '0.0.2';
# XXX-INGY is 5.8.1 too old/broken for utf8?
# XXX-XDG Lancaster consensus was that it was sufficient until
# proven otherwise


#####################################################################
# The Tiny::YAML API.
#
# These are the currently documented API functions/methods and
# exports:

use base 'Exporter';
our @EXPORT = qw{ Load Dump };
our @EXPORT_OK = qw{ LoadFile DumpFile };

###
# Functional/Export API:

# XXX-INGY Returning last document seems a bad behavior.
# XXX-XDG I think first would seem more natural, but I don't know
# that it's worth changing now
sub Load {
    my @data = Tiny::YAML->New->load(@_);
    wantarray ? @data : $data[0];
}

sub LoadFile {
    my $file = shift;
    my @data = Tiny::YAML->New->load_file($file);
    wantarray ? @data : $data[0];
}

sub Dump {
    return Tiny::YAML->new(@_)->_dump_string;
}

sub DumpFile {
    my $file = shift;
    return Tiny::YAML->new(@_)->_dump_file($file);
}


###
# Object Oriented API:

# Create an empty Tiny::YAML object
# XXX-INGY Why do we use ARRAY object?
# NOTE: I get it now, but I think it's confusing and not needed.
# Will change it on a branch later, for review.
#
# XXX-XDG I don't support changing it yet.  It's a very well-documented
# "API" of Tiny::YAML.  I'd support deprecating it, but Adam suggested
# we not change it until YAML.pm's own OO API is established so that
# users only have one API change to digest, not two
sub new {
    my $class = shift;
    bless [ @_ ], $class;
}

# XXX/YTTY - Normal style `new()` for migration.
sub New {
    bless {}, shift;
}


#####################################################################
# Constants

# Printed form of the unprintable characters in the lowest range
# of ASCII characters, listed by ASCII ordinal position.
my @UNPRINTABLE = qw(
    0    x01  x02  x03  x04  x05  x06  a
    b    t    n    v    f    r    x0E  x0F
    x10  x11  x12  x13  x14  x15  x16  x17
    x18  x19  x1A  e    x1C  x1D  x1E  x1F
);

# Printable characters for escapes
my %UNESCAPES = (
    0 => "\x00", z => "\x00", N    => "\x85",
    a => "\x07", b => "\x08", t    => "\x09",
    n => "\x0a", v => "\x0b", f    => "\x0c",
    r => "\x0d", e => "\x1b", '\\' => '\\',
);

# These 3 values have special meaning when unquoted and using the
# default YAML schema. They need quotes if they are strings.
my %QUOTE = map { $_ => 1 } qw{
    null true false
};

#####################################################################
# Tiny::YAML Implementation.
#
# These are the private methods that do all the work. They may change
# at any time.


###
# Loader functions:

# Create an object from a file
sub load_file {
    my $self = shift;

    # Check the file
    my $file = shift or $self->_error( 'You did not specify a file name' );
    $self->_error( "File '$file' does not exist" )
        unless -e $file;
    $self->_error( "'$file' is a directory, not a file" )
        unless -f _;
    $self->_error( "Insufficient permissions to read '$file'" )
        unless -r _;

    # Open unbuffered with strict UTF-8 decoding and no translation layers
    open( my $fh, "<:unix:encoding(UTF-8)", $file );
    unless ( $fh ) {
        $self->_error("Failed to open file '$file': $!");
    }

    # flock if available (or warn if not possible for OS-specific reasons)
    if ( _can_flock() ) {
        flock( $fh, Fcntl::LOCK_SH() )
            or warn "Couldn't lock '$file' for reading: $!";
    }

    # slurp the contents
    my $contents = eval {
        use warnings FATAL => 'utf8';
        local $/;
        <$fh>
    };
    if ( my $err = $@ ) {
        $self->_error("Error reading from file '$file': $err");
    }

    # close the file (release the lock)
    unless ( close $fh ) {
        $self->_error("Failed to close file '$file': $!");
    }

    $self->_load_string( $contents );
}

# Create an object from a string
sub load {
    my $self = shift;
    my $string = $_[0];
#     eval {
        unless ( defined $string ) {
            die \"Did not provide a string to load";
        }

        # Check if Perl has it marked as characters, but it's internally
        # inconsistent.  E.g. maybe latin1 got read on a :utf8 layer
        if ( utf8::is_utf8($string) && ! utf8::valid($string) ) {
            die \<<'...';
Read an invalid UTF-8 string (maybe mixed UTF-8 and 8-bit character set).
Did you decode with lax ":utf8" instead of strict ":encoding(UTF-8)"?
...
        }

        # Ensure Unicode character semantics, even for 0x80-0xff
        utf8::upgrade($string);

        # Check for and strip any leading UTF-8 BOM
        $string =~ s/^\x{FEFF}//;

        return + Pegex::Parser->new(
            grammar => 'Tiny::YAML::Grammar'->new,
            receiver => 'Tiny::YAML::Constructor'->new,
            # debug => 1,
        )->parse($string);
#     };
    if ( ref $@ eq 'SCALAR' ) {
        $self->_error(${$@});
    } elsif ( $@ ) {
        $self->_error($@);
    }
}

# sub _unquote_single {
#     my ($self, $string) = @_;
#     return '' unless length $string;
#     $string =~ s/\'\'/\'/g;
#     return $string;
# }
#
# sub _unquote_double {
#     my ($self, $string) = @_;
#     return '' unless length $string;
#     $string =~ s/\\"/"/g;
#     $string =~
#         s{\\([Nnever\\fartz0b]|x([0-9a-fA-F]{2}))}
#          {(length($1)>1)?pack("H2",$2):$UNESCAPES{$1}}gex;
#     return $string;
# }

###
# Dumper functions:

# Save an object to a file
sub _dump_file {
    my $self = shift;

    require Fcntl;

    # Check the file
    my $file = shift or $self->_error( 'You did not specify a file name' );

    my $fh;
    # flock if available (or warn if not possible for OS-specific reasons)
    if ( _can_flock() ) {
        # Open without truncation (truncate comes after lock)
        my $flags = Fcntl::O_WRONLY()|Fcntl::O_CREAT();
        sysopen( $fh, $file, $flags );
        unless ( $fh ) {
            $self->_error("Failed to open file '$file' for writing: $!");
        }

        # Use no translation and strict UTF-8
        binmode( $fh, ":raw:encoding(UTF-8)");

        flock( $fh, Fcntl::LOCK_EX() )
            or warn "Couldn't lock '$file' for reading: $!";

        # truncate and spew contents
        truncate $fh, 0;
        seek $fh, 0, 0;
    }
    else {
        open $fh, ">:unix:encoding(UTF-8)", $file;
    }

    # serialize and spew to the handle
    print {$fh} $self->_dump_string;

    # close the file (release the lock)
    unless ( close $fh ) {
        $self->_error("Failed to close file '$file': $!");
    }

    return 1;
}

# Save an object to a string
sub _dump_string {
    my $self = shift;
    return '' unless ref $self && @$self;

    # Iterate over the documents
    my $indent = 0;
    my @lines  = ();

    eval {
        foreach my $cursor ( @$self ) {
            push @lines, '---';

            # An empty document
            if ( ! defined $cursor ) {
                # Do nothing

            # A scalar document
            } elsif ( ! ref $cursor ) {
                $lines[-1] .= ' ' . $self->_dump_scalar( $cursor );

            # A list at the root
            } elsif ( ref $cursor eq 'ARRAY' ) {
                unless ( @$cursor ) {
                    $lines[-1] .= ' []';
                    next;
                }
                push @lines, $self->_dump_array( $cursor, $indent, {} );

            # A hash at the root
            } elsif ( ref $cursor eq 'HASH' ) {
                unless ( %$cursor ) {
                    $lines[-1] .= ' {}';
                    next;
                }
                push @lines, $self->_dump_hash( $cursor, $indent, {} );

            } else {
                die \("Cannot serialize " . ref($cursor));
            }
        }
    };
    if ( ref $@ eq 'SCALAR' ) {
        $self->_error(${$@});
    } elsif ( $@ ) {
        $self->_error($@);
    }

    join '', map { "$_\n" } @lines;
}

sub _has_internal_string_value {
    my $value = shift;
    my $b_obj = B::svref_2object(\$value);  # for round trip problem
    return $b_obj->FLAGS & B::SVf_POK();
}

sub _dump_scalar {
    my $string = $_[1];
    my $is_key = $_[2];
    # Check this before checking length or it winds up looking like a string!
    my $has_string_flag = _has_internal_string_value($string);
    return '~'  unless defined $string;
    return "''" unless length  $string;
    if (Scalar::Util::looks_like_number($string)) {
        # keys and values that have been used as strings get quoted
        if ( $is_key || $has_string_flag ) {
            return qq['$string'];
        }
        else {
            return $string;
        }
    }
    if ( $string =~ /[\x00-\x09\x0b-\x0d\x0e-\x1f\x7f-\x9f\'\n]/ ) {
        $string =~ s/\\/\\\\/g;
        $string =~ s/"/\\"/g;
        $string =~ s/\n/\\n/g;
        $string =~ s/[\x85]/\\N/g;
        $string =~ s/([\x00-\x1f])/\\$UNPRINTABLE[ord($1)]/g;
        $string =~ s/([\x7f-\x9f])/'\x' . sprintf("%X",ord($1))/ge;
        return qq|"$string"|;
    }
    if ( $string =~ /(?:^[~!@#%&*|>?:,'"`{}\[\]]|^-+$|\s|:\z)/ or
        $QUOTE{$string}
    ) {
        return "'$string'";
    }
    return $string;
}

sub _dump_array {
    my ($self, $array, $indent, $seen) = @_;
    if ( $seen->{refaddr($array)}++ ) {
        die \"Tiny::YAML does not support circular references";
    }
    my @lines  = ();
    foreach my $el ( @$array ) {
        my $line = ('  ' x $indent) . '-';
        my $type = ref $el;
        if ( ! $type ) {
            $line .= ' ' . $self->_dump_scalar( $el );
            push @lines, $line;

        } elsif ( $type eq 'ARRAY' ) {
            if ( @$el ) {
                push @lines, $line;
                push @lines, $self->_dump_array( $el, $indent + 1, $seen );
            } else {
                $line .= ' []';
                push @lines, $line;
            }

        } elsif ( $type eq 'HASH' ) {
            if ( keys %$el ) {
                push @lines, $line;
                push @lines, $self->_dump_hash( $el, $indent + 1, $seen );
            } else {
                $line .= ' {}';
                push @lines, $line;
            }

        } else {
            die \"Tiny::YAML does not support $type references";
        }
    }

    @lines;
}

sub _dump_hash {
    my ($self, $hash, $indent, $seen) = @_;
    if ( $seen->{refaddr($hash)}++ ) {
        die \"Tiny::YAML does not support circular references";
    }
    my @lines  = ();
    foreach my $name ( sort keys %$hash ) {
        my $el   = $hash->{$name};
        my $line = ('  ' x $indent) . $self->_dump_scalar($name, 1) . ":";
        my $type = ref $el;
        if ( ! $type ) {
            $line .= ' ' . $self->_dump_scalar( $el );
            push @lines, $line;

        } elsif ( $type eq 'ARRAY' ) {
            if ( @$el ) {
                push @lines, $line;
                push @lines, $self->_dump_array( $el, $indent + 1, $seen );
            } else {
                $line .= ' []';
                push @lines, $line;
            }

        } elsif ( $type eq 'HASH' ) {
            if ( keys %$el ) {
                push @lines, $line;
                push @lines, $self->_dump_hash( $el, $indent + 1, $seen );
            } else {
                $line .= ' {}';
                push @lines, $line;
            }

        } else {
            die \"Tiny::YAML does not support $type references";
        }
    }

    @lines;
}


#####################################################################
# DEPRECATED API methods:

# Error storage (DEPRECATED as of 1.57)
our $errstr    = '';

# Set error
sub _error {
    require Carp;
    $errstr = $_[1];
    $errstr =~ s/ at \S+ line \d+.*//;
    Carp::croak( $errstr );
}

# Retrieve error
my $errstr_warned;
sub errstr {
    require Carp;
    Carp::carp( "Tiny::YAML->errstr and \$Tiny::YAML::errstr is deprecated" )
        unless $errstr_warned++;
    $errstr;
}


#####################################################################
# Helper functions. Possibly not needed.


# Use to detect nv or iv
use B;

# XXX-INGY Is flock Tiny::YAML's responsibility?
# Some platforms can't flock :-(
# XXX-XDG I think it is.  When reading and writing files, we ought
# to be locking whenever possible.  People (foolishly) use YAML
# files for things like session storage, which has race issues.
my $HAS_FLOCK;
sub _can_flock {
    if ( defined $HAS_FLOCK ) {
        return $HAS_FLOCK;
    }
    else {
        require Config;
        my $c = \%Config::Config;
        $HAS_FLOCK = grep { $c->{$_} } qw/d_flock d_fcntl_can_lock d_lockf/;
        require Fcntl if $HAS_FLOCK;
        return $HAS_FLOCK;
    }
}


# XXX-INGY Is this core in 5.8.1? Can we remove this?
# XXX-XDG Scalar::Util 1.18 didn't land until 5.8.8, so we need this
#####################################################################
# Use Scalar::Util if possible, otherwise emulate it

BEGIN {
    local $@;
    if ( eval { require Scalar::Util; Scalar::Util->VERSION(1.18); } ) {
        *refaddr = *Scalar::Util::refaddr;
    }
    else {
        eval <<'END_PERL';
# Scalar::Util failed to load or too old
sub refaddr {
    my $pkg = ref($_[0]) or return undef;
    if ( !! UNIVERSAL::can($_[0], 'can') ) {
        bless $_[0], 'Scalar::Util::Fake';
    } else {
        $pkg = undef;
    }
    "$_[0]" =~ /0x(\w+)/;
    my $i = do { no warnings 'portable'; hex $1 };
    bless $_[0], $pkg if defined $pkg;
    $i;
}
END_PERL
    }
}

# For Tiny::YAML we want one simple file. These `INCLUDE`s get inlined before
# going to CPAN. We want to optimize this section over time. It gives us
# something *very* specific to optimize.
use Pegex::Parser;              #INCLUDE
use Pegex::Optimizer;           #INCLUDE
use Tiny::YAML::Grammar;        #INCLUDE
use Tiny::YAML::Constructor;    #INCLUDE

1;

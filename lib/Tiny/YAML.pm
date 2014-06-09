use 5.008001; # sane UTF-8 support
use strict;
use warnings;
package Tiny::YAML;
$Tiny::YAML::VERSION = '0.0.5';
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
    open $fh, ">:unix:encoding(UTF-8)", $file;

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

# Set error
sub _error {
    require Carp;
    my $errstr = $_[1];
    $errstr =~ s/ at \S+ line \d+.*//;
    Carp::croak( $errstr );
}

#####################################################################
# Helper functions. Possibly not needed.


# Use to detect nv or iv
use B;

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

# For Tiny::YAML we want one simple file. These `INLINE`s get inlined before
# going to CPAN. We want to optimize this section over time. It gives us
# something *very* specific to optimize.
#use Pegex::Optimizer;           #INLINE
BEGIN{$INC{'Pegex/Optimizer.pm'} = 'INLINE/Pegex/Optimizer.pm'}
package Pegex::Optimizer;
$Pegex::Optimizer::VERSION = '0.0.5';
$Pegex::Optimizer::VERSION = '0.31';
use Pegex::Base;

has parser => (required => 1);
has grammar => (required => 1);
has receiver => (required => 1);

sub optimize_grammar {
    my ($self, $start) = @_;
    return if $self->grammar->tree->{optimized};
    while (my ($name, $node) = each %{$self->grammar->{tree}}) {
        next unless ref($node);
        $self->optimize_node($node);
    }
    $self->optimize_node({'.ref' => $start});
    $self->{optimized} = 1;
}

sub optimize_node {
    my ($self, $node) = @_;

    my ($min, $max) = @{$node}{'+min', '+max'};
    $node->{'+min'} = defined($max) ? 0 : 1
        unless defined $node->{'+min'};
    $node->{'+max'} = defined($min) ? 0 : 1
        unless defined $node->{'+max'};
    $node->{'+asr'} = 0
        unless defined $node->{'+asr'};

    for my $kind (qw(ref rgx all any err code xxx)) {
        die if $kind eq 'xxx';
        if ($node->{rule} = $node->{".$kind"}) {
            $node->{kind} = $kind;
            $node->{method} = $self->parser->can("match_$kind") or die;
            last;
        }
    }

    if ($node->{kind} =~ /^(?:all|any)$/) {
        $self->optimize_node($_) for @{$node->{rule}};
    }
    elsif ($node->{kind} eq 'ref') {
        my $ref = $node->{rule};
        my $rule = $self->grammar->{tree}{$ref};
        if (my $action = $self->receiver->can("got_$ref")) {
            $rule->{action} = $action;
        }
        elsif (my $gotrule = $self->receiver->can("gotrule")) {
            $rule->{action} = $gotrule;
        }
        $node->{method} = $self->parser->can("match_ref_trace")
            if $self->parser->{debug};
    }
    elsif ($node->{kind} eq 'rgx') {
      # XXX $node;
    }
}

1;
#use Pegex::Grammar;             #INLINE
BEGIN{$INC{'Pegex/Grammar.pm'} = 'INLINE/Pegex/Grammar.pm'}
package Pegex::Grammar;
$Pegex::Grammar::VERSION = '0.0.5';
$Pegex::Grammar::VERSION = '0.31';
use Pegex::Base;

# Grammar can be in text or tree form. Tree will be compiled from text.
# Grammar can also be stored in a file.
has file => ();
has text => (
    builder => 'make_text',
    lazy => 1,
);
has tree => (
    builder => 'make_tree',
    lazy => 1,
);
has start_rules => [];

sub make_text {
    my ($self) = @_;
    my $filename = $self->file
        or return '';
    open TEXT, $filename
        or die "Can't open '$filename' for input\n:$!";
    return do {local $/; <TEXT>}
}

sub make_tree {
    my ($self) = @_;
    my $text = $self->text
        or die "Can't create a '" . ref($self) .
            "' grammar. No tree or text or file.";
    require Pegex::Compiler;
    return Pegex::Compiler->new->compile(
        $text,
        @{$self->start_rules || []}
    )->tree;
}

# This import is to support: perl -MPegex::Grammar::Module=compile
sub import {
    my ($package) = @_;
    if (((caller))[1] =~ /^-e?$/ and @_ == 2 and $_[1] eq 'compile') {
        $package->compile_into_module();
        exit;
    }
    if (my $env = $ENV{PERL_PEGEX_AUTO_COMPILE}) {
        my %modules = map {($_, 1)} split ',', $env;
        if ($modules{$package}) {
            if (my $grammar_file = $package->file) {
                if (-f $grammar_file) {
                    my $module = $package;
                    $module =~ s!::!/!g;
                    $module .= '.pm';
                    my $module_file = $INC{$module};
                    if (-M $grammar_file < -M $module_file) {
                        $package->compile_into_module();
                        local $SIG{__WARN__};
                        delete $INC{$module};
                        require $module;
                    }
                }
            }
        }
    }
}

sub compile_into_module {
    my ($package) = @_;
    my $grammar_file = $package->file;
    open GRAMMAR, $grammar_file
        or die "Can't open $grammar_file for input";
    my $grammar_text = do {local $/; <GRAMMAR>};
    close GRAMMAR;
    my $module = $package;
    $module =~ s!::!/!g;
    $module = "$module.pm";
    my $file = $INC{$module} or return;
    my $perl;
    my @rules;
    if ($package->can('start_rules')) {
        @rules = @{$package->start_rules || []};
    }
    if ($module eq 'Pegex/Pegex/Grammar.pm') {
        require Pegex::Bootstrap;
        $perl = Pegex::Bootstrap->new->compile($grammar_text, @rules)->to_perl;
    }
    else {
        require Pegex::Compiler;
        $perl = Pegex::Compiler->new->compile($grammar_text, @rules)->to_perl;
    }
    open IN, $file or die $!;
    my $module_text = do {local $/; <IN>};
    close IN;
    $perl =~ s/^/  /gm;
    $module_text =~ s/^(sub\s+make_tree\s*\{).*?(^\})/$1\n$perl$2/ms;
    open OUT, '>', $file or die $!;
    print OUT $module_text;
    close OUT;
}

1;
#use Pegex::Tree;                #INLINE
BEGIN{$INC{'Pegex/Tree.pm'} = 'INLINE/Pegex/Tree.pm'}
package Pegex::Tree;
$Pegex::Tree::VERSION = '0.0.5';
$Pegex::Tree::VERSION = '0.31';
use Pegex::Base;
extends 'Pegex::Receiver';

sub gotrule {
    my $self = shift;
    @_ || return ();
    return {$self->{parser}{rule} => $_[0]}
        if $self->{parser}{parent}{-wrap};
    return $_[0];
}

sub final {
    my $self = shift;
    return(shift) if @_;
    return [];
}

1;
#use Pegex::Parser;              #INLINE
BEGIN{$INC{'Pegex/Parser.pm'} = 'INLINE/Pegex/Parser.pm'}
package Pegex::Parser;
$Pegex::Parser::VERSION = '0.0.5';
$Pegex::Parser::VERSION = '0.31';
use Pegex::Base;
use Pegex::Input;
use Pegex::Optimizer;
use Scalar::Util;

{
    package Pegex::Constant;
$Pegex::Constant::VERSION = '0.0.5';
$Pegex::Constant::VERSION = '0.31';
our $Null = [];
    our $Dummy = [];
}

has grammar => (required => 1);
has receiver => ();
has input => ();

has rule => ();
has parent => ();
has 'debug' => (
    default => sub {
        exists($ENV{PERL_PEGEX_DEBUG}) ? $ENV{PERL_PEGEX_DEBUG} :
        defined($Pegex::Parser::Debug) ? $Pegex::Parser::Debug :
        0;
    },
);

has position => 0;
has farthest => 0;

has throw_on_error => 1;

sub parse {
    my ($self, $input, $start) = @_;

    if ($start) {
        $start =~ s/-/_/g;
    }

    $self->{position} = 0;
    $self->{farthest} = 0;

    if (not ref $input or not UNIVERSAL::isa($input, 'Pegex::Input')) {
        $input = Pegex::Input->new(string => $input);
    }
    $self->{input} = $input;
    $self->{input}->open unless $self->{input}{_is_open};
    $self->{buffer} = $self->{input}->read;
    $self->{length} = length ${$self->{buffer}};

    die "No 'grammar'. Can't parse" unless $self->{grammar};

    $self->{grammar}{tree} = $self->{grammar}->make_tree
        unless defined $self->{grammar}{tree};

    my $start_rule_ref = $start ||
        $self->{grammar}{tree}{'+toprule'} ||
        ($self->{grammar}{tree}{'TOP'} ? 'TOP' : undef)
            or die "No starting rule for Pegex::Parser::parse";

    die "No 'receiver'. Can't parse" unless $self->{receiver};

    Pegex::Optimizer->new(
        parser => $self,
        grammar => $self->{grammar},
        receiver => $self->{receiver},
    )->optimize_grammar($start_rule_ref);

    # Add circular ref and weaken it.
    $self->{receiver}{parser} = $self;
    Scalar::Util::weaken($self->{receiver}{parser});

    if ($self->{receiver}->can("initial")) {
        $self->{rule} = $start_rule_ref;
        $self->{parent} = {};
        $self->{receiver}->initial();
    }

    my $match = $self->match_ref($start_rule_ref, {});

    $self->{input}->close;

    if (not $match or $self->{position} < $self->{length}) {
        $self->throw_error("Parse document failed for some reason");
        return;  # In case $self->throw_on_error is off
    }

    if ($self->{receiver}->can("final")) {
        $self->{rule} = $start_rule_ref;
        $self->{parent} = {};
        # XXX mismatch with ruby port
        $match = [ $self->{receiver}->final(@$match) ];
    }

    return $match->[0];
}

sub match_next {
    my ($self, $next) = @_;

    my ($rule, $method, $kind, $min, $max, $assertion) =
        @{$next}{'rule', 'method', 'kind', '+min', '+max', '+asr'};

    my ($position, $match, $count) =
        ($self->{position}, [], 0);

    while (my $return = $method->($self, $rule, $next)) {
        $position = $self->{position} unless $assertion;
        $count++;
        push @$match, @$return;
        last if $max == 1;
    }
    if (not $count and $min == 0 and $kind eq 'all') {
        $match = [[]];
    }
    if ($max != 1) {
        if ($next->{-flat}) {
            $match = [ map { (ref($_) eq 'ARRAY') ? (@$_) : ($_) } @$match ];
        }
        else {
            $match = [$match]
        }
        $self->{farthest} = $position
            if ($self->{position} = $position) > $self->{farthest};
    }
    my $result = ($count >= $min and (not $max or $count <= $max))
        ^ ($assertion == -1);
    if (not($result) or $assertion) {
        $self->{farthest} = $position
            if ($self->{position} = $position) > $self->{farthest};
    }

    # YYY ($result ? $next->{'-skip'} ? [] : $match : 0) if $main::x;
    return ($result ? $next->{'-skip'} ? [] : $match : 0);
}

sub match_ref {
    my ($self, $ref, $parent) = @_;
    my $rule = $self->{grammar}{tree}{$ref}
        or die "No rule defined for '$ref'";
    my $match = $self->match_next($rule) or return 0;
    return $Pegex::Constant::Dummy unless $rule->{action};
    @{$self}{'rule', 'parent'} = ($ref, $parent);
    # XXX API mismatch
    [ $rule->{action}->($self->{receiver}, @$match) ];
}

sub match_rgx {
    my ($self, $regexp) = @_;
    my $buffer = $self->{buffer};

    pos($$buffer) = $self->{position};

    $$buffer =~ /$regexp/g or return 0;
    $self->{position} = pos($$buffer);

    no strict 'refs';
    my $match = [ map $$_, 1..$#+ ];
    $match = [ $match ] if $#+ > 1;
    $self->{farthest} = $self->{position}
        if $self->{position} > $self->{farthest};
    return $match;
}

sub match_all {
    my ($self, $list) = @_;
    my $position = $self->{position};
    my $set = [];
    my $len = 0;
    for my $elem (@$list) {
        if (my $match = $self->match_next($elem)) {
            if (not ($elem->{'+asr'} or $elem->{'-skip'})) {
                push @$set, @$match;
                $len++;
            }
        }
        else {
            $self->{farthest} = $position
                if ($self->{position} = $position) > $self->{farthest};
            return 0;
        }
    }
    $set = [ $set ] if $len > 1;
    return $set;
}

sub match_any {
    my ($self, $list) = @_;
    for my $elem (@$list) {
        if (my $match = $self->match_next($elem)) {
            return $match;
        }
    }
    return 0;
}

sub match_err {
    my ($self, $error) = @_;
    $self->throw_error($error);
}

sub match_ref_trace {
    my ($self, $ref, $parent) = @_;
    my $asr = $parent->{'+asr'};
    my $note =
        $asr == -1 ? '(!)' :
        $asr == 1 ? '(=)' :
        '';
    $self->trace("try_$ref$note");
    my $result;
    if ($result = $self->match_ref($ref)) {
        $self->trace("got_$ref$note");
    }
    else {
        $self->trace("not_$ref$note");
    }
    return $result;
}

sub trace {
    my ($self, $action) = @_;
    my $indent = ($action =~ /^try_/) ? 1 : 0;
    $self->{indent} ||= 0;
    $self->{indent}-- unless $indent;
    print STDERR ' ' x $self->{indent};
    $self->{indent}++ if $indent;
    my $snippet = substr(${$self->{buffer}}, $self->{position});
    $snippet = substr($snippet, 0, 30) . "..." if length $snippet > 30;
    $snippet =~ s/\n/\\n/g;
    print STDERR sprintf("%-30s", $action) .
        ($indent ? " >$snippet<\n" : "\n");
}

sub throw_error {
    my ($self, $msg) = @_;
    $@ = $self->{error} = $self->format_error($msg);
    return undef unless $self->{throw_on_error};
    require Carp;
    Carp::croak($self->{error});
}

sub format_error {
    my ($self, $msg) = @_;
    my $buffer = $self->{buffer};
    my $position = $self->{farthest};
    my $real_pos = $self->{position};

    my $line = @{[substr($$buffer, 0, $position) =~ /(\n)/g]} + 1;
    my $column = $position - rindex($$buffer, "\n", $position);

    my $pretext = substr(
        $$buffer,
        $position < 50 ? 0 : $position - 50,
        $position < 50 ? $position : 50
    );
    my $context = substr($$buffer, $position, 50);
    $pretext =~ s/.*\n//gs;
    $context =~ s/\n/\\n/g;

    return <<"...";
Error parsing Pegex document:
  msg:      $msg
  line:     $line
  column:   $column
  context:  $pretext$context
  ${\ (' ' x (length($pretext) + 10) . '^')}
  position: $position ($real_pos pre-lookahead)
...
}

1;
#use Tiny::YAML::Grammar;        #INLINE
BEGIN{$INC{'Tiny/YAML/Grammar.pm'} = 'INLINE/Tiny/YAML/Grammar.pm'}
package Tiny::YAML::Grammar;
$Tiny::YAML::Grammar::VERSION = '0.0.5';

use base 'Pegex::Grammar';

use constant file => '../yaml-pegex-pm/share/yaml.pgx';

sub make_tree {
  {
    '+grammar' => 'yaml',
    '+toprule' => 'yaml_stream',
    '+version' => '0.0.1',
    'EOL' => {
      '.rgx' => qr/\G\r?\n/
    },
    'SPACE' => {
      '.rgx' => qr/\G\ /
    },
    'block_indent' => {
      '.rgx' => qr/\G/
    },
    'block_key' => {
      '.ref' => 'block_scalar'
    },
    'block_mapping' => {
      '.all' => [
        {
          '.ref' => 'block_indent'
        },
        {
          '.all' => [
            {
              '.ref' => 'block_mapping_pair'
            },
            {
              '+min' => 0,
              '-flat' => 1,
              '.all' => [
                {
                  '.ref' => 'ignore_line'
                },
                {
                  '.ref' => 'block_mapping_pair'
                }
              ]
            }
          ]
        },
        {
          '.all' => [
            {
              '.ref' => 'EOL'
            },
            {
              '.ref' => 'block_undent'
            }
          ]
        }
      ]
    },
    'block_mapping_pair' => {
      '.all' => [
        {
          '.ref' => 'block_ondent'
        },
        {
          '.ref' => 'block_key'
        },
        {
          '.ref' => 'mapping_separator'
        },
        {
          '.ref' => 'block_value'
        }
      ]
    },
    'block_ondent' => {
      '.rgx' => qr/\G/
    },
    'block_scalar' => {
      '.rgx' => qr/\G(\|\r?\nXXX|\>\r?\nXXX|"[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`]).+?(?=:\ |\r?\n|\z))/
    },
    'block_sequence' => {
      '.all' => [
        {
          '.ref' => 'block_sequence_entry'
        },
        {
          '+min' => 0,
          '-flat' => 1,
          '.all' => [
            {
              '.ref' => 'list_separator'
            },
            {
              '.ref' => 'block_sequence_entry'
            }
          ]
        },
        {
          '+max' => 1,
          '.ref' => 'list_separator'
        }
      ]
    },
    'block_sequence_entry' => {
      '.rgx' => qr/\G\-\ +(\|\r?\nXXX|\>\r?\nXXX|"[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`]).+?(?=:\ |\r?\n|\z))\r?\n/
    },
    'block_undent' => {
      '.rgx' => qr/\G/
    },
    'block_value' => {
      '.any' => [
        {
          '.ref' => 'block_scalar'
        },
        {
          '.ref' => 'flow_node'
        }
      ]
    },
    'document_foot' => {
      '.rgx' => qr/\G\.\.\.\r?\n/
    },
    'document_head' => {
      '.rgx' => qr/\G\-\-\-(?:\ +|(?=\r?\n))/
    },
    'flow_mapping' => {
      '.all' => [
        {
          '.ref' => 'flow_mapping_start'
        },
        {
          '+max' => 1,
          '.all' => [
            {
              '.ref' => 'flow_mapping_pair'
            },
            {
              '+min' => 0,
              '-flat' => 1,
              '.all' => [
                {
                  '.ref' => 'list_separator'
                },
                {
                  '.ref' => 'flow_mapping_pair'
                }
              ]
            },
            {
              '+max' => 1,
              '.ref' => 'list_separator'
            }
          ]
        },
        {
          '.ref' => 'flow_mapping_end'
        }
      ]
    },
    'flow_mapping_end' => {
      '.rgx' => qr/\G\s*\}\s*/
    },
    'flow_mapping_pair' => {
      '.all' => [
        {
          '.ref' => 'flow_node'
        },
        {
          '.ref' => 'mapping_separator'
        },
        {
          '.ref' => 'flow_node'
        }
      ]
    },
    'flow_mapping_start' => {
      '.rgx' => qr/\G\s*\{\s*/
    },
    'flow_node' => {
      '.any' => [
        {
          '.ref' => 'flow_sequence'
        },
        {
          '.ref' => 'flow_mapping'
        },
        {
          '.ref' => 'flow_scalar'
        }
      ]
    },
    'flow_scalar' => {
      '.rgx' => qr/\G("[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`]).+?(?=[&\*\#\{\}\[\]%,]|:\ |,\ |\r?\n|\z))/
    },
    'flow_sequence' => {
      '.all' => [
        {
          '.ref' => 'flow_sequence_start'
        },
        {
          '+max' => 1,
          '.all' => [
            {
              '.ref' => 'flow_sequence_entry'
            },
            {
              '+min' => 0,
              '-flat' => 1,
              '.all' => [
                {
                  '.ref' => 'list_separator'
                },
                {
                  '.ref' => 'flow_sequence_entry'
                }
              ]
            },
            {
              '+max' => 1,
              '.ref' => 'list_separator'
            }
          ]
        },
        {
          '.ref' => 'flow_sequence_end'
        }
      ]
    },
    'flow_sequence_end' => {
      '.rgx' => qr/\G\s*\]\s*/
    },
    'flow_sequence_entry' => {
      '.ref' => 'flow_scalar'
    },
    'flow_sequence_start' => {
      '.rgx' => qr/\G\s*\[\s*/
    },
    'ignore_line' => {
      '.rgx' => qr/\G(?:[\ \t]*|\#.*)\r?\n/
    },
    'list_separator' => {
      '.rgx' => qr/\G,\ +/
    },
    'mapping_separator' => {
      '.rgx' => qr/\G:\ +/
    },
    'node_alias' => {
      '.rgx' => qr/\G\*(\w+)/
    },
    'node_anchor' => {
      '.rgx' => qr/\G\&(\w+)/
    },
    'node_prefix' => {
      '.any' => [
        {
          '.all' => [
            {
              '.ref' => 'node_anchor'
            },
            {
              '+max' => 1,
              '.all' => [
                {
                  '+min' => 1,
                  '.ref' => 'SPACE'
                },
                {
                  '.ref' => 'node_tag'
                }
              ]
            }
          ]
        },
        {
          '.all' => [
            {
              '.ref' => 'node_tag'
            },
            {
              '+max' => 1,
              '.all' => [
                {
                  '+min' => 1,
                  '.ref' => 'SPACE'
                },
                {
                  '.ref' => 'node_anchor'
                }
              ]
            }
          ]
        }
      ]
    },
    'node_tag' => {
      '.rgx' => qr/\G!!?(\w+)/
    },
    'top_node' => {
      '.all' => [
        {
          '+max' => 1,
          '.ref' => 'node_prefix'
        },
        {
          '.any' => [
            {
              '.ref' => 'node_alias'
            },
            {
              '.ref' => 'flow_mapping'
            },
            {
              '.ref' => 'flow_sequence'
            },
            {
              '.ref' => 'block_sequence'
            },
            {
              '.ref' => 'block_mapping'
            },
            {
              '.ref' => 'block_scalar'
            }
          ]
        }
      ]
    },
    'yaml_document' => {
      '.all' => [
        {
          '+max' => 1,
          '.ref' => 'document_head'
        },
        {
          '.ref' => 'top_node'
        },
        {
          '+max' => 1,
          '.ref' => 'ignore_line'
        },
        {
          '+max' => 1,
          '.ref' => 'document_foot'
        }
      ]
    },
    'yaml_stream' => {
      '.all' => [
        {
          '+min' => 0,
          '.ref' => 'ignore_line'
        },
        {
          '+min' => 0,
          '.all' => [
            {
              '.ref' => 'yaml_document'
            },
            {
              '+min' => 0,
              '.ref' => 'ignore_line'
            }
          ]
        }
      ]
    }
  }
}

1;
#use Tiny::YAML::Constructor;    #INLINE
BEGIN{$INC{'Tiny/YAML/Constructor.pm'} = 'INLINE/Tiny/YAML/Constructor.pm'}
use strict;
package Tiny::YAML::Constructor;
$Tiny::YAML::Constructor::VERSION = '0.0.5';
use base 'Pegex::Tree';
# use XXX -with => 'YAML::XS';

sub init {
    my ($self) = @_;
    $self->{data} = [];
    return;
}

sub final {
    my ($self) = @_;
    return @{$self->{data}};
}

sub got_block_mapping {
    my ($self, $got) = @_;
    my $key = $got->[0][0][0];
    my $value = $got->[0][0][1];
    return {$key, $value};
}

sub got_yaml_document {
    my ($self, $got) = @_;
    push @{$self->{data}}, $got->[0][0];
    return;
}

1;

1;

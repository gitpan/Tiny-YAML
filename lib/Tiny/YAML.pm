use strict; use warnings;
package Tiny::YAML;
our $VERSION = '0.0.8';

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
        grammar => 'YAML::Pegex::Grammar'->new,
        receiver => 'Tiny::YAML::Constructor'->new,
        # debug => 1,
    )->parse($string);

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

no strict;  # Needed for Pegex::Base to compile.
#use Pegex::Base();              #INLINE
BEGIN { $INC{'Pegex/Base.pm'} = 'INLINE/Pegex/Base.pm' }
BEGIN {
#line 1 "Pegex::Base"
package
Pegex::Base;
# use Mo qw'build default builder xxx import nonlazy';
#   The following line of code was produced from the previous line by
#   Mo::Inline version 0.38
no warnings;my$M=__PACKAGE__.'::';*{$M.Object::new}=sub{my$c=shift;my$s=bless{@_},$c;my%n=%{$c.::.':E'};map{$s->{$_}=$n{$_}->()if!exists$s->{$_}}keys%n;$s};*{$M.import}=sub{import warnings;$^H|=1538;my($P,%e,%o)=caller.'::';shift;eval"no Mo::$_",&{$M.$_.::e}($P,\%e,\%o,\@_)for@_;return if$e{M};%e=(extends,sub{eval"no $_[0]()";@{$P.ISA}=$_[0]},has,sub{my$n=shift;my$m=sub{$#_?$_[0]{$n}=$_[1]:$_[0]{$n}};@_=(default,@_)if!($#_%2);$m=$o{$_}->($m,$n,@_)for sort keys%o;*{$P.$n}=$m},%e,);*{$P.$_}=$e{$_}for keys%e;@{$P.ISA}=$M.Object};*{$M.'build::e'}=sub{my($P,$e)=@_;$e->{new}=sub{$c=shift;my$s=&{$M.Object::new}($c,@_);my@B;do{@B=($c.::BUILD,@B)}while($c)=@{$c.::ISA};exists&$_&&&$_($s)for@B;$s}};*{$M.'default::e'}=sub{my($P,$e,$o)=@_;$o->{default}=sub{my($m,$n,%a)=@_;exists$a{default}or return$m;my($d,$r)=$a{default};my$g='HASH'eq($r=ref$d)?sub{+{%$d}}:'ARRAY'eq$r?sub{[@$d]}:'CODE'eq$r?$d:sub{$d};my$i=exists$a{lazy}?$a{lazy}:!${$P.':N'};$i or ${$P.':E'}{$n}=$g and return$m;sub{$#_?$m->(@_):!exists$_[0]{$n}?$_[0]{$n}=$g->(@_):$m->(@_)}}};*{$M.'builder::e'}=sub{my($P,$e,$o)=@_;$o->{builder}=sub{my($m,$n,%a)=@_;my$b=$a{builder}or return$m;my$i=exists$a{lazy}?$a{lazy}:!${$P.':N'};$i or ${$P.':E'}{$n}=\&{$P.$b}and return$m;sub{$#_?$m->(@_):!exists$_[0]{$n}?$_[0]{$n}=$_[0]->$b:$m->(@_)}}};use constant XXX_skip=>1;my$dm='YAML::XS';*{$M.'xxx::e'}=sub{my($P,$e)=@_;$e->{WWW}=sub{require XXX;local$XXX::DumpModule=$dm;XXX::WWW(@_)};$e->{XXX}=sub{require XXX;local$XXX::DumpModule=$dm;XXX::XXX(@_)};$e->{YYY}=sub{require XXX;local$XXX::DumpModule=$dm;XXX::YYY(@_)};$e->{ZZZ}=sub{require XXX;local$XXX::DumpModule=$dm}};my$i=\&import;*{$M.import}=sub{(@_==2 and not$_[1])?pop@_:@_==1?push@_,grep!/import/,@f:();goto&$i};*{$M.'nonlazy::e'}=sub{${shift.':N'}=1};@f=qw[build default builder xxx import nonlazy];use strict;use warnings;

our $DumpModule = 'YAML';
}
use strict;
#use Pegex::Optimizer;           #INLINE
BEGIN { $INC{'Pegex/Optimizer.pm'} = 'INLINE/Pegex/Optimizer.pm' }
BEGIN {
#line 1 "Pegex::Optimizer"
package
Pegex::Optimizer;
use Pegex::Base;

has parser => (required => 1);
has grammar => (required => 1);
has receiver => (required => 1);

sub optimize_grammar {
    my ($self, $start) = @_;
    my $tree = $self->grammar->{tree};
    return if $tree->{'+optimized'};
    $self->set_max_parse if $self->parser->{maxparse};
    $self->{extra} = {};
    while (my ($name, $node) = each %$tree) {
        next unless ref($node);
        $self->optimize_node($node);
    }
    $self->optimize_node({'.ref' => $start});
    my $extra = delete $self->{extra};
    for my $key (%$extra) {
        $tree->{$key} = $extra->{$key};
    }
    $tree->{'+optimized'} = 1;
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
        return if $kind eq 'xxx';
        if ($node->{rule} = $node->{".$kind"}) {
            delete $node->{".$kind"};
            $node->{kind} = $kind;
            if ($kind eq 'ref') {
                my $rule = $node->{rule} or die;
                if (my $method = $self->grammar->can("rule_$rule")) {
                    $node->{method} = $self->make_method_wrapper($method);
                }
                elsif (not $self->grammar->{tree}{$rule}) {
                    if (my $method = $self->grammar->can("$rule")) {
                        warn <<"...";
Warning:

    You have a method called '$rule' in your grammar.
    It should probably be called 'rule_$rule'.

...
                    }
                    die "No rule '$rule' defined in grammar";
                }
            }
            $node->{method} ||= $self->parser->can("match_$kind") or die;
            last;
        }
    }

    if ($node->{kind} =~ /^(?:all|any)$/) {
        $self->optimize_node($_) for @{$node->{rule}};
    }
    elsif ($node->{kind} eq 'ref') {
        my $ref = $node->{rule};
        my $rule = $self->grammar->{tree}{$ref};
        $rule ||= $self->{extra}{$ref} = {};
        if (my $action = $self->receiver->can("got_$ref")) {
            $rule->{action} = $action;
        }
        elsif (my $gotrule = $self->receiver->can("gotrule")) {
            $rule->{action} = $gotrule;
        }
        if ($self->parser->{debug}) {
            $node->{method} = $self->make_trace_wrapper($node->{method});
        }
    }
    elsif ($node->{kind} eq 'rgx') {
      # XXX $node;
    }
}

sub make_method_wrapper {
    my ($self, $method) = @_;
    return sub {
        my ($parser, $ref, $parent) = @_;
        @{$parser}{'rule', 'parent'} = ($ref, $parent);
        $method->(
            $parser->{grammar},
            $parser,
            $parser->{buffer},
            $parser->{position},
        );
    }
}

sub make_trace_wrapper {
    my ($self, $method) = @_;
    return sub {
        my ($self, $ref, $parent) = @_;
        my $asr = $parent->{'+asr'};
        my $note =
            $asr == -1 ? '(!)' :
            $asr == 1 ? '(=)' :
            '';
        $self->trace("try_$ref$note");
        my $result;
        if ($result = $self->$method($ref, $parent)) {
            $self->trace("got_$ref$note");
        }
        else {
            $self->trace("not_$ref$note");
        }
        return $result;
    }
}

sub set_max_parse {
    require Pegex::Parser;
    my ($self) = @_;
    my $maxparse = $self->parser->{maxparse};
    no warnings 'redefine';
    my $method = \&Pegex::Parser::match_ref;
    my $counter = 0;
    *Pegex::Parser::match_ref = sub {
        die "Maximum parsing rules reached ($maxparse)\n"
            if $counter++ >= $maxparse;
        my $self = shift;
        $self->$method(@_);
    };
}
}
#use Pegex::Grammar;             #INLINE
BEGIN { $INC{'Pegex/Grammar.pm'} = 'INLINE/Pegex/Grammar.pm' }
BEGIN {
#line 1 "Pegex::Grammar"
package
Pegex::Grammar;
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
    print "Compiled '$grammar_file' into '$file'.\n";
}
}
#use Pegex::Tree;                #INLINE
BEGIN { $INC{'Pegex/Tree.pm'} = 'INLINE/Pegex/Tree.pm' }
BEGIN {
#line 1 "Pegex::Tree"
package
Pegex::Tree;
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
}
#use Pegex::Input;               #INLINE
BEGIN { $INC{'Pegex/Input.pm'} = 'INLINE/Pegex/Input.pm' }
BEGIN {
#line 1 "Pegex::Input"
package
Pegex::Input;

use Pegex::Base;

has string => ();
has stringref => ();
has file => ();
has handle => ();
has _buffer => ();
has _is_eof => 0;
has _is_open => 0;
has _is_close => 0;

# NOTE: Current implementation reads entire input into _buffer on open().
sub read {
    my ($self) = @_;
    die "Attempted Pegex::Input::read before open" if not $self->{_is_open};
    die "Attempted Pegex::Input::read after EOF" if $self->{_is_eof};

    my $buffer = $self->{_buffer};
    $self->{_buffer} = undef;
    $self->{_is_eof} = 1;

    return $buffer;
}

sub open {
    my ($self) = @_;
    die "Attempted to reopen Pegex::Input object"
        if $self->{_is_open} or $self->{_is_close};

    if (my $ref = $self->{stringref}) {
        $self->{_buffer} = $ref;
    }
    elsif (my $handle = $self->{handle}) {
        $self->{_buffer} = \ do { local $/; <$handle> };
    }
    elsif (my $path = $self->{file}) {
        open my $handle, $path
            or die "Pegex::Input can't open $path for input:\n$!";
        $self->{_buffer} = \ do { local $/; <$handle> };
    }
    elsif (exists $self->{string}) {
        $self->{_buffer} = \$self->{string};
    }
    else {
        die "Pegex::Input::open failed. No source to open";
    }
    $self->{_is_open} = 1;
    return $self;
}

sub close {
    my ($self) = @_;
    die "Attempted to close an unopen Pegex::Input object"
        if $self->{_is_close};
    close $self->{handle} if $self->{handle};
    $self->{_is_open} = 0;
    $self->{_is_close} = 1;
    $self->{_buffer} = undef;
    return $self;
}
}
#use Pegex::Parser;              #INLINE
BEGIN { $INC{'Pegex/Parser.pm'} = 'INLINE/Pegex/Parser.pm' }
BEGIN {
#line 1 "Pegex::Parser"
package
Pegex::Parser;
use Pegex::Base;

use Pegex::Input;
use Pegex::Optimizer;
use Scalar::Util;

{
    package
Pegex::Constant;
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
    # XXX Add an optional $position argument. Default to 0. This is the
    # position to start parsing. Set position and farthest below to this
    # value. Allows for sub-parsing. Need to somehow return the finishing
    # position of a subparse. Maybe this all goes in a subparse() method.
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

    $self->{optimizer} = Pegex::Optimizer->new(
        parser => $self,
        grammar => $self->{grammar},
        receiver => $self->{receiver},
    );
    $self->{optimizer}->optimize_grammar($start_rule_ref);

    # Add circular ref and weaken it.
    $self->{receiver}{parser} = $self;
    Scalar::Util::weaken($self->{receiver}{parser});

    if ($self->{receiver}->can("initial")) {
        $self->{rule} = $start_rule_ref;
        $self->{parent} = {};
        $self->{receiver}->initial();
    }

    my $match = $self->debug ? do {
        my $method = $self->{optimizer}->make_trace_wrapper(\&match_ref);
        $self->$method($start_rule_ref, {'+asr' => 0});
    } : $self->match_ref($start_rule_ref, {});

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

sub match_rule {
    my ($self, $position, $match) = (@_, []);
    $self->{position} = $position;
    $self->{farthest} = $self->{position}
        if $self->{position} > $self->{farthest};
    $match = [ $match ] if @$match > 1;
    my ($ref, $parent) = @{$self}{'rule', 'parent'};
    my $rule = $self->{grammar}{tree}{$ref}
        or die "No rule defined for '$ref'";

    [ $rule->{action}->($self->{receiver}, @$match) ];
}

sub match_ref {
    my ($self, $ref, $parent) = @_;
    my $rule = $self->{grammar}{tree}{$ref}
        or die "No rule defined for '$ref'";
    my $match = $self->match_next($rule) or return;
    return $Pegex::Constant::Dummy unless $rule->{action};
    @{$self}{'rule', 'parent'} = ($ref, $parent);

    # XXX Possible API mismatch.
    # Not sure if we should "splat" the $match.
    [ $rule->{action}->($self->{receiver}, @$match) ];
}

sub match_rgx {
    my ($self, $regexp) = @_;
    my $buffer = $self->{buffer};

    pos($$buffer) = $self->{position};

    $$buffer =~ /$regexp/g or return;
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
            return;
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
    return;
}

sub match_err {
    my ($self, $error) = @_;
    $self->throw_error($error);
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
}
#use YAML::Pegex::Grammar 0.0.8; #INLINE
BEGIN { $INC{'YAML/Pegex/Grammar.pm'} = 'INLINE/YAML/Pegex/Grammar.pm' }
BEGIN {
#line 1 "YAML::Pegex::Grammar"
use strict; use warnings;
package
YAML::Pegex::Grammar;
our $VERSION = '0.0.8';

use Pegex::Base;
extends 'Pegex::Grammar';

use constant file => '../yaml-pgx/yaml.pgx';

has indent => [];

sub rule_block_indent {
    my ($self, $parser, $buffer, $pos) = @_;
    my $indents = $self->{indent};
    pos($$buffer) = $pos;
    return if $pos >= length($$buffer);
    if ($pos == 0) {
        $$buffer =~ /\G( *)(?=[^\s\#])/g or die;
        push @$indents, length($1);
        return $parser->match_rule($pos);
    }
    my $len = @$indents ? $indents->[-1] + 1 : 0;
    $$buffer =~ /\G\r?\n( {$len,})(?=[^\s\#])/g or return;
    push @$indents, length($1);
    return $parser->match_rule($pos);
}

sub rule_block_ondent {
    my ($self, $parser, $buffer, $pos) = @_;
    my $indents = $self->{indent};
    my $len = $indents->[-1];
    my $re = $pos > 0 ? '\r?\n' : '';
    pos($$buffer) = $pos;
    $$buffer =~ /\G$re( {$len})(?=[^\s\#])/g or return;
    return $parser->match_rule(pos($$buffer));
}

sub rule_block_undent {
    my ($self, $parser, $buffer, $pos) = @_;
    my $indents = $self->{indent};
    return unless @$indents;
    my $len = $indents->[-1];
    pos($$buffer) = $pos;
    if ($$buffer =~ /\G((?:\r?\n)?)(?=\z|\.\.\.\r?\n|\-\-\-\r?\n)/ or
        $$buffer !~ /\G\r?\n( {$len})/g
    ) {
        pop @$indents;
        return $parser->match_rule($pos);
    }
    return;
}

# sub make_tree {
#     use Pegex::Bootstrap;
#     use IO::All;
#     my $grammar = io->file(file)->all;
#     Pegex::Bootstrap->new->compile($grammar)->tree;
# }
# sub make_treeXXX {
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
    'block_key' => {
      '.rgx' => qr/\G(\|\r?\nXXX|\>\r?\nXXX|"[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`\@]).+?(?=:\s|\r?\n|\z)):(?:\ +|\ *(?=\r?\n))/
    },
    'block_mapping' => {
      '.all' => [
        {
          '.ref' => 'block_indent'
        },
        {
          '+min' => 1,
          '.ref' => 'block_mapping_pair'
        },
        {
          '.ref' => 'block_undent'
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
          '.ref' => 'block_value'
        }
      ]
    },
    'block_node' => {
      '.any' => [
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
    },
    'block_scalar' => {
      '.rgx' => qr/\G(\|\r?\nXXX|\>\r?\nXXX|"[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`\@]).+?(?=:\s|\r?\n|\z))/
    },
    'block_sequence' => {
      '+min' => 1,
      '.ref' => 'block_sequence_entry'
    },
    'block_sequence_entry' => {
      '.rgx' => qr/\G\-\ +(\|\r?\nXXX|\>\r?\nXXX|"[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`\@]).+?(?=:\s|\r?\n|\z))\r?\n/
    },
    'block_value' => {
      '.any' => [
        {
          '.ref' => 'flow_mapping'
        },
        {
          '.ref' => 'flow_sequence'
        },
        {
          '.ref' => 'block_node'
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
          '.ref' => 'flow_mapping_separator'
        },
        {
          '.ref' => 'flow_node'
        }
      ]
    },
    'flow_mapping_separator' => {
      '.rgx' => qr/\G:(?:\ +|\ *(?=\r?\n))/
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
      '.rgx' => qr/\G("[^"]*"|'[^']*'|(?![&\*\#\{\}\[\]%`\@]).+?(?=[&\*\#\{\}\[\]%,]|:\ |,\ |\r?\n|\z))/
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
      '.rgx' => qr/\G(?:\#.*|[\ \t]*)(?=\r?\n)/
    },
    'list_separator' => {
      '.rgx' => qr/\G,\ +/
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
        },
        {
          '+max' => 1,
          '.ref' => 'EOL'
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
}
#use Tiny::YAML::Constructor;    #INLINE
BEGIN { $INC{'Tiny/YAML/Constructor.pm'} = 'INLINE/Tiny/YAML/Constructor.pm' }
BEGIN {
#line 1 "Tiny::YAML::Constructor"
use strict; use warnings;
package
Tiny::YAML::Constructor;
use Pegex::Base;
extends 'Pegex::Tree';

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
    return +{
        map {
            @$_
        } @{$got->[0]}
    };
}

sub got_yaml_document {
    my ($self, $got) = @_;
    push @{$self->{data}}, $got->[0][0];
    return;
}
}

1;

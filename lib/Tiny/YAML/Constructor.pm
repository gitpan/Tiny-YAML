use strict;
package Tiny::YAML::Constructor;
$Tiny::YAML::Constructor::VERSION = '0.0.1';
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

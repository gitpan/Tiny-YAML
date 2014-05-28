use Test::More tests => 1;
use YAML::XS;
use Tiny::YAML;

is YAML::XS::Dump(Tiny::YAML::Load "foo: bar\n"),
  "---\nfoo: bar\n",
  "Simple Hash Load Works";

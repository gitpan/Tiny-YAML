use Test::More;
use Tiny::YAML();

sub load_dump {
    my ($yaml, $want, $label) = @_;
    is Tiny::YAML::Dump(Tiny::YAML::Load($yaml)), $want, $label;
}

load_dump
  "foo: bar\n",
  "---\nfoo: bar\n",
  "Simple Hash Load Works";

load_dump
  "foo: bar",
  "---\nfoo: bar\n",
  "No trailing newline";

load_dump <<'...', <<'...', 'Simplest Indentation';
foo:
  bar: baz
...
---
foo:
  bar: baz
...

load_dump <<'...', <<'...', 'Indent / Undent';
foo:
  bar: 42
baz: 123
...
---
baz: '123'
foo:
  bar: '42'
...

load_dump <<'...', <<'...', 'Document Header';
---
foo: bar
...
---
foo: bar
...

load_dump <<'....', <<'...', 'Document Footer';
foo: bar
...
....
---
foo: bar
...

done_testing;

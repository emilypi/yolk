#!/bin/sh
set -eu
cd "$(dirname "$0")/.."

# Generated benchmark reports/data retain their canonical bytes. Everything
# else below is authored source, configuration, or a Markdown code fence.
git ls-files --cached --others --exclude-standard -z \
  '*.hs' '*.sh' '*.cabal' '*.yaml' '*.yml' 'cabal.project' '*.md' |
  xargs -0 perl -CSD -ne '
    use strict;
    use warnings;
    our ($file, $line_number, $fenced, $failed);
    if (!defined($file) || $ARGV ne $file) {
      $file = $ARGV;
      $line_number = 0;
      $fenced = 0;
    }
    ++$line_number;
    next if $file eq "bench/baseline.md";
    if ($file =~ /\.md\z/) {
      if (/^\s*```/) {
        $fenced = !$fenced;
        next;
      }
      next unless $fenced;
    }
    s/\r?\n\z//;
    my $columns = 0;
    for my $character (split //) {
      $columns += $character eq "\t" ? 8 - $columns % 8 : 1;
    }
    if ($columns > 90) {
      printf "%s:%d: %d columns\n", $file, $line_number, $columns;
      $failed = 1;
    }
    END { exit($failed || 0) }
  '

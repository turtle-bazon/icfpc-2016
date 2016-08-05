#!/usr/bin/perl

use strict;
use warnings;
use HTML::TreeBuilder;
foreach my $file_name (@ARGV) {
    my $tree = HTML::TreeBuilder->new;
    $tree->parse_file($file_name);
    for my $subtree ($tree->look_down(_tag => "div", class => "modal-body")) {
      for my $subsubtree ($subtree->look_down(_tag => "textarea")) {
        my $html = $subsubtree->as_text;
        $html =~ s/(?<!\n)\z/\n/;
        print $html;
      }
    }
    $tree = $tree->delete;
}

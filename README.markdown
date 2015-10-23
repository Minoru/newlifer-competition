Quick run-down of approaches implemented in each module.

Naive approach
==============

NOTE: it's not implemented, but is used as a basis for other versions.

Take a string, compare it with its reversed version. If it doesn't match, take
a tail of the reversed version and compare again. If it does match and is
longer than the current best, remember it. Repeat for as long as tail is
non-empty.

When empty tail is reached, drop the first character of the input string and
repeat the process again.

If there's no more characters to drop from the string, return current best.

Version1
========

This one actually didn't carry best solution around (I didn't think of that ☹);
instead, it runs itself on the tail of input string and then compares the
result to the one obtained on the full string, returning the longest of the
two. This uses up the call stack.

This version implements an optimization compared to naive version: instead of
checking all the tails of the reversed input, we only check the ones that start
with the same character as input string. To achieve that, we pre-compute a map
of all tails of reversed input, indexed by its first character.

Version2
========

This version implements an optimization compared to version1: it carries
current best solution around, and thus could be rewritten as tail-recursive
function.

Another, but strongly related, optimization allows the algorithm to cut off
branches that won't ever yield improvements over current best result. We do
that by comparing the length of tail obtained from the map with the length of
the current best.

Version3
========

This version is effectively version2, but uses ByteStrings internally.

Version4
========

The same as version3, but with Text instead of ByteStrings.

In version3, I went with ByteStrings instead of Text because the latter
promised worse O() on operations I used. But I lost the ability to process
Unicode strings correctly. Now I'm not sure I've done the right thing, so let's
just benchmark it!

Result: at the moment, it's even slower than Version1. It's probably just me,
though; I'll try to optimize it.

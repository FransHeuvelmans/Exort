# Exort - External CSV sort
This is a small program to sort CSV/TSV (etc.) files which are larger than memory. It does  an external merge of sorted parts. CSV's are parsed using [univocity-parsers][8].

It can handle sorting on multiple columns with a different ordering for each. And it can handle quoted text columns.

Some alternatives which might be better suited are:

* [xsv][2] (in-memory, but fast and easy)
* [(unix) sort][3] Fast and can be combined with *split*
* [miller][4] (very complete)
* Many smaller Python tools 
  (I have previously used an adapted version of [Melvilgit's external-Merge-sort][5] and [ShadenSmith's csvsorter][6] by including [Pandas'][7] csv parsing w. error reporting)

[1]: https://www.scala-lang.org
[2]: https://github.com/BurntSushi/xsv
[3]: http://www.man7.org/linux/man-pages/man1/sort.1.html
[4]: http://johnkerl.org/miller/doc/
[5]: https://github.com/melvilgit/external-Merge-Sort
[6]: https://github.com/ShadenSmith/csvsorter
[7]: https://pandas.pydata.org
[8]: https://github.com/uniVocity/univocity-parsers
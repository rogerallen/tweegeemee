# tweegeemee

A Clojure library designed to breed images via twitter.

See https://twitter.com/tweegeemee/media for the latest images.

So far, I'm using https://howistart.org/posts/clojure/1, http://www.karlsims.com/papers/siggraph91.html, https://github.com/nodename/clevolution, https://github.com/mikera/clisk, http://www.thattommyhall.com/2013/08/23/genetic-programming-in-clojure-with-zippers/ and http://blog.raynes.me/blog/2011/12/02/waving-our-tentacles/ for ideas, code and inspiration.

## To Do

* [x] create images
 * [x] show images
 * [x] allow for errors during creation
 * [x] check that image isn't just black
 * [x] save image as file
* [x] post images
 * [x] 140 characters is too constraining--use gists for code
 * [ ] too many files in a gist makes it unreadable.  Use 1 file & append?
 * [ ] too many repeats.  need to hash the creation string & small image & check for dupes
* [x] read twitter favorites & retweets
* [x] breed new images
* [x] fill out all the missing functions.
 * [x] limiting to fewer levels is helping perf
 * [ ] constrain the creation process to select and use only a few functions.  More choices are not making better images.
* [x] add mutation, too.
* [ ] show that we can create interesting children & mutants
* [ ] hook up to heroku
* [ ] profit! (ha)

## Usage

Not yet

## License

Copyright © 2015 Roger Allen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

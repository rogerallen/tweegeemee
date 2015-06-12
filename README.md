# tweegeemee (트의기미)

A Clojure library designed to breed images via twitter.  Now running on Heroku.

See https://twitter.com/tweegeemee/media for the latest images.

Follow, Favorite and Retweet [tweegeemee](https://twitter.com/tweegeemee)
images to select them for future generations.

Currently posting a batch of new images every 3 hours. The suffix at
the end of the image name tells you generation-type: a,b are random;
C,D are sexually reproduced and M,N are mutants with only one parent.

For reproduction, the last 60 tweets are considered. The top 5 are allowed to
produce offspring.

Oh, what is "트의기미"?  That is the Korean (Hangul) characters for
twee (트의) gee (기) mee (미).  I believe it is just a nonsense word
and liked the look of it.

Let's see where this goes...

## Background

A while back, I saw [Carin Meier's](https://github.com/gigasquid)
["How I Start" post](https://howistart.org/posts/clojure/1) about
running a Clojure-based twitter bot on Heroku. She showed how easy it
was to get this working, I was intrigued and decided I'd like to give
it a try--but with my own twist.

In the back of my mind, something I've always wanted to do was
experiment with image generation via lisp s-expressions as [Karl
Sims](http://www.karlsims.com) did in his ['91 Siggraph
paper](http://www.karlsims.com/papers/siggraph91.html).  But, how to
decide what constitutes a "good" image to breed?  Well, how about
using Twitter retweets and favorites for scoring?

I found the [Clisk clojure image library](https://github.com/mikera/clisk)
was perfect for recreating Sims' imagery.  I also took some inspriration from
https://github.com/nodename/clevolution, http://www.thattommyhall.com/2013/08/23/genetic-programming-in-clojure-with-zippers/ and http://blog.raynes.me/blog/2011/12/02/waving-our-tentacles/.

So far, this has been quite fun to work on.

## Usage

You should be able to clone/fork this library, follow the "How I Start"
post and get this running on Heroku.

If you just want to create & breed your own imagery, see the bottom of
core.clj for snippets to generate & show a random image, breed images
by hand and mutate images.

## License

Copyright © 2015 Roger Allen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

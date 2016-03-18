# tweegeemee (트의기미)

A Clojure library designed to breed images via twitter.  Now running on Heroku.

See https://twitter.com/tweegeemee/media for the latest images.

Each tweet has a name based on the timestamp of creation with a suffix
at the end, a link to a github gist and a hashtag (currently #ProceduralArt).

The link to a github gist highlights the code used to create the image.

Follow, Favorite and Retweet [tweegeemee](https://twitter.com/tweegeemee)
images to select them for future generations.

Currently posting a set of two new images every hour. The suffix at
the end of the image name tells you generation-type: a,b are random;
C,D are sexually reproduced and M,N are mutants with only one parent.

For reproduction, the last 200 tweets are considered. The top 5 are
allowed to produce offspring.

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

I found the [Clisk clojure image
library](https://github.com/mikera/clisk) was perfect for recreating
Sims' imagery.  I also took some inspriration from
https://github.com/nodename/clevolution,
http://www.thattommyhall.com/2013/08/23/genetic-programming-in-clojure-with-zippers/
and http://blog.raynes.me/blog/2011/12/02/waving-our-tentacles/.

So far, this has been quite fun to work on.

## Usage

You should be able to clone/fork this library, follow the "How I
Start" post and get this running on Heroku.  Note that as of version
1.3, due to Heroku free dynos now requiring 6 hours of sleep in a 24
hour period, I switched to using the Scheduler add-on.  See [Issue
11](https://github.com/rogerallen/tweegeemee/issues/11) for more
information.

If you just want to create & breed your own imagery, see the bottom of
core.clj for snippets to generate & show a random image, breed images
by hand and mutate images.

### Making your own images

Perhaps you'd like to reproduce one of the images posted?  Maybe, you
want a desktop background or something like that?

First, click that tweet's link to go to the github gist data structure.  For example,

```clj
 { :name "150613_061332_D.clj" :hash -504503564 :image-hash -1696445894
   :code (clisk.live/vdivide (clisk.live/vfrac (clisk.live/vmin (clisk.live/v+ (clisk.live/alpha clisk.live/grain) [-0.9438 0.4027 2.3753 1.7962]) (clisk.live/sigmoid (clisk.live/vfloor [0.4416 -2.6627 -1.6566])))) (clisk.live/gradient (clisk.live/square (clisk.live/v- [-0.2226 -2.2105 -2.7124 -1.7799] clisk.live/vsnoise))))
 }
```

Second, copy the info after the :code keyword to the end of the line.  e.g. `(clisk.live/vdivide ... )``

Next, load up a repl and paste the clipboard contents in to define a variable `code` after a (important!) single quote like so:

```clj
tweegeemee.core=> (def code '(clisk.live/vdivide (clisk.live/vfrac (clisk.live/vmin (clisk.live/v+ (clisk.live/alpha clisk.live/grain) [-0.9438 0.4027 2.3753 1.7962]) (clisk.live/sigmoid (clisk.live/vfloor [0.4416 -2.6627 -1.6566])))) (clisk.live/gradient (clisk.live/square (clisk.live/v- [-0.2226 -2.2105 -2.7124 -1.7799] clisk.live/vsnoise))))
 )

tweegeemee.core=> (show (eval code) :width 720 :height 720)
```

To save an image, do

```clj
(write-png "nice_one.png" (image (eval (:code d)) :width 720 :height 720))
```

## License

Copyright © 2015 Roger Allen

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

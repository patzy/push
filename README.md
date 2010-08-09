PUSH
====

This is my simple game for [ILGE2010](http://dto.github.com/notebook/2010expo.html).
You play some sort of particle accelerating through a corridor.
Your only ability is to teleport yourself ahead in time to avoid obstacles.

Gameplay
--------

Your goal is to go as far as possible to do the best score. The score is related to the effective
distance travelled. This means that only distance travelled when your *not* teleporting makes score.
Score also increase with your speed, i.e. the faster you go the more score you get for the same
distance.

There are 3 types of obstacles with different effects:
- warp: automatic teleportation (blue walls);
- slow: reduce your speed (green walls);
- containment: immediate death (red walls).

There's no game end except if you die. You can die if you touch a containment wall
or if your speed stays under 500 for more than 3 seconds.

Running
-------

You need some Common Lisp implementation (clisp, ccl or sbcl)
with [asdf](http://common-lisp.net/project/asdf/).

The game requires the following libs (and their respective dependencies):
- [glaw](http://github.com/patzy/glaw) and glaw-imago
- [glop](http://github.com/patzy/glop)

When you have all this just do:
    (asdf:operate 'asdf:load-op :push)
    (push:run)


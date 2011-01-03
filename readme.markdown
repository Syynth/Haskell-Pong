# **Pong**: yet another Haskell retrogame

This version of Pong is based, in large part, on Sven Panitz's tutorial on HOpenGL. Essentially,
I'm tearing it apart and putting it back together to see how it works.

That said, I've made a few improvements:

* Code is broken out into modules, making improvements to the code easier.
* All functions are explicitly typed, reducing mystery-meat type inference errors.
* Keyboard bindings are cleaned up. Keys move when pressed and stop when lifted. Different keys
are used for up and down directions.

And a few improvements have yet to be made:

* The paddles don't yet have collision logic, although it will be easy to layer that into the code
* Once the paddles work, I will remove the horizontal bounce
* Need to draw a dashed line down the middle
* Scores should be displayed.
* Ball should speed up after a few hits
* Maybe a little color, some sounds...


##To play:

The keys 'Q' and 'A' move the left paddle.
The keys 'O' and 'L' move the right paddle.
The idea is to keep the ball in play.

##Some ideas for the future:

* Create a fully 3D ball and paddles, give light, and maybe a little depth on the Z axis.
* Make the *game itself* 3D. Perhaps able to rotate? (Although that might render it a bit evil...)
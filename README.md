Monokex
=======

Monokex is a protocol framework inspired by [Noise][].

[Noise]: https://noiseprotocol.org/

It aims to generate specifications, test vectors, working C code, and
more, from Noise protocol descriptions such as this one:

    XX:
      -> e
      <- e, ee, s, es
      -> s, se

Try it out!
-----------

- Edit the `protocols.txt` file.
- Call `make` to generate the output files.
- Look at the results in the `gen/` folder

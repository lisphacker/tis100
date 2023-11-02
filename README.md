# tis100

This is a Haskell implementation of an emulator for the Tesselated Intelligence System processor defined in the [Zachtronics game TIS-100](http://www.zachtronics.com/tis-100/). It uses the assembly save format from the game and the layout and I/O specification defined in another [C-based open-source emulator implementation](https://github.com/Phlarx/tis).

The emulation is not quite complete since it does not yet support the ANY and LAST ports but everything else should work (🤞).

To run one of the examples:
```sh
stack run tissim -- examples/segment00150/segment00150.asm -c examples/segment00150/segment00150.cfg
```

To run tests:
```sh
stack test
```

Thanks to [Derek Anderson](https://github.com/Phlarx) for the layout and I/O format spec and [Clara Schaertl Short et al](https://github.com/kk4ead) for the [best practices document](https://kk4ead.github.io/tis-100/).

# Clw-Tetris - Sample tetris written in Common Lisp for web

## Usage

```lisp
> (ql:quickload :qlot)
> (qlot:quickload :clw-tetris)
> (clw-tetris:start :port <port number>)
```

After starting, you can access to the tetris by `http://localhost:<port number>` using a web browser.

## Installation

This project depends on liblaries that are not registered in the quicklisp repository. So I recommend to use [qlot](https://github.com/fukamachi/qlot) to install them.

```bash
# Under a directory managed by quicklisp
$ git clone https://github.com/eshamster/clw-tetris.git
$ cd clw-tetris
$ ros install qlot # if you haven't installed
$ qlot install
```

## Operation

You can move pieces by both keyboard and mouse.

### Move by Keyboard

| Key | Explanation |
| :-------: | :---------- |
| Left/Right/Down  | Move |
| C | Rotate to right |
| Z | Rotate to left  |

<img alt="Move by Keyboard" src="https://raw.githubusercontent.com/wiki/eshamster/clw-tetris/images/summary.gif" width="500px">

### Move by Mouse

| Operation | Explanation |
| :-------: | :---------- |
| Left click  | Warp to the point |
| Right click | Move to down |
| Wheel down | Rotate to right |
| Wheel up   | Rotate to left  |

<img alt="Move by Mouse" src="https://raw.githubusercontent.com/wiki/eshamster/clw-tetris/images/move_by_mouse.gif" width="500px">

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2017 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License.

<h1> imageCompressor </h1>

Compress your images using this program !

## Requierements

Youll have to install the following:
1. stack
2. make

## How to build ?

Build using make:
```sh
make
```

## How to run

1. Convert your image into positions and colors
```
(0,0) (255,0,0)
(0,1) (254,3,2)
...
```
2. Use the software `imageCompressor` as the following
```sh
./imageCompressor -n [colors] -l [convergence] -f [file]
```
- colors: the number of colors you want
- congergence: how the program will know how to stop (mostly at 0.8)
- file: the file with positions & colors

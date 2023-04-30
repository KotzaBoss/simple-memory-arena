To build:
```shell
cmake -B build && make -C build -j$(nproc)
```
The interesting parts of the build directory are:
```
build
├── ada
│   ├── main
└── cpp
    ├── bench.1e0.out
    ├── bench.1e1.out
    ├── bench.1e2.out
    ├── bench.1e3.out
    ├── bench.1e4.out
    ├── bench.1e5.out
    ├── main.1e0.out
    ├── main.1e1.out
    ├── main.1e2.out
    ├── main.1e3.out
    ├── main.1e4.out
    └── main.1e5.out
```
The `cpp` executables can be run without any arguments, however the ada executable expects 1 `Positive` argument, `./build/ada/main 1234`.

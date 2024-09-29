# Benchmark

## Backends

### Node.js

Build with:

```sh
spago bundle -p bench --platform node --bundle-type app --minify
```

Run with:

```sh
time node --max-old-space-size=16384 ./bench/index.js -w 39 -h 32 -n 10000 -s 7
```

### Chez Scheme

The following root `spago.yaml` have to be used:

```yaml
package:
  name: points
  dependencies:
    - field
workspace:
  backend:
    cmd: "purescm"
    args:
      - "build"
  packageSet:
    url: https://raw.githubusercontent.com/purescm/purescm/731234487b3451190dbccc68237a1afb92ddb034/package-sets/1.0.0.json
  extraPackages:
    argparse-basic: 2.0.0
    strings:
      git: "https://github.com/purescm/purescript-core.git"
      ref: "ca099b896e24f5f8875a16dff8e1b333c2475da8"
      subdir: "strings"
```

Also `test` section from `field/spago.yaml` should be removed/commented out.

Build with:

```sh
spago build -p bench
purescm bundle-app
```

Run with:

```sh
scheme --program output/main - -w 39 -h 32 -n 5 -s 7
```

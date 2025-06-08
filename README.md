# Conflux

A dynamic layout generator for the River Wayland compositor, written in Haskell.

## Description

Conflux is a layout generator that provides dynamic tiling functionality for the River Wayland compositor. It manages window layouts with configurable parameters. Currently it replicates the behaviour of rivertile, but adding more layouts is quite easy and might be added in the future.

## Usage

```bash
conflux [OPTIONS]
```

### Options

- `-n, --namespace NAMESPACE`  
  Namespace for the layout (default: "conflux")

- `-r, --main-ratio RATIO`  
  Main area ratio (default: 0.6)

- `-c, --main-count COUNT`  
  Number of windows in main area (default: 1)

- `-v, --view-padding PADDING`  
  Padding between views (default: 2)

- `-o, --outer-padding PADDING`  
  Outer padding (default: 2)

- `-l, --layout LAYOUT`  
  Initial layout (default: "rStack")

the options can be changed at runtime using `riverctl send-layout-cmd`


### Example

```bash
conflux --main-ratio 0.7 --main-count 2 --view-padding 4
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.


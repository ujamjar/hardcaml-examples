# HardCaml Examples

Module  | Description 
:-------|:-----------
Mul     | ASIC style wallace and dadda tree multipliers
Prefix  | Parallel prefix networks
Sorting | Bitonic and Odd-Even merge sorting networks
Lfsr    | Linear feedback shift registers
Rac     | Rom-accumulator
Cordic  | Trigonometry in hardware

### About

Each example design is compiled into an executable that can generate
a VHDL or Verilog netlist, or run a simulation testbench.

The designs are also compiled into a library called 'hardcaml-examples' 
which can be explored in the OCaml toplevel.

Each core exposes various configuration options and can generate
quite different architectures.  

Generally, the core's themselves are quite simple.  Hopefully they provide
a reasonable way to understand HardCaml design.

Even so, the configuration of the cores can get quite complex (esp. Cordic
for example).  That's because a little code can do quite a lot!

### TODO

* Javascript framework implementation


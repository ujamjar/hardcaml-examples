# HardCaml Framework and Examples

A framework for building hardware core generators as a console
application or web page along with 6 example cores of varying
complexity.

Module  | Description 
:-------|:-----------
Mul     | ASIC style wallace and dadda tree multipliers
Prefix  | Parallel prefix networks
Sorting | Bitonic and Odd-Even merge sorting networks
Lfsr    | Linear feedback shift registers
Rac     | Rom-accumulator
Cordic  | Trigonometry in hardware

### About the framework

A core is written against the `HardCamlFramework.Framework.Design` 
signature.  It will provide the following functionality.

* Basic information about the core
* Core and testbench parameters (ie bit widths, number of cycles etc)
* Parameter validation
* A core generation function
* A testbench function

The console and javascript framework backends will use the HardCaml API to
implement various simulation and code generation options.

### About the console examples

The example designs are compiled into a library called 'hardcaml-examples' 
which can be explored in the OCaml toplevel.

Each example is also compiled into a native code console application.

A top level console program is just the application of an example to the framework
console backend ie

```
module A = HardCamlFrameworkConsole.App.Make(HardCamlExamples.Sorting.Design)
```

which should be linked with the `hardcaml-framework.console` and `hardcaml-examples`
ocamlfind libraries.

### About the web apps

A webapp is made up of two byte code executables and a html page.  The executables 
are compiled into javascript with `js_of_ocaml`.

Two executables are required as one will be run on the main html page to control the
user interface and the other is run as a webworker and performs circuit generation,
simulation etc.

The main application looks like

```
module A = HardCamlFrameworkJS.Appmain.Make(HardCamlExamples.Sorting.Design)
```

while the webworker application is just

```
module A = HardCamlFrameworkJS.Appww.Make(HardCamlExamples.Sorting.Design)
```

Both should be linked with the ocamlfind libraries `hardcaml-framework.js`, 
`hardcaml-examples` and `js_of_ocaml`.

The userinterface attaches itself to a div in the html page called 
`hardcaml-framework-webapp`.  The div should also have a data attribute called
`data-hcww` which provides the name of the webworker javascript file ie

```
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
        <title>HardCamlFramework Sorting WebApp</title>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="http://yui.yahooapis.com/pure/0.6.0/pure-min.css">
    </head>
    <body>
        <!-- The following 2 lines enable the webapp -->
        <div data-hcww="hcwwsort.js" id="hardcaml-framework-webapp"></div>
        <script type="text/javascript" src="hcjssort.js"></script>
    </body>
</html>
```


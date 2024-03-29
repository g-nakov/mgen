## What is MGen
`mgen` is a code generator for reading and validating input data based on type declarations. `mgen` reads a simple declarative description of the input data, then generates code in a chosen language (currently we support Matlab and Python) and performs code transformations and optimisations so that the generated code looks similar to code that one would write by hand.

## Motivation
Many metrology software packages come with careful descriptions of input formats in their documentation, usually describing what input is required (e.g., `thermal conductivities`), in what form (e.g., `an array with an entry for each layer`), and in what unit (e.g., $Wm^{-2}K^{-1}$). However these are written for humans, not machines, and consequently the code to read the inputs and convert them to the internal units used, if applicable, is also written by humans. This is typically fiddly code, with perhaps nested loops, and many opportunities for off-by-one errors to slip in.

Our approach is instead to make the description of the input format formal, so that it can be understood by a machine, which can then write the code for reading the inputs.

## Installation

Installation requires [Haskell](https://www.haskell.org) and the `cabal` tool, which can be installed using [GHCup](https://www.haskell.org/ghcup/) if required.

After cloning this repository, install the software by executing
```shell
cabal install
```
in the cloned directory. This installs a binary called `mgen`.

## Input data format
An input is declared with its name, followed by a colon
`:`, followed by its optional human-readable description.

An input is either:
* **A scalar field**. The field must be tagged with its type.

  The tag starts with `@` followed by `int` or `number` for integral fields. For example, the following declares an integral field with name `nlayer`:
  ```yaml
  nlayer : contains the @number of layers (2 or 3) in the sample
  ```
  Alternatively, the field might have a SI unit attached to it instead. The `@` symbol is then followed by the name of the unit. Common prefixes and derived SI units are recognised. For example:
  ```yaml
  tflash : duration of laser flash in @ms
  rloss : heat transfer coefficient for losses in  @W m^-2 K^-1
  ```
* **An array**. The array might be given as an explicit enumeration of its elements. Each element consists of a textual description and a type tag. The supported tags are the same as for scalar fields.
  ```yaml
  arrayExample :
    - one @number
    - two in @mm
    - three in @km
  ```
  If all the elements are of the same type, the tag might be omitted and specified only once when declaring the name of the array.
  ```yaml
  anotherArrayExample : of type @mm
    - one
    - two
    - three
  ```
  However, there might be cases where array literals are not a suitable solution --- for instance, when we do not know the number of elements beforehand. In such situations, we can use field-indexed arrays, which store as many elements as some integral value stored in a previously defined scalar field. For example:
  ```yaml
  dim : @Int
  ...
  vec : @dim many elements in unit @mm
  ```
  Rather than supplying a single type tag, we can give a comma separated list of tags.
  ```yaml
  length  : @Number
  array  : @length many elements of @mm, @km, @s
  ```
  The first of `length` many elements will be associated with the first tag `@mm`, the second with the second tag `@km`, and so on. If there are more elements than tags, the remaining elements will inherit the last tag, i.e `@s` in our example. The generated Python code implementing this behaviour looks like this:
  ```python
  src.length = int(src0[readPtr])
  if  0 <= src.length:
      src.array[0] = 1e-3 * float(src0[readPtr + 0])
  if  1 <= src.length:
      src.array[1] = 1e3 * float(src0[readPtr + 1])
  for i in range(2, src.length):
      src.array[i] = float(src0[readPtr + i])
  ```

* **A composite object** - composite objects are structures grouping arrays or scalar fields, or nesting other composite objects. For example:
  ```yaml
  group :
    field1 : @int
    nested_group :
      field2 : @km
  ```

## Running `mgen`

To generate code from an input description in a file `desc.mgen`, run the following command:
```shell
mgen desc.mgen
```
The generated code is printed in the terminal. Hence to save it to a file, the output can be redirected:
```shell
mgen desc.mgen > readDesc.m
```

By default, Matlab code is generated, but the output language can be changed to Python with the `--lang` option:
```shell
mgen --lang python desc.mgen
```


## Examples
See full examples of input data specifications and the generated code in [examples](examples/). A detailed walkthrough of the Python code generation is also available as a Jupyter [notebook](examples/common.ipynb).

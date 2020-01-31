This is a WIP. Spec is mostly done but will likely be refined.

# Potato Forest Tech Tree
Potato Tech Tree is a flexible and simple game design tool for generating quantified tech trees. It is intended for both players and designers to better understand their game's progress tree. This tool has 3 components:

- Markup Language
- Visualizer
- Solver

## Markup Language
A Potato Tech Tree definition is built from items and recipes. Items represent quantifiable things and recipes represent operations for creating items. Items are intended to represent many things. For example, they could represent a piece of research enabling new actions or be a logical intermediary of a more complex recipe.

Potato Tech Tree Builder uses a simplified human writeable file format that is documented here.

### Expressions
Expressions are ways to refer to quantified items. Item expressions are simply a quantity followed by the unique id of the item. For example `52 cards`. If no quantity is specified, it defaults to 1

A list of expressions is simply a list of expressions separate by a new line, for example:

```
queen
king
2 rooks
2 bishops
2 knights
8 pawns
```

This document also outlines a more powerful language for expressions to be implemented in a future version (see future section below).

### Recipes
Recipes are defined with the following fields

```
RECIPE <unique id>
REQUIRES
<list of expressions>
INPUTS
<list of expressions>
OUTPUTS
<list of expressions>
```

`INPUTS` is a list that designates the input requirements of the recipe

`OUTPUTS` is a list that designates the output of the recipe

`REQUIRES` designate items that are required but not consumed by the recipe. For example, "metallurgy" might be a requirement for the "iron bar" recipe. If an item is exclusively required by the recipe (so that it can't be used in another recipe as either an input or requirement at the same time) it can prefixed with the `exclusive` keyword, for example:

```
REQUIRES
exclusive 2 monitors
exclusive person
INPUTS
999999999 time
OUTPUTS
web_application
```

### Items
It's necessary to define all items used by recipes. Each item is defined with the following fields:

```
ITEM <unique id>
TITLE <string>
DESC <string>
```

`ITEM` is the internal name of the item. It must not collide with any other item name or keyword. `TITLE` is a human readable name for the item used for rendering.
`DESC` is an optional textual description of the item.

If there is a maximum limit to the number of items that can exist, you can use the optional `LIMIT` parameter:

```
LIMIT <integer>
```

Optionally, recipes can be added to items with the following two fields:

```
REQUIRES
<list of expressions>
INPUTS
<list of expressions>
QUANTITY
<integer>
```

This is syntactic sugar for a recipe that outputs the designated quantity of the defined item. If the quantity field is taken to be 1 by default.

Finally, item definitions allow for the following optional graph generator directives:

```
TIER <0-99>
ICON <thumbnail.png>
FULL <image.png>
PHANTOM <omit/pass/false>
```

These are described in detail in the graph generation section.

### Starting Items
Items available at start are specified as follows

```
STARTING
<list of items>
```

### Time
The only built-in item is `time` which represents in-game elapsed time. Time is like an item with infinite starting quantity. It is treated uniquely by the system in the following ways:

1. Time can not be listed in an `OUTPUT` field.

2. Time can not be flagged as exclusive. Instead, if a set of recipes can not be executed at the same time, they should all require a unique token that is included as a starting item. For example:

  ```
  ITEM research_token
  LIMIT 1
  ...

  STARTING
  research_token
  ...

  REQUIRES
  exclusive research_token
  1000 time
  ```

  In this case, a single starting `research_token` means only 1 recipe requiring `research token` can be executed at once.

3. <TODO DELETE THIS AND DISALLOW TIME IN INPUT FIELD???> By putting time in the `INPUT` field means that no other recipe requiring or using time can be executed at the same time. It is like having a global `token` item that is included as an exclusive requirement for every recipe.

## Graph Generator
The graph generator generates a web-based graph visual of the defined item tree indicating which items are needed for which items. Hints can be added to the definition file to help structure the visual better.

### Icons and Images
Definitions include directives for the graph generator. Icons and full images for an item can be specified with the optional `ICON` and `FULL` image fields.

### Tier
The graph generator attempts to render items in rows where each row is "tier". By default, starting items that are not outputs of some recipe and time are tier `0` items and a tier `n` item is made from only tier `m` items where `m < n` where `n` is minimal. The `TIER` field can force items to be in a different tier. Doing so will force all items in tiers above it to be higher. All items in tiers below it will be forced into lower tiers. In this case, a tier `n` item is made from only tier `m` items where `m < n` OR `m = n` if there is a tier `m+1` item that depends on it. Manually set tiers must not break this invariant. If there are multiple recipes that define different tiers for an item, the minimum is used.

For example, by default, we have:

```
items: A -> B -> C -> D -> E
        \_> B'
tier:  1    2    3    4    5  
```
Forcing `C` to tier 2 pushes `B` to tier 1, however `B'` stays in tier 2 since it does not have a tier 2 item that depends on it

```
items: A -> B -> C -> D -> E
        \______> B'
tier:  1    1    2    3    4  
```

When rendered, each tier is rendered at a different vertical level denoted by a different color. If there are dependencies within the same tier, they will be offset into visual subtiers. 


<TODO PHANTOM types, probably needs restrictions>
If `PHANTOM` is set to `omit` or `pass`, the item will not show up in the renderer. Recipes that use phantom items will simply omit all connections to items sets to `omit` or inherit its recipe ingredients if it is set to `pass`.

### Total Requirements
<TODO on hover, show total items required using simple solver or something>

## Potato Solver
The Potato Solver generates heuristics of the following forms:

1. What is the "best" way for me to produce item X
  - where "best" means least total usage of a set of resources which may include time

## Examples
[Starcraft 1 Zerg Tech Tree](examples/sc1.spec)
<TODO Merge Dragons D:>
<TODO call for contributors>

## FUTURE

### Tags (delete)
Items can be grouped with tags and these tags can be referred to in recipes. For example, there may be many types of wood all tagged "wood", and any wood tagged item can be used in a recipe calling for wood
<TODO quantity to tags?>
<TODO is it better just to have intermediate recipes instead of tags? e.g. oak -> wood, pine -> wood, cedar -> wood, wood -> coal, probably yeah? Note that we need better render directives in this case as you don't want to render every type of wood>

### More Render Directives
<TODO>

### Variable Expressions
Recipes can use variables to help encode multiple recipes. Variables are defined in between two % signs and matched through the recipe. Note that this requires similar materials to have the same name prefix so that only those items are matched. For example

```
RECIPE
REQUIRES
wood%a%
metal%b%
OUTPUTS
hatchet%a%%b%
```

### Random Output Expressions
For example `4-6 rocks` as an output would randomly create 4, 5 or 6 (uniformly distributed) rocks. Perhaps a more complicated syntax for non-uniform or conditional distributions might be appropriate.

### Formulaic Expressions
Expressions should be able to access existing properties and use them in formulas. This would allow for things like upgrades with exponentially increasing cost.

### Expression Macros
include macros for quantified items for examples

`1 s = 1000 ms`

This is distinct from creating a new items because they are not shown by the visualizer

### Solver Interface
The first version only contains a basic solver using A* algorithm. Instead, there should be a solver interface allowing other solvers to be plugged in as appropriate.

# TODO
Item flags:
- hidden items: e.g. research_token type stuff that shouldn't be displayed by visualizer
  - prob needs restrictions on inputs/outputs so that visualizer doesn't break.
- exclusive groups: e.g. exclusive_1 exclusive_2...
  - <TODO explain how this works?>
- allow or modifier inside of REQUIRE/INPUT recipes (instead of specifying multiple recipes)
- find a way to fix the zerg larva problem: some units are locked per instance
  - with variable expressions, you can make hatchery_%x% and larva_%x% which does solve this problem but really messes up the visualizer

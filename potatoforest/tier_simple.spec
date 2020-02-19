# basic example
ITEM teddy_bear
STARTING 1
ITEM apple
INPUTS teddy_bear
ITEM milk
INPUTS apple
ITEM shake
INPUTS milk
ITEM blender
INPUTS shake

# same as basic example, except will be processed in reverse order (items are processed in alphabetical order)
ITEM a
INPUTS b
ITEM b
INPUTS c
ITEM c

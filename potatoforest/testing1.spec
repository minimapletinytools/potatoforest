ITEM teddy_bear
TITLE a teddy bear
DESC a very soft and cuddly bear made of natural cotton
TIER 5
ICON tb.png
FULL tb_full.png
TAG stuffed_animal bear cute
PHANTOM pass
LIMIT 1
STARTING 1

ITEM apple

ITEM milk
REQUIRES apple
ITEM shake
ITEM blender

ITEM vegan_milkshake
REQUIRES
  blender
INPUTS
  milk
  shake
QUANTITY 2

RECIPE blenders_are_made_of_milk_and_money
REQUIRES
  exclusive blender
  milk
INPUTS
  coin
OUTPUTS
  blender


ITEM coin
LIMIT 10

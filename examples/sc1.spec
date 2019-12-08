# Example spec for SC1 Zerg
# some simplifications made such as larva and egg stages being omitted
# includes meta recipes and items to simulate economy (in absence of opponent)

STARTING
4 drones
1 hatchery
50 minerals
# economy meta items
50 mineral_patch
12 gas_patch

ITEM hatchery
TAG building tier1
INPUTS
drone
300 minerals
1800 time

ITEM extractor
TAG building tier1
INPUTS
drone
25 minerals
600 time
gas_patch_occupied

ITEM creep_colony
TAG building tier1
INPUTS
drone
300 time

ITEM spawning_pool
TAG building tier1
REQUIRES
hatchery
INPUTS
drone
1200 time

ITEM evolution_chamber
TAG building tier1
REQUIRES
hatchery
INPUTS
drone
600 time

ITEM hydralisk_den
TAG building tier1
REQUIRES
spawning_pool
INPUTS
drone
600 time

ITEM sunken_colony
TAG building tier1
REQUIRES
spawning_pool
INPUTS
creep_colony
300 time

ITEM spore_colony
TAG building tier1
REQUIRES
evolution_chamber
INPUTS
creep_colony
300 time

ITEM lair
TAG building tier2
INPUTS
hatchery
1500 time

ITEM queens_nest
TAG building tier2
INPUTS
drone
900 time

ITEM spire
TAG building tier2
INPUTS
drone
1800 time

ITEM infested_command_center
TAG building tier2
REQUIRES
queen
enemy_command_center

ITEM hive
TAG building tier3
REQUIRES
queens_nest
INPUTS
lair
1800 time

ITEM ultralisk_cavern
TAG building tier3
REQUIRES
hive
INPUTS
drone
1200 time

ITEM greater_spire
TAG building tier3
REQUIRES
hive
INPUTS
spire
1800 time

ITEM nydus_canal
TAG building tier3
REQUIRES
hive
INPUTS
drone
600 time

ITEM nydus_canal_complete
TAG building tier3
INPUTS
nydus_canal
QUANTITY 2

ITEM defiler_mound
TAG building tier3
REQUIRES
hive
INPUTS
drone
900 time

# UNITS

ITEM overlord
TAG unit tier1
INPUTS
100 minerals
600 time

ITEM drone
TAG unit tier1
INPUTS
50 minerals
300 time

ITEM zergling
TAG unit tier1
REQUIRES
spawning_pool
INPUTS
50 minerals
420 time
QUANTITY 2

ITEM hydralisk
TAG unit tier1
REQUIRES
hydralisk_den
INPUTS
75 minerals
25 gas
420 time

ITEM mutalisk
TAG unit tier2
REQUIRES
spire
INPUTS
100 minerals
100 gas
600 time

ITEM scourge
TAG unit tier2
REQUIRES
spire
INPUTS
25 minerals
75 gas
450 time
QUANTITY 2

ITEM queen
TAG unit tier2
REQUIRES
queens_nest
INPUTS
100 minerals
100 gas
750 time

ITEM infested_terran
TAG unit tier2
REQUIRES
infested_command_center
INPUTS
100 minerals
50 gas
600 time

ITEM ultralisk
TAG unit tier3
REQUIRES
ultralisk_cavern
INPUTS
200 minerals
200 gas
900 time

ITEM guardian
TAG unit tier3
REQUIRES
greater_spire
INPUTS
mutalisk
50 minerals
100 gas
600 time

ITEM defiler
TAG unit tier3
REQUIRES
defiler_mound
INPUTS
50 minerals
150 gas
750 time


# economy meta recipes

RECIPE hatchery_minerals
INPUTS
drone
300 minerals
1800 time
8 mineral_patch
2 gas_patch
OUTPUTS
hatchery
8 mineral_patch_occupied
2 gas_patch_occupied


RECIPE mineral_extraction
REQUIRES
exclusive mineral_patch_occupied
INPUTS
drone
100 time
OUTPUTS
drone
8 minerals

RECIPE gas_extraction
REQUIRES
exclusive extractor
INPUTS
drone
50 time
OUTPUTS
drone
8 gas

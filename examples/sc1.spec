# Example spec for SC1 Zerg
# some simplifications made such as larva and egg stages being omitted
# includes meta recipes and items to simulate economy (in absence of opponent)

# STARTING ITEMS
# 4 drones
# 50 minerals
# 1 hatchery
# 8 mineral_patch_occupied
# 2 gas_patch_occupied
# 50 mineral_patch
# 12 gas_patch

ITEM hatchery
TIER 1
TAG building
STARTING 1
REQUIRES
1800 time
INPUTS
drone
300 minerals

ITEM extractor
TIER 1
TAG building
REQUIRES
600 time
INPUTS
drone
25 minerals
gas_patch_occupied


ITEM creep_colony
TIER 1
TAG building
REQUIRES
300 time
INPUTS
drone

ITEM spawning_pool
TIER 1
TAG building
REQUIRES
hatchery
1200 time
INPUTS
drone


ITEM evolution_chamber
TIER 1
TAG building
REQUIRES
hatchery
600 time
INPUTS
drone

ITEM hydralisk_den
TIER 1
TAG building
REQUIRES
spawning_pool
600 time
INPUTS
drone

ITEM sunken_colony
TIER 1
TAG building
REQUIRES
spawning_pool
300 time
INPUTS
creep_colony

ITEM spore_colony
TIER 1
TAG building
REQUIRES
evolution_chamber
300 time
INPUTS
creep_colony


ITEM lair
TIER 2
TAG building
REQUIRES
1500 time
INPUTS
hatchery

ITEM queens_nest
TIER 2
TAG building
REQUIRES
900 time
INPUTS
drone

ITEM spire
TIER 2
TAG building
REQUIRES
1800 time
INPUTS
drone

ITEM infested_command_center
TIER 2
TAG building
REQUIRES
queen
enemy_command_center

ITEM hive
TIER 3
TAG building
REQUIRES
queens_nest
1800 time
INPUTS
lair

ITEM ultralisk_cavern
TIER 3
TAG building
REQUIRES
hive
1200 time
INPUTS
drone

ITEM greater_spire
TIER 3
TAG building
REQUIRES
hive
1800 time
INPUTS
spire

ITEM nydus_canal
TIER 3
TAG building
REQUIRES
hive
600 time
INPUTS
drone

ITEM nydus_canal_complete
TIER 3
TAG building
INPUTS
nydus_canal
QUANTITY 2

ITEM defiler_mound
TIER 3
TAG building
REQUIRES
hive
900 time
INPUTS
drone

# UNITS

ITEM overlord
TIER 1
TAG unit
STARTING 1
REQUIRES
600 time
INPUTS
100 minerals


ITEM drone
TIER 1
TAG unit
STARTING 1
REQUIRES
300 time
INPUTS
50 minerals

ITEM zergling
TIER 1
TAG unit
REQUIRES
spawning_pool
420 time
INPUTS
50 minerals
QUANTITY 2

ITEM hydralisk
TIER 1
TAG unit
REQUIRES
hydralisk_den
420 time
INPUTS
75 minerals
25 gas

ITEM mutalisk
TIER 2
TAG unit
REQUIRES
spire
600 time
INPUTS
100 minerals
100 gas

ITEM scourge
TIER 2
TAG unit
REQUIRES
spire
450 time
INPUTS
25 minerals
75 gas
QUANTITY 2

ITEM queen
TIER 2
TAG unit
REQUIRES
queens_nest
750 time
INPUTS
100 minerals
100 gas

ITEM infested_terran
TIER 2
TAG unit
REQUIRES
infested_command_center
600 time
INPUTS
100 minerals
50 gas

ITEM ultralisk
TIER 3
TAG unit
REQUIRES
ultralisk_cavern
900 time
INPUTS
200 minerals
200 gas

ITEM guardian
TIER 3
TAG unit
REQUIRES
greater_spire
600 time
INPUTS
mutalisk
50 minerals
100 gas

ITEM defiler
TIER 3
TAG unit
REQUIRES
defiler_mound
750 time
INPUTS
50 minerals
150 gas


# economy meta items
ITEM gas
ITEM minerals
STARTING 50
ITEM mineral_patch
STARTING 50
ITEM mineral_patch_occupied
STARTING 8
ITEM gas_patch
STARTING 12
# occupied means there is a hatchery nearby
ITEM gas_patch_occupied
STARTING 2

# economy meta recipes
RECIPE hatchery_minerals
REQUIRES
1800 time
INPUTS
drone
300 minerals
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

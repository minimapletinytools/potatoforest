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
TAG building tier1
STARTING 1
REQUIRES
1800 time
INPUTS
drone
300 minerals

ITEM extractor
TAG building tier1
REQUIRES
600 time
INPUTS
drone
25 minerals
gas_patch_occupied


ITEM creep_colony
TAG building tier1
REQUIRES
300 time
INPUTS
drone

ITEM spawning_pool
TAG building tier1
REQUIRES
hatchery
1200 time
INPUTS
drone


ITEM evolution_chamber
TAG building tier1
REQUIRES
hatchery
600 time
INPUTS
drone

ITEM hydralisk_den
TAG building tier1
REQUIRES
spawning_pool
600 time
INPUTS
drone

ITEM sunken_colony
TAG building tier1
REQUIRES
spawning_pool
300 time
INPUTS
creep_colony

ITEM spore_colony
TAG building tier1
REQUIRES
evolution_chamber
300 time
INPUTS
creep_colony


ITEM lair
TAG building tier2
REQUIRES
1500 time
INPUTS
hatchery

ITEM queens_nest
TAG building tier2
REQUIRES
900 time
INPUTS
drone

ITEM spire
TAG building tier2
REQUIRES
1800 time
INPUTS
drone

ITEM infested_command_center
TAG building tier2
REQUIRES
queen
enemy_command_center

ITEM hive
TAG building tier3
REQUIRES
queens_nest
1800 time
INPUTS
lair

ITEM ultralisk_cavern
TAG building tier3
REQUIRES
hive
1200 time
INPUTS
drone

ITEM greater_spire
TAG building tier3
REQUIRES
hive
1800 time
INPUTS
spire

ITEM nydus_canal
TAG building tier3
REQUIRES
hive
600 time
INPUTS
drone

ITEM nydus_canal_complete
TAG building tier3
INPUTS
nydus_canal
QUANTITY 2

ITEM defiler_mound
TAG building tier3
REQUIRES
hive
900 time
INPUTS
drone

# UNITS

ITEM overlord
TAG unit tier1
STARTING 1
REQUIRES
600 time
INPUTS
100 minerals


ITEM drone
TAG unit tier1
STARTING 1
REQUIRES
300 time
INPUTS
50 minerals

ITEM zergling
TAG unit tier1
REQUIRES
spawning_pool
420 time
INPUTS
50 minerals
QUANTITY 2

ITEM hydralisk
TAG unit tier1
REQUIRES
hydralisk_den
420 time
INPUTS
75 minerals
25 gas

ITEM mutalisk
TAG unit tier2
REQUIRES
spire
600 time
INPUTS
100 minerals
100 gas

ITEM scourge
TAG unit tier2
REQUIRES
spire
450 time
INPUTS
25 minerals
75 gas
QUANTITY 2

ITEM queen
TAG unit tier2
REQUIRES
queens_nest
750 time
INPUTS
100 minerals
100 gas

ITEM infested_terran
TAG unit tier2
REQUIRES
infested_command_center
600 time
INPUTS
100 minerals
50 gas

ITEM ultralisk
TAG unit tier3
REQUIRES
ultralisk_cavern
900 time
INPUTS
200 minerals
200 gas

ITEM guardian
TAG unit tier3
REQUIRES
greater_spire
600 time
INPUTS
mutalisk
50 minerals
100 gas

ITEM defiler
TAG unit tier3
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

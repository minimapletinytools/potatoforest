# track 1
# a <- b <- c1 <- c2 <- c3
#           └-----------^
ITEM t1_a
STARTING 1

ITEM t1_b
INPUTS t1_a

# simple 3 item loop, should be tier 2
ITEM t1_c1
INPUTS t1_b t1_c3

ITEM t1_c2
INPUTS t1_c1

ITEM t1_c3
INPUTS t1_c2

# track 2
# time <-----------┐
# a <- b1 <- b2 <- d
#      └-----^     |
# c1 <- c2 <-------┘
# └-----^
ITEM t2_a

# simple 2 item cycle, should be tier 2
ITEM t2_b1
INPUTS t2_a t2_b2

ITEM t2_b2
INPUTS t2_b1

# 2 item cycle, should be tier 0
ITEM t2_c1
INPUTS t2_c2

ITEM t2_c2
INPUTS t2_c1

ITEM t2_d
INPUTS
t2_b2 t2_c1 time

# track 3, 1 item cycle, should be tier 1
# a <┐
# ^--┘
ITEM t3_a
INPUTS t3_a

# track 4 backtrack
# a -┐
# ↓  |
# b  |
# ↓  |
# c  |
# ↓  ↓
# d1 d2
# ↓  |
# e ←┘

ITEM t4_a
INPUTS t4_b t4_d2
ITEM t4_b
INPUTS t4_c
ITEM t4_c
INPUTS t4_d1
ITEM t4_d1
INPUTS t4_e
ITEM t4_d2
INPUTS t4_e

# track 5 double loop
#         ┌---------v
#         |  ┌ d3 ← d2
# a ← b ← c ←┤
#         |  └ e3 ← e2
#         └---------^

ITEM t5_a
ITEM t5_b
INPUTS t5_a
ITEM t5_c1
INPUTS t5_d2 t5_e2
ITEM t5_d2
INPUTS t5_d3
ITEM t5_d3
INPUTS t5_c
ITEM t5_e2
INPUTS t5_e3
ITEM t5_e3
INPUTS t5_c

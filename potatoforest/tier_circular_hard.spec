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
INPUTS t2_b2 t2_c1 time

# track 3
1 item cycle, should be tier 1
ITEM t3_a
INPUTS t3_a

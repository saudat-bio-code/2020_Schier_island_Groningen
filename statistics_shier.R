#binomial test of sweeping/trapping versus expected value.
binom.test(167,251,0.5) #trapping
binom.test(118,288,0.5) #sweeping


#Testing between two methods
male = c(167, 118)
female = c(84, 170)
complete_frame = data.frame(male, female)
fisher.test(complete_frame)

#Testing between night and day with the same fisher test.
male_sw = c(34, 84)
female_sw = c(36,134)
complete_frame_sw = data.frame(male_sw, female_sw)
fisher.test(complete_frame_sw)

#Fisher test for difference between the sites A and B.
male_sites = c(181,146)
female_sites = c(104,108)
complete_frame_sites = data.frame(male_sites, female_sites)
fisher.test(complete_frame_sites)

#comparing with literature:
binom.test( 167,251, 0.89)
binom.test(118,288,0.89)
binom.test(285,539,0.89) #together

male = c(1561, 167)
female = c(177, 84)
complete_frame = data.frame(male, female)
fisher.test(complete_frame)

male = c(1561, 285)
female = c(177, 254)
complete_frame = data.frame(male, female)
fisher.test(complete_frame)

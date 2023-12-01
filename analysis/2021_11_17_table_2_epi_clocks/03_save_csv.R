
# save outputs

write.csv(epi_clocks, here("outputs/epigenetic_clocks_table.csv"), row.names=F)
write.csv(cell_type_proportions, here("outputs/cell_type_proportions.csv"), row.names=F)

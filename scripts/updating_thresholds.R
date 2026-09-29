# Load current sysdata
load("R/sysdata.rda")

# Update thresholds_config with new k1_to_k2_for_k3 values
thresholds_config <- list(
  GLOBETROTTER = list(
    k1_to_k2_for_k2 = 0.69,
    k1_to_k2_for_k3 = 0.90,   # NEW: stricter threshold for k>=3
    k2_to_k3 = 0.43,
    k3_to_k4 = 0.31,
    clustering_strength = 0.78,cv_threshold =1
  ),
  DATES = list(
    k1_to_k2_for_k2 = 1.20,
    k1_to_k2_for_k3 = 2.59,   # Already exists, just confirming
    k2_to_k3 = 0.71,
    k3_to_k4 = 0.49,
    clustering_strength = 0.983, cv_threshold=2.5
  )
)

# Check it looks right
print(thresholds_config)

# Save back to sysdata.rda
usethis::use_data(thresholds_config, internal = TRUE, overwrite = TRUE)

# Reload package
devtools::load_all()

# Verify it worked
print(admixplorer:::thresholds_config)

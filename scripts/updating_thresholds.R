# Load current sysdata
load("R/sysdata.rda")

# Update thresholds_config with new k1_to_k2_for_k3 values
thresholds_config <- list(
  GLOBETROTTER = list(
    k1_to_k2_for_k2 = 0.56,
    k1_to_k2_for_k3 = 0.66,   # NEW: stricter threshold for k>=3
    k2_to_k3 = 0.34,
    k3_to_k4 = 0.18,
    clustering_strength = 0.78,cv_threshold =1
  ),
  DATES = list(
    k1_to_k2_for_k2 = 0.97,
    k1_to_k2_for_k3 = 2.21,   # Already exists, just confirming
    k2_to_k3 = 0.58,
    k3_to_k4 = 0.42,
    clustering_strength = 0.99, cv_threshold=1
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

#!/bin/bash

MUNGED_DIR="/Users/apple/Documents/STAT620/Final/data/munged_sumstats"
LDSC_DIR="/Users/apple/ldsc-2.0.1"
REF_DIR="/Users/apple/Documents/STAT620/HW/HW4/ldsc_inputs"
RESULTS_DIR="data/genetic_correlation_results/"
mkdir -p "${RESULTS_DIR}"
#############################################
### Step 3: run ldsc to calculate genetic correlation
#############################################

traits=(
    "age_at_first_birth"
    "cannabis_dependence"
    "chronotype"
    "income"
    "number_of_children_born"
    "ptsd"
    "sporadic_miscarriage"
)

for i in "${!traits[@]}"; do
  for j in $(seq $((i+1)) $((${#traits[@]}-1))); do
    t1=${traits[i]}
    t2=${traits[j]}

    sum1="${MUNGED_DIR}/${t1}.sumstats.gz"
    sum2="${MUNGED_DIR}/${t2}.sumstats.gz"
    out="${RESULTS_DIR}/${t1}_${t2}_rg"

    if [[ -f "${sum1}" ]] && [[ -f "${sum2}" ]]; then
      echo "Running genetic correlation between ${t1} and ${t2}..."
      python "${LDSC_DIR}/ldsc.py" \
        --rg "${sum1},${sum2}" \
        --ref-ld-chr "${REF_DIR}/for_h2/eur_w_ld_chr/" \
        --w-ld-chr "${REF_DIR}/for_h2/eur_w_ld_chr/" \
        --out "${out}"
    else
      echo "ERROR: missing munged file for ${t1} or ${t2}:"
      [[ ! -f "${sum1}" ]] && echo "  -> ${sum1} not found"
      [[ ! -f "${sum2}" ]] && echo "  -> ${sum2} not found"
    fi

  done
done
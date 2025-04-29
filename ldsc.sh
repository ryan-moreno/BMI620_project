#!/bin/bash
# Create directories if they don't exist
mkdir -p /Users/apple/Documents/STAT620/Final/data/munged_sumstats
mkdir -p /Users/apple/Documents/STAT620/Final/results

eval "$(conda shell.bash hook)"
conda activate ldsc_env

# Set paths explicitly
LDSC_DIR="/Users/apple/ldsc-2.0.1"
SUMSTATS_DIR="/Users/apple/Documents/STAT620/Final/data/sumstats"
MUNGED_DIR="/Users/apple/Documents/STAT620/Final/data/munged_sumstats"
RESULTS_DIR="/Users/apple/Documents/STAT620/Final/data/heritability_results"
REF_DIR="/Users/apple/Documents/STAT620/HW/HW4/ldsc_inputs"

echo "Checking reference files..."
if [ ! -f "${REF_DIR}/w_hm3.snplist" ]; then
    echo "ERROR: Reference file not found: ${REF_DIR}/w_hm3.snplist"
    echo "Please check the path to your reference files"
    exit 1
fi

echo "LD score reference files:"
ls -l ${REF_DIR}/for_h2/eur_w_ld_chr/

#############################################
### Step 1: QC and reformat GWAS summary statistics for ldsc
#############################################

echo "Step 1: Processing age_at_first_birth..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/age_at_first_birth.tsv.gz \
  --N 542901 \
  --snp variant_id \
  --a1 effect_allele \
  --a2 other_allele \
  --p p_value \
  --out ${MUNGED_DIR}/age_at_first_birth \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

echo "Step 1: Processing cannabis_dependence..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/cannabis_dependence.tsv.gz \
  --N-cas 17968 \
  --N-con 357219 \
  --snp SNP \
  --a1 A1 \
  --a2 A2 \
  --p P \
  --out ${MUNGED_DIR}/cannabis_dependence \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

echo "Step 1: Processing chronotype..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/chronotype.tsv.gz \
  --N 449734 \
  --snp SNP \
  --a1 ALLELE1 \
  --a2 ALLELE0 \
  --p P_BOLT_LMM \
  --out ${MUNGED_DIR}/chronotype \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

echo "Step 1: Processing income..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/income.tsv.gz \
  --N 286301 \
  --snp SNP \
  --a1 Effect_allele \
  --a2 Non_effect_Allele \
  --p P \
  --out ${MUNGED_DIR}/income \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

echo "Step 1: Processing number_of_children_born..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/number_of_children_born.tsv.gz \
  --N 318463 \
  --snp SNPID \
  --a1 A1 \
  --a2 A2 \
  --p Pvalue \
  --out ${MUNGED_DIR}/number_of_children_born \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

echo "Step 1: Processing ptsd..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/ptsd.tsv.gz \
  --N-cas 23213 \
  --N-con 151447 \
  --snp SNP \
  --a1 A1 \
  --a2 A2 \
  --p P \
  --out ${MUNGED_DIR}/ptsd \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

echo "Step 1: Processing sporadic_miscarriage..."
python ${LDSC_DIR}/munge_sumstats.py \
  --sumstats ${SUMSTATS_DIR}/sporadic_miscarriage.txt.gz \
  --N-cas 499996 \
  --N-con 174109 \
  --snp rsID \
  --a1 A1 \
  --a2 A2 \
  --p P \
  --out ${MUNGED_DIR}/sporadic_miscarriage \
  --merge-alleles ${REF_DIR}/w_hm3.snplist

#############################################
### Step 2: run ldsc to calculate total heritability
#############################################

if [ -f "${MUNGED_DIR}/age_at_first_birth.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for age_at_first_birth..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/age_at_first_birth.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/age_at_first_birth
else
    echo "ERROR: Munging failed for age_at_first_birth. Check the log file: ${MUNGED_DIR}/age_at_first_birth.log"
fi

if [ -f "${MUNGED_DIR}/cannabis_dependence.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for cannabis_dependence..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/cannabis_dependence.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/cannabis_dependence
else
    echo "ERROR: Munging failed for cannabis_dependence. Check the log file: ${MUNGED_DIR}/cannabis_dependence.log"
fi

if [ -f "${MUNGED_DIR}/chronotype.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for chronotype..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/chronotype.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/chronotype
else
    echo "ERROR: Munging failed for chronotype. Check the log file: ${MUNGED_DIR}/chronotype.log"
fi

if [ -f "${MUNGED_DIR}/income.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for income..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/income.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/income
else
    echo "ERROR: Munging failed for income. Check the log file: ${MUNGED_DIR}/income.log"
fi

if [ -f "${MUNGED_DIR}/number_of_children_born.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for number_of_children_born..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/number_of_children_born.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/number_of_children_born
else
    echo "ERROR: Munging failed for number_of_children_born. Check the log file: ${MUNGED_DIR}/number_of_children_born.log"
fi

if [ -f "${MUNGED_DIR}/ptsd.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for ptsd..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/ptsd.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/ptsd
else
    echo "ERROR: Munging failed for ptsd. Check the log file: ${MUNGED_DIR}/ptsd.log"
fi

if [ -f "${MUNGED_DIR}/sporadic_miscarriage.sumstats.gz" ]; then
    echo "Step 2: Running LD Score regression for sporadic_miscarriage..."
    python ${LDSC_DIR}/ldsc.py \
      --h2 ${MUNGED_DIR}/sporadic_miscarriage.sumstats.gz \
      --ref-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --w-ld-chr ${REF_DIR}/for_h2/eur_w_ld_chr/ \
      --out ${RESULTS_DIR}/sporadic_miscarriage
else
    echo "ERROR: Munging failed for sporadic_miscarriage. Check the log file: ${MUNGED_DIR}/sporadic_miscarriage.log"
fi
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats

# ============================================
# 1. DATA LOADING & PREPARATION
# ============================================
# Load dataset and standardize column names for easier access
try:
    data = pd.read_csv("Sleep_health_and_lifestyle_dataset.csv")
    data.columns = data.columns.str.lower().str.replace(" ", "_")
    print("✓ Dataset loaded successfully!")
except FileNotFoundError:
    print("Error: File not found!")
    data = pd.DataFrame()

if not data.empty:
    # ============================================
    # 2. DATA EXPLORATION
    # ============================================
    print("\n" + "="*50)
    print("DATA EXPLORATION")
    print("="*50)
    
    # Check basic dataset structure
    print("\nDataset Info:")
    print(data.info())
    print(f"\nGender Distribution:\n{data['gender'].value_counts()}")
    
    # Visualize sleep distribution
    plt.figure(figsize=(8, 4))
    sns.histplot(data['sleep_duration'], bins=10, kde=True, color='skyblue')
    plt.title("Distribution of Sleep Duration")
    plt.xlabel("Hours of Sleep")
    plt.show()
    
    # ============================================
    # 3. DATA CLEANING & VALIDATION
    # ============================================
    print("\n" + "="*50)
    print("DATA CLEANING")
    print("="*50)
    
    # Function to validate values within acceptable ranges
    def validate_range(x, min_val, max_val):
        """Return value if within range, otherwise NaN"""
        if pd.isna(x): 
            return np.nan
        return x if min_val <= x <= max_val else np.nan
    
    # Track data cleaning impact
    rows_before = len(data)
    data_clean = data.copy()
    
    # Validate sleep (3-12 hours) and stress (1-10 scale)
    data_clean["sleep_duration"] = data_clean["sleep_duration"].apply(
        lambda x: validate_range(x, 3, 12)
    )
    data_clean["stress_level"] = data_clean["stress_level"].apply(
        lambda x: validate_range(x, 1, 10)
    )
    
    # Remove incomplete records
    data_clean = data_clean.dropna(subset=["sleep_duration", "stress_level", "gender"])
    
    rows_removed = rows_before - len(data_clean)
    print(f"Removed {rows_removed} rows with invalid data ({rows_removed/rows_before*100:.1f}%)")
    print(f"Final sample size: {len(data_clean)} participants")
    
    # ============================================
    # 4. RESEARCH QUESTION 1: SLEEP vs STRESS
    # ============================================
    print("\n" + "="*50)
    print("RQ1: SLEEP DURATION vs STRESS LEVEL")
    print("="*50)
    
    # Calculate correlation and significance
    r, p_value = stats.pearsonr(data_clean["sleep_duration"], data_clean["stress_level"])
    
    print(f"\nCorrelation coefficient: {r:.3f}")
    print(f"P-value: {p_value:.4f}")
    
    if p_value < 0.05:
        strength = "strong" if abs(r) > 0.7 else "moderate" if abs(r) > 0.4 else "weak"
        direction = "negative" if r < 0 else "positive"
        print(f"✓ Statistically significant {strength} {direction} relationship")
    else:
        print("✗ No significant relationship found")
    
    # Visualize relationship with regression line
    plt.figure(figsize=(9, 5))
    sns.regplot(data=data_clean, x="sleep_duration", y="stress_level", 
                scatter_kws={'alpha':0.5}, line_kws={'color':'red', 'linewidth':2})
    plt.title(f"Sleep vs Stress (r={r:.3f}, p={p_value:.4f})")
    plt.xlabel("Sleep Duration (hours)")
    plt.ylabel("Stress Level (1-10)")
    plt.show()
    
    # ============================================
    # 5. RESEARCH QUESTION 2: GENDER INFLUENCE
    # ============================================
    print("\n" + "="*50)
    print("RQ2: GENDER INFLUENCE ON SLEEP & STRESS")
    print("="*50)
    
    # Separate by gender
    male_data = data_clean[data_clean["gender"] == "Male"]
    female_data = data_clean[data_clean["gender"] == "Female"]
    
    # Sleep comparison
    print("\nSleep Duration by Gender:")
    print(f"  Male: {male_data['sleep_duration'].mean():.2f} ± {male_data['sleep_duration'].std():.2f} hours")
    print(f"  Female: {female_data['sleep_duration'].mean():.2f} ± {female_data['sleep_duration'].std():.2f} hours")
    
    t_stat, p_val = stats.ttest_ind(male_data["sleep_duration"], female_data["sleep_duration"])
    print(f"  T-test: t={t_stat:.3f}, p={p_val:.4f}")
    print(f"  {'✓ Significant difference' if p_val < 0.05 else '✗ No significant difference'}")
    
    # Stress comparison
    print("\nStress Level by Gender:")
    print(f"  Male: {male_data['stress_level'].mean():.2f} ± {male_data['stress_level'].std():.2f}")
    print(f"  Female: {female_data['stress_level'].mean():.2f} ± {female_data['stress_level'].std():.2f}")
    
    t_stat, p_val = stats.ttest_ind(male_data["stress_level"], female_data["stress_level"])
    print(f"  T-test: t={t_stat:.3f}, p={p_val:.4f}")
    print(f"  {'✓ Significant difference' if p_val < 0.05 else '✗ No significant difference'}")
    
    # Visualize with simple bar charts
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    
    sns.barplot(data=data_clean, x="gender", y="sleep_duration", palette="Set2", 
                errorbar="sd", ax=axes[0])
    axes[0].set_title("Average Sleep Duration by Gender")
    axes[0].set_ylabel("Hours")
    
    sns.barplot(data=data_clean, x="gender", y="stress_level", palette="Set2", 
                errorbar="sd", ax=axes[1])
    axes[1].set_title("Average Stress Level by Gender")
    axes[1].set_ylabel("Stress (1-10)")
    
    plt.tight_layout()
    plt.show()
    
    # ============================================
    # 6. COMBINED ANALYSIS: SLEEP CATEGORIES
    # ============================================
    # Create sleep groups: Short (<6h), Normal (6-8h), Long (>8h)
    bins = [0, 6, 8, 24]
    labels = ["Short", "Normal", "Long"]
    data_clean["sleep_group"] = pd.cut(data_clean["sleep_duration"], bins=bins, labels=labels)
    
    # Visualize sleep groups by gender
    plt.figure(figsize=(10, 5))
    sns.barplot(data=data_clean, x="sleep_group", y="stress_level", 
                hue="gender", palette="coolwarm", errorbar="sd")
    plt.title("Stress by Sleep Category & Gender")
    plt.xlabel("Sleep Category")
    plt.ylabel("Stress Level")
    plt.show()
    
    # ============================================
    # 7. SUMMARY
    # ============================================
    print("\n" + "="*50)
    print("SUMMARY STATISTICS")
    print("="*50)
    
    print(f"\nTotal participants: {len(data_clean)}")
    print(f"Gender: {dict(data_clean['gender'].value_counts())}")
    print(f"\nOverall sleep: {data_clean['sleep_duration'].mean():.2f} ± {data_clean['sleep_duration'].std():.2f} hours")
    print(f"Overall stress: {data_clean['stress_level'].mean():.2f} ± {data_clean['stress_level'].std():.2f}/10")
    
    print("\nStress by Sleep Category:")
    for group in labels:
        group_data = data_clean[data_clean["sleep_group"] == group]
        if len(group_data) > 0:
            avg = group_data["stress_level"].mean()
            print(f"  {group}: {avg:.2f}/10 (n={len(group_data)})")
    
    print("\n" + "="*50)
    print("ANALYSIS COMPLETE ✓")
    print("="*50)
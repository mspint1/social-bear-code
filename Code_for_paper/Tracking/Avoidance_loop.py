import pandas as pd
import numpy as np
import os
import glob
from shapely.geometry import Point
from shapely.ops import transform
import pyproj
from itertools import combinations
import ortega

# Set up coordinate projection (WGS84 to EPSG:3857)
p = pyproj.Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)

def convert_to_projected(x, lat, lon):
    point = Point(float(x[lon]), float(x[lat]))
    projected_point = transform(p.transform, point)
    x[lat], x[lon] = projected_point.y, projected_point.x  # y = northing, x = easting
    return x

def run_pairwise_avoidance(file1, file2, year,
                           min_delay=1441,
                           max_delay=10080,
                           max_el_time_min=90,
                           AVOID_DIR_MIN=140,      # degrees: how "opposite" headings must be
                           GAP_MAX_MIN=120,        
                           REQUIRE_INCREASING_DIST=True):
    # Load data
    df1 = pd.read_csv(file1)
    df2 = pd.read_csv(file2)

    # Combine and clean
    combined_df = pd.concat([df1, df2], ignore_index=True)
    combined_df['study-timestamp'] = pd.to_datetime(
        combined_df['study-timestamp'], errors='coerce', format="mixed", utc=True
    )
    combined_df = combined_df.dropna(subset=['study-timestamp'])
    combined_df['study-timestamp'] = combined_df['study-timestamp'].dt.tz_localize(None)
    combined_df.sort_values(['individual-local-identifier', 'study-timestamp'], inplace=True)

    # Project lat/lon
    combined_df = combined_df.apply(
        lambda x: convert_to_projected(x, 'location-latitude', 'location-longitude'),
        axis=1
    )

    # Auto time window
    start_time = str(combined_df['study-timestamp'].min())
    end_time   = str(combined_df['study-timestamp'].max())
    if pd.isna(start_time) or pd.isna(end_time):
        print(f" Skipping due to invalid time: {file1} vs {file2}")
        return None

    # ORTEGA analysis
    delayed = ortega.ORTEGA(
        data=combined_df,
        start_time=start_time,
        end_time=end_time,
        latitude_field='location-latitude',
        longitude_field='location-longitude',
        minute_min_delay=min_delay,
        minute_max_delay=max_delay,
        time_field='study-timestamp',
        id_field='individual-local-identifier',
        max_el_time_min=max_el_time_min,
        speed_average=True,
        attr_fields=['altitude']
    )
    results = delayed.interaction_analysis()
    results.compute_interaction_duration()

    df = results.df_all_intersection_pairs.copy()
    if df.empty:
        print(f" No intersections found: {os.path.basename(file1)} vs {os.path.basename(file2)}")
        return None

    # Direction difference (0..180)
    df['abs_diff_direction'] = np.abs((df['p1_direction'] - df['p2_direction'] + 180) % 360 - 180)

    df = df.sort_values("p1_t_start").reset_index(drop=True)
    df["gap_minutes"] = df["p1_t_start"].diff().dt.total_seconds() / 60

    # Anti-aligned if directions differ by least threshold
    df["anti_aligned"] = df["abs_diff_direction"] >= AVOID_DIR_MIN

    # require increasing distance across events
    dist_cols = [c for c in df.columns if 'dist' in c.lower()]
    if REQUIRE_INCREASING_DIST and dist_cols:
        dcol = dist_cols[0]
        # distance change vs previous event 
        df["dist_delta"] = df[dcol].diff()
        # increasing if current distance ≥ previous 
        df["distance_increasing"] = df["dist_delta"] >= 0
        # First row has NaN, treat as not increasing
        df.loc[df.index[0], "distance_increasing"] = False
        df["avoid_event"] = df["anti_aligned"] & df["distance_increasing"]
    else:
        # fall back to anti-alignment
        df["avoid_event"] = df["anti_aligned"]

    # Continues a run if avoid_event AND gap ≤ threshold
    df["continues_run"] = df["avoid_event"] & (df["gap_minutes"].fillna(np.inf) <= GAP_MAX_MIN)

    # Start a new run whenever we do NOT continue a run
    df["run_id"] = (~df["continues_run"]).cumsum()

    # Runs must have ≥ 2 avoid events to count
    run_sizes = df[df["avoid_event"]].groupby("run_id").size()
    valid_runs = set(run_sizes[run_sizes >= 2].index)

    # Only keep events inside qualifying runs
    df["dyadic_avoidance"] = df["avoid_event"] & df["run_id"].isin(valid_runs)

    filtered_df = df[df["dyadic_avoidance"]].copy()

    # Extract Bear IDs
    bear_ids = combined_df['individual-local-identifier'].unique()
    if len(bear_ids) != 2:
        print(f" Skipping pair with unexpected number of individuals: {file1}, {file2}")
        return None
    bear1, bear2 = sorted(bear_ids)

    # Summary metrics
    avoidance_sequences = len(valid_runs)
    avoidance_ellipses  = len(filtered_df)

    return {
        "Year": year,
        "Bear1": bear1,
        "Bear2": bear2,
        "Total_Interactions": len(results.df_all_intersection_pairs),
        "Avoidance_Sequences": avoidance_sequences,
        "Dyadic_Avoidance_Events": avoidance_ellipses,
        "Dir_Threshold_Deg": AVOID_DIR_MIN,
        "Gap_Max_Min": GAP_MAX_MIN,
        "Require_Increasing_Distance": bool(REQUIRE_INCREASING_DIST and dist_cols)
    }

# Locate all files and loop through bear pairs
root_dir = "G:/2023_MPG_Bear_Data/Bear_Codes/Derived Data/"
all_files = glob.glob(os.path.join(root_dir, "**", "*_Positions.csv"), recursive=True)
files_by_year = {}
for file in all_files:
    base = os.path.basename(file)
    parts = base.split("_")
    if len(parts) >= 2 and parts[0].isdigit():
        year = parts[0]
        files_by_year.setdefault(year, []).append(file)

# Store summary results
summary_records = []

# Run function and save
for year in sorted(files_by_year.keys()):
    file_list = files_by_year[year]
    print(f"\n🔵 Processing {year} ({len(file_list)} bears)")
    for file1, file2 in combinations(file_list, 2):
        try:
            result = run_pairwise_avoidance(file1, file2, year,
                                            AVOID_DIR_MIN=140,
                                            GAP_MAX_MIN=120,
                                            REQUIRE_INCREASING_DIST=True)
            if result:
                summary_records.append(result)
                print(f"🟠 {result['Bear1']} vs {result['Bear2']} — "
                      f"{result['Avoidance_Sequences']} sequences / "
                      f"{result['Dyadic_Avoidance_Events']} ellipses")
        except Exception as e:
            print(f"🔴 ERROR for {file1} vs {file2}: {e}")

summary_df = pd.DataFrame(summary_records)
summary_df.to_csv("ORTEGA_Avoidance_Summary.csv", index=False)
print("\n✅ Summary saved to ORTEGA_Avoidance_Summary.csv")

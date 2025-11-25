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

def run_pairwise_random_direction(file1, file2, year,
                                  min_delay=1441,
                                  max_delay=10080,
                                  max_el_time_min=90):

    # Load data
    df1 = pd.read_csv(file1)
    df2 = pd.read_csv(file2)

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

    # Time window
    start_time = str(combined_df['study-timestamp'].min())
    end_time   = str(combined_df['study-timestamp'].max())
    if pd.isna(start_time) or pd.isna(end_time):
        print(f" Skipping due to invalid time: {file1} vs {file2}")
        return None

    # ORTEGA 
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

    # Compute absolute direction difference
    df['abs_diff_direction'] = np.abs((df['p1_direction'] - df['p2_direction'] + 180) % 360 - 180)

    # Random = between 45° and 140°
    random_df = df[(df['abs_diff_direction'] > 45) & (df['abs_diff_direction'] < 140)]

    # IDs
    bear_ids = combined_df['individual-local-identifier'].unique()
    if len(bear_ids) != 2:
        print(f"⚠️ Skipping pair with unexpected number of individuals: {file1}, {file2}")
        return None
    bear1, bear2 = sorted(bear_ids)

    return {
        "Year": year,
        "Bear1": bear1,
        "Bear2": bear2,
        "Total_Interactions": int(len(df)),
        "Random_NonTracking_Events": int(len(random_df)),
        "Direction_Range": "45–140"
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
summary_records = []


for year in sorted(files_by_year.keys()):
    file_list = files_by_year[year]
    print(f"\n🔵 Processing {year} ({len(file_list)} bears)")
    for file1, file2 in combinations(file_list, 2):
        try:
            result = run_pairwise_random_direction(file1, file2, year)
            if result:
                summary_records.append(result)
                print(f"⚫ {result['Bear1']} vs {result['Bear2']} — "
                      f"{result['Random_NonTracking_Events']} random events (45–140°)")
        except Exception as e:
            print(f"🔴 ERROR for {file1} vs {file2}: {e}")

summary_df = pd.DataFrame(summary_records)
summary_df.to_csv("ORTEGA_RandomDirection_Summary.csv", index=False)
print("\n✅ Summary saved to ORTEGA_RandomDirection_Summary.csv")

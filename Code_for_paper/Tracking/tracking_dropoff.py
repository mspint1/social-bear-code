import pandas as pd
import ortega
import numpy as np

# Read in the CSVs
zz = pd.read_csv("F:/2023_MPG_Bear_Data/Bear_Codes/Derived Data/ZZ/2020_ZZ")
trouble = pd.read_csv("F:/2023_MPG_Bear_Data/Bear_Codes/Derived Data/Troublemaker/2020_Trouble_Positions.csv")

# Combine both dataframes
combined_df = pd.concat([zz, trouble], ignore_index=True)

# Convert timestamp to datetime
combined_df['study-timestamp'] = pd.to_datetime(combined_df['study-timestamp'], format='mixed', utc = True)

# Remove timezone if present
combined_df['study-timestamp'] = combined_df['study-timestamp'].dt.tz_localize(None)

# Sort by ID and timestamp
combined_df.sort_values(['individual-local-identifier', 'study-timestamp'], inplace=True)

pair = [40891, 40893]
pairdf = combined_df[(combined_df["individual-local-identifier"]==pair[0])|(combined_df["individual-local-identifier"]==pair[1])]

# import necessary libraries
!pip install pyproj fastkml shapely
import pyproj
from fastkml import kml
from shapely.geometry import Point
from shapely.ops import transform

p = pyproj.Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)
def convert_to_projected(x, lat, lon):
    point = Point(float(x[lon]), float(x[lat]))
    projected_point = transform(p.transform, point)
    x[lat], x[lon] = projected_point.y, projected_point.x
    return x

pairdf = pairdf.apply(lambda x: convert_to_projected(x, 'location-latitude', 'location-longitude'), axis=1)

# Define concurrent interaction
concurrent_interaction = ortega.ORTEGA(data=pairdf,
                             start_time='2020-01-01 00:00:00',  ##set a time window to subset data 
                             end_time='2020-12-30 23:00:00',
                             latitude_field = 'utm_northing',
                             longitude_field = 'utm_easting',
                             minute_min_delay=0,
                             minute_max_delay=60,# 1h
                             time_field='study-timestamp',
                             id_field='individual-local-identifier',
                             max_el_time_min=90,
                             speed_average=True,
                             attr_fields=['altitude']
                             )
concurrent_results = concurrent_interaction.interaction_analysis()
concurrent_results.compute_interaction_duration()
concurrent_results.df_interaction_events

#plot concurrent interactions
ortega.visualization.plot_original_tracks(concurrent_interaction, colors=['red', 'blue'])
ortega.visualization.plot_interaction(concurrent_interaction, concurrent_results.intersection_ellipse_pair, 
                                      colors=['red', 'blue', 'yellow'])

#method=mean
concurrent_results.attach_attributes(col=['altitude'], method='mean')

#method=difference
concurrent_results.attach_attributes(col=['altitude'], method='difference')

# define delayed interaction
delayed_interaction = ortega.ORTEGA(data=pairdf,
                             start_time='2020-01-01 00:00:00',  ##set a time window to subset data
                             end_time='2020-12-30 23:00:00',
                             latitude_field = 'location-latitude',
                             longitude_field = 'location-longitude',
                             minute_min_delay=1441, 
                             minute_max_delay=10080, #1 week
                             time_field='study-timestamp',
                             id_field='individual-local-identifier',
                             max_el_time_min=90,
                             speed_average=True,
                             attr_fields=['altitude']
                             )

delayed_results = delayed_interaction.interaction_analysis()
delayed_results.compute_interaction_duration()
delayed_results.df_interaction_events

# plot delayed interactions
ortega.visualization.plot_original_tracks(delayed_interaction, colors=['red', 'blue'])
ortega.visualization.plot_interaction(delayed_interaction, delayed_results.intersection_ellipse_pair, 
                                      colors=['red', 'blue', 'yellow'])


# import more libraries for further stats
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Extract time lags (in days) from delayed_results
lags = []
for i in month_delayed_results.interaction_list:
    time_a = i['point_a']['timestamp']  # Earlier time
    time_b = i['point_b']['timestamp']  # Later time
    delay = (time_b - time_a).total_seconds() / (60 * 60 * 24)  # Convert to days
    lags.append(delay)

# Create a new dataframe
df_lags = pd.DataFrame({'Delay (days)': lags})

# Filter range
df_lags = df_lags[(df_lags['Delay (days)'] >= 1) & (df_lags['Delay (days)'] <= 30)]

# Round the days 
df_lags['Delay (days)'] = df_lags['Delay (days)'].round()

# 5. Count delayed interactions per day
counts = df_lags['Delay (days)'].value_counts().sort_index()

# 6. Plot
plt.figure(figsize=(10, 5))
sns.lineplot(x=counts.index, y=counts.values, marker='o')
plt.title("Drop-off in Delayed Interactions by Delay Duration")
plt.xlabel("Delay Duration (Days)")
plt.ylabel("Number of Delayed Interactions")
plt.xticks(range(1, 31))
plt.grid(True)
plt.tight_layout()
plt.show()

# Create data for month tracking plot
month_delayed_interaction = ortega.ORTEGA(data=pairdf,
                             start_time='2020-05-27 00:00:00',  ##set a time window to subset data
                             end_time='2020-07-13 23:00:00',
                             latitude_field = 'location-latitude',
                             longitude_field = 'location-longitude',
                             minute_min_delay=1440, 
                             minute_max_delay=43800, #1 month
                             time_field='study-timestamp',
                             id_field='individual-local-identifier',
                             max_el_time_min=90,
                             speed_average=True,
                             attr_fields=['altitude']
                             )
month_delayed_results = month_delayed_interaction.interaction_analysis()


import pandas as pd
import matplotlib.pyplot as plt

# Copy and convert timestamps
month_df = month_delayed_results.df_interaction_events.copy()
month_df['p1_end'] = pd.to_datetime(month_df['p1_end'])
month_df['p2_start'] = pd.to_datetime(month_df['p2_start'])

# Calculate delay in days between interactions
month_df['delay_days'] = (month_df['p2_start'] - month_df['p1_end']).dt.total_seconds() / (60 * 60 * 24)

# Filter to reasonable delays (1 to 30 days)
month_df = month_df[(month_df['delay_days'] >= 1) & (month_df['delay_days'] <= 30)]

# Round to whole days for grouping
month_df['delay_days_rounded'] = month_df['delay_days'].round().astype(int)

# Count number of interactions for each delay duration
month_delay_counts = month_df['delay_days_rounded'].value_counts().sort_index()

# Plot
plt.figure(figsize=(10, 5))
plt.plot(month_delay_counts.index, month_delay_counts.values, marker='o', linewidth=2)
plt.xlabel("Delay Duration (days)")
plt.ylabel("Number of Delayed Interactions")
plt.title("Drop-off of Delayed Interactions Over 1–30 Days")
plt.grid(True)
plt.tight_layout()
plt.show()


### Difference in Direction
import matplotlib.pyplot as plt
import seaborn as sns

df = close_delayed_results.df_all_intersection_pairs

# Step 1: Create a dataframe with direction difference for each individual separately
id1_dir = df[['p1_direction']].copy()
id1_dir['ID'] = 'ID1'
id1_dir = id1_dir.rename(columns={'p1_direction': 'Direction'})

id2_dir = df[['p2_direction']].copy()
id2_dir['ID'] = 'ID2'
id2_dir = id2_dir.rename(columns={'p2_direction': 'Direction'})

# Step 2: Combine into one dataframe
direction_df = pd.concat([id1_dir, id2_dir], ignore_index=True)

# Step 3: Plot
plt.figure(figsize=(6, 4))
sns.kdeplot(
    data=direction_df,
    x='Direction',
    hue='ID',
    bw_adjust=1.2,
    linewidth=2,
    palette={'ID1': 'red', 'ID2': 'blue'}
)

# Step 4: Customize
plt.xlabel("Direction (°)")
plt.ylabel("Density")
plt.title("(a)", loc='left', fontsize=12, fontweight='bold')
plt.xlim(0, 360)
plt.tight_layout()
plt.show()
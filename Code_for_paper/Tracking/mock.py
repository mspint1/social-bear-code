import pandas as pd
import ortega
import numpy as np
import pyproj
from shapely.geometry import Point
from shapely.ops import transform
import ortega.visualization as oviz

# Load data
df = pd.read_csv(r"G:\tracking_mock.csv")

# Convert to datetime safely
df['study_timestamp'] = pd.to_datetime(df['study_timestamp'], errors='coerce')

# sort data
df.sort_values(['individual_local_identifier', 'study_timestamp'], inplace=True)

# Filter for pair
pair = ['bear_1', 'bear_2']
pairdf = df[df['individual_local_identifier'].isin(pair)]

p = pyproj.Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)

def convert_to_projected(x, lat, lon):
    point = Point(float(x[lon]), float(x[lat]))
    projected_point = transform(p.transform, point)
    x[lat], x[lon] = projected_point.y, projected_point.x
    return x

pairdf = pairdf.apply(lambda x: convert_to_projected(x, 'latitude', 'longitude'), axis=1)

# Ortega
delayed_interaction = ortega.ORTEGA(data=pairdf,
latitude_field = 'latitude',
longitude_field = 'longitude',
minute_min_delay = 1441,
minute_max_delay = 10080,
time_field = 'study_timestamp',
id_field = 'individual_local_identifier',
max_el_time_min = 90,
speed_average = True
)
delayed_results = delayed_interaction.interaction_analysis()

ortega.visualization.plot_original_tracks(delayed_interaction, colors=['red', 'blue'])
ortega.visualization.plot_interaction(delayed_interaction, delayed_results.intersection_ellipse_pair, 
                                      colors=['red', 'blue', 'yellow'])

## Tracking identify
df = delayed_results.df_all_intersection_pairs.copy()

# Ensure time and direction differences are absolute
df['abs_diff_direction'] = np.abs((df['p1_direction'] - df['p2_direction'] + 180) % 360 - 180)

# Apply dyadic tracking condition: within 7 days (10080 min) and within 45° direction change
df['dyadic_tracking'] = (df['diff_time'] <= 10080) & (df['abs_diff_direction'] <= 45)

# View result summary
print(df['dyadic_tracking'].value_counts())
# Plot

filtered_df = df[df['dyadic_tracking'] == True].copy()
filtered_df = filtered_df[
    (filtered_df['p1_t_start'] > filtered_df['p2_t_end']) |
    (filtered_df['p2_t_start'] > filtered_df['p1_t_end'])
]

filtered_indices = filtered_df.index

filtered_ellipses = [delayed_results.intersection_ellipse_pair[i] for i in filtered_indices]

# Plot ORTEGA interaction
oviz.plot_interaction(delayed_interaction, filtered_ellipses,
                      colors=['red', 'blue', 'yellow'])


### Avoidance
# Read csv
df = pd.read_csv(r"F:\avoidance_mock.csv")

# Convert to datetime safely
df['study_timestamp'] = pd.to_datetime(df['study_timestamp'], errors='coerce')

# sort
df.sort_values(['individual_local_identifier', 'study_timestamp'], inplace=True)

# Filter for pair
pair = ['bear_1', 'bear_2']
pairdf = df[df['individual_local_identifier'].isin(pair)]

p = pyproj.Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)

# Run the avoidance function
def convert_to_projected(x, lat, lon):
    point = Point(float(x[lon]), float(x[lat]))
    projected_point = transform(p.transform, point)
    x[lat], x[lon] = projected_point.y, projected_point.x
    return x

pairdf = pairdf.apply(lambda x: convert_to_projected(x, 'latitude', 'longitude'), axis=1)

delayed_interaction = ortega.ORTEGA(data=pairdf,
latitude_field = 'latitude',
longitude_field = 'longitude',
minute_min_delay = 1441,
minute_max_delay = 10080,
time_field = 'study_timestamp',
id_field = 'individual_local_identifier',
max_el_time_min = 90,
speed_average = True
)
delayed_results = delayed_interaction.interaction_analysis()

ortega.visualization.plot_original_tracks(delayed_interaction, colors=['red', 'blue'])
ortega.visualization.plot_interaction(delayed_interaction, delayed_results.intersection_ellipse_pair, 
                                      colors=['red', 'blue', 'yellow'])

## avoidance identify
df = delayed_results.df_all_intersection_pairs.copy()

# Ensure time and direction differences are absolute
df['abs_diff_direction'] = np.abs((df['p1_direction'] - df['p2_direction'] + 180) % 360 - 180)

# Apply dyadic avoidance condition: within 7 days (10080 min) and within 120 direction change
df['dyadic_avoidance'] = (df['diff_time'] <= 10080) & (df['abs_diff_direction'] >= 120)

# View result summary
print(df['dyadic_avoidance'].value_counts())
# Plot

filtered_df = df[df['dyadic_avoidance'] == True].copy()
filtered_df = filtered_df[
    (filtered_df['p1_t_start'] > filtered_df['p2_t_end']) |
    (filtered_df['p2_t_start'] > filtered_df['p1_t_end'])
]

filtered_indices = filtered_df.index

filtered_ellipses = [delayed_results.intersection_ellipse_pair[i] for i in filtered_indices]

import ortega.visualization as oviz

oviz.plot_interaction(delayed_interaction, filtered_ellipses,
                      colors=['red', 'blue', 'yellow'])


## Non-tracking
# Read data
df = pd.read_csv(r"G:\no_tracking_mock.csv")

# Convert to datetime 
df['study_timestamp'] = pd.to_datetime(df['study_timestamp'], errors='coerce')

# sort
df.sort_values(['individual_local_identifier', 'study_timestamp'], inplace=True)

# Filter for pair
pair = ['bear_1', 'bear_2']
pairdf = df[df['individual_local_identifier'].isin(pair)]

p = pyproj.Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)

def convert_to_projected(x, lat, lon):
    point = Point(float(x[lon]), float(x[lat]))
    projected_point = transform(p.transform, point)
    x[lat], x[lon] = projected_point.y, projected_point.x
    return x

pairdf = pairdf.apply(lambda x: convert_to_projected(x, 'latitude', 'longitude'), axis=1)

delayed_interaction = ortega.ORTEGA(data=pairdf,
latitude_field = 'latitude',
longitude_field = 'longitude',
minute_min_delay = 1441,
minute_max_delay = 10080,
time_field = 'study_timestamp',
id_field = 'individual_local_identifier',
max_el_time_min = 90,
speed_average = True
)
delayed_results = delayed_interaction.interaction_analysis()

ortega.visualization.plot_original_tracks(delayed_interaction, colors=['red', 'blue'])
ortega.visualization.plot_interaction(delayed_interaction, delayed_results.intersection_ellipse_pair, 
                                      colors=['red', 'blue', 'yellow'])

## non-tracking identify
df = delayed_results.df_all_intersection_pairs.copy()

# Ensure time and direction differences are absolute
df['abs_diff_direction'] = np.abs((df['p1_direction'] - df['p2_direction'] + 180) % 360 - 180)

within_angle = (df['abs_diff_direction'] <= 130) & (df['abs_diff_direction'] >= 50)

# Apply dyadic non-tracking condition: within 7 days (10080 min)
df['non_tracking'] = (df['diff_time'] <= 10080) & within_angle

# View result summary
print(df['non_tracking'].value_counts())
# Plot

filtered_df = df[df['non_tracking'] == True].copy()
filtered_df = filtered_df[
    (filtered_df['p1_t_start'] > filtered_df['p2_t_end']) |
    (filtered_df['p2_t_start'] > filtered_df['p1_t_end'])
]

filtered_indices = filtered_df.index

filtered_ellipses = [delayed_results.intersection_ellipse_pair[i] for i in filtered_indices]

import ortega.visualization as oviz

oviz.plot_interaction(delayed_interaction, filtered_ellipses,
                      colors=['red', 'blue', 'yellow'])
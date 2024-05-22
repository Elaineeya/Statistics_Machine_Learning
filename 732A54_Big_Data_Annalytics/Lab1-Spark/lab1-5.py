from pyspark import SparkContext

sc = SparkContext(appName = "temperature analysis1-5")
precipitation_file = sc.textFile("BDA/input/precipitation-readings.csv")
stations_file = sc.textFile("BDA/input/stations-Ostergotland.csv")

precipitation_lines = precipitation_file.map(lambda line: line.split(";"))
stations_lines = stations_file.map(lambda line: line.split(";"))

# Get the list of station numbers in Östergotland
ostergotland_stations = stations_lines.map(lambda x: x[0]).collect()

# Broadcast the list of station numbers to all nodes
broadcast_stations = sc.broadcast(ostergotland_stations)

# (key, value) = ((year, month, station), precipitation)
daily_precipitation = precipitation_lines.map(lambda x: ((x[1][0:4], x[1][5:7], x[0]), float(x[3])))

# Filter for years 1993-2016 and for stations in Östergotland
filtered_precipitation = daily_precipitation.filter(lambda x: int(x[0][0]) >= 1993 and int(x[0][0]) <= 2016 and x[0][2] in broadcast_stations.value)

# Calculate total monthly precipitation for each station
total_monthly_precipitation = filtered_precipitation.reduceByKey(lambda a, b: a + b)

# (key, value) = ((year, month), total_monthly_precipitation)
monthly_precipitation = total_monthly_precipitation.map(lambda x: ((x[0][0], x[0][1]), x[1]))

# Calculate average monthly precipitation
monthly_avg_precipitation = monthly_precipitation.groupByKey().mapValues(lambda temps: sum(temps) / len(temps))

# Output: Year, month, average monthly precipitation
monthly_avg_precipitation_output = monthly_avg_precipitation.map(lambda x: (x[0][0], x[0][1], x[1]))
monthly_avg_precipitation_output.saveAsTextFile("BDA/output/lab1-5/")

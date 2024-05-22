from pyspark import SparkContext

sc = SparkContext(appName = "temperature analysis1-4")
temperature_file = sc.textFile("BDA/input/temperature-readings.csv")
precipitation_file = sc.textFile("BDA/input/precipitation-readings.csv")

temperature_lines = temperature_file.map(lambda line: line.split(";"))
precipitation_lines = precipitation_file.map(lambda line: line.split(";"))

# (key, value) = (station, temperature)
temperature_data = temperature_lines.map(lambda x: (x[0], float(x[3])))

# (key, value) = (station, precipitation)
precipitation_data = precipitation_lines.map(lambda x: (x[0], float(x[3])))

# Filter for temperatures between 25 and 30 degrees
filtered_temperature_data = temperature_data.filter(lambda x: x[1] >= 25 and x[1] <= 30)

# Filter for precipitation between 100 and 200 mm
filtered_precipitation_data = precipitation_data.filter(lambda x: x[1] >= 100 and x[1] <= 200)

# Get maximum temperature for each station
max_temperature = filtered_temperature_data.reduceByKey(max)

# Get maximum precipitation for each station
max_precipitation = filtered_precipitation_data.reduceByKey(max)

# Join the two datasets
joined_data = max_temperature.join(max_precipitation)

# Output: Station number, maximum measured temperature, maximum daily precipitation
stations_output = joined_data.map(lambda x: (x[0], x[1][0], x[1][1]))
stations_output.saveAsTextFile("BDA/output/lab1-4/")

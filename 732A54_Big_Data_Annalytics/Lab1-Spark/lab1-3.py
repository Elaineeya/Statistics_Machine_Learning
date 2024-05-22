from pyspark import SparkContext

sc = SparkContext(appName = "temperature analysis1-3")
temperature_file = sc.textFile("BDA/input/temperature-readings.csv")
lines = temperature_file.map(lambda line: line.split(";"))

# (key, value) = ((year, month, day, station), temperature)
daily_temperature = lines.map(lambda x: ((x[1][0:4], x[1][5:7], x[1][8:10], x[0]), float(x[3])))

# Filter for years 1960-2014
filtered_temperatures = daily_temperature.filter(lambda x: int(x[0][0]) >= 1960 and int(x[0][0]) <= 2014)

# Get daily maximum and minimum temperatures
daily_max_temperatures = filtered_temperatures.reduceByKey(max)
daily_min_temperatures = filtered_temperatures.reduceByKey(min)

# Calculate daily average temperature
daily_avg_temperature = daily_max_temperatures.join(daily_min_temperatures).mapValues(lambda x: (x[0] + x[1]) / 2)

# (key, value) = ((year, month, station), daily_avg_temperature)
monthly_temperature = daily_avg_temperature.map(lambda x: ((x[0][0], x[0][1], x[0][3]), x[1]))

# Calculate average monthly temperature
monthly_avg_temperature = monthly_temperature.groupByKey().mapValues(lambda temps: sum(temps) / len(temps))

# Output: Year, month, station number, average monthly temperature
monthly_avg_output = monthly_avg_temperature.map(lambda x: (x[0][0], x[0][1], x[0][2], x[1]))
monthly_avg_output.saveAsTextFile("BDA/output/lab1-3/")

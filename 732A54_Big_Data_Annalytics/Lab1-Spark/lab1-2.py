from pyspark import SparkContext

sc = SparkContext(appName = "temperature analysis1-2")
temperature_file = sc.textFile("BDA/input/temperature-readings.csv")
lines = temperature_file.map(lambda line: line.split(";"))

# (key, value) = ((year, month), (station, temperature))
temperature_data = lines.map(lambda x: ((x[1][0:4], x[1][5:7]), (x[0], float(x[3]))))

# Filter for temperatures > 10 degrees and for years 1950-2014
filtered_temperatures = temperature_data.filter(lambda x: x[1][1] > 10 and int(x[0][0]) >= 1950 and int(x[0][0]) <= 2014)

# Count the number of readings for each month
# (key, value) = ((year, month), 1)
count_temperatures = filtered_temperatures.map(lambda x: ((x[0][0], x[0][1]), 1))

# Count the number of readings for each month
total_count_temperatures = count_temperatures.reduceByKey(lambda a, b: a + b)

# Output: Year, month, total count
total_count_output = total_count_temperatures.map(lambda x: (x[0][0], x[0][1], x[1]))
total_count_output.saveAsTextFile("BDA/output/lab1-2/total_count/")

# Count the number of readings for each month, taking only distinct readings from each station
# (key, value) = ((year, month), (station, 1))
distinct_count_temperatures = filtered_temperatures.map(lambda x: ((x[0][0], x[0][1]), (x[1][0], 1))).distinct()

# Count the number of readings for each month, taking only distinct readings from each station
distinct_count_temperatures = distinct_count_temperatures.map(lambda x: ((x[0][0], x[0][1]), 1)).reduceByKey(lambda a, b: a + b)

# Output: Year, month, distinct count
distinct_count_output = distinct_count_temperatures.map(lambda x: (x[0][0], x[0][1], x[1]))
distinct_count_output.saveAsTextFile("BDA/output/lab1-2/distinct_count/")


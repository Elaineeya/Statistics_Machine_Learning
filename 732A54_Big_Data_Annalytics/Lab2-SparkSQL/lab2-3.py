from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext()
sqlContext = SQLContext(sc)

# Load a text file and convert each line to a tuple.
rdd = sc.textFile("BDA/input/temperature-readings.csv")
parts = rdd.map(lambda l: l.split(";"))
temp_readings_row = parts.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), int(p[1].split("-")[2]), float(p[3]), p[4] ))

temp_readings_string = ["station", "date", "year", "month", "day", "value", "quality"]
# Apply the schema to the RDD.
df = sqlContext.createDataFrame(temp_readings_row, temp_readings_string)
# Register the DataFrame as a table.
df.registerTempTable("tempReadingsTable")

# Filter for the period 1960-2014
filtered_df = df.filter((df.year >= 1960) & (df.year <= 2014))

# Calculate daily average temperature
daily_avg_temperature = filtered_df.groupBy('year', 'month', 'day', 'station').agg(((F.max('value') + F.min('value')) / 2).alias('dailyavg'))

# Calculate monthly average temperature for each station
monthly_avg_temperature = daily_avg_temperature.groupBy('year', 'month', 'station').agg(F.avg('dailyavg').alias('avgMonthlyTemperature')).orderBy('avgMonthlyTemperature', ascending=False)

# Ouput the results
monthly_avg_temperature.rdd.saveAsTextFile("BDA/output/lab2-3/")

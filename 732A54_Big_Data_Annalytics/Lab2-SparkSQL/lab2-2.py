from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext()
sqlContext = SQLContext(sc)

# Load a text file and convert each line to a tuple.
rdd = sc.textFile("BDA/input/temperature-readings.csv")
parts = rdd.map(lambda l: l.split(";"))
temp_readings_row = parts.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))

temp_readings_string = ["station", "date", "year", "month", "time", "value", "quality"]

# Apply the schema to the RDD.
df = sqlContext.createDataFrame(temp_readings_row, temp_readings_string)
# Register the DataFrame as a table.
df.registerTempTable("tempReadingsTable")

# Filter for the period 1950-2014 which are higher than 10 degrees
filtered_df = df.filter((df.year >= 1950) & (df.year <= 2014) & (df.value > 10))

# Count the number of readings for each month 
total_count_temperatures = filtered_df.groupBy('year', 'month').count().orderBy('count', ascending=False)

# Repeat the exercise, this time taking only distinct readings from each station
distinct_temperatures = filtered_df.dropDuplicates(['year', 'month', 'station'])
distinct_count_temperatures = distinct_temperatures.groupBy('year', 'month').count().orderBy('count', ascending=False)

# Ouput the results
total_count_temperatures.rdd.saveAsTextFile("BDA/output/lab2-2/total_count/")
distinct_count_temperatures.rdd.saveAsTextFile("BDA/output/lab2-2/distinct_count/")

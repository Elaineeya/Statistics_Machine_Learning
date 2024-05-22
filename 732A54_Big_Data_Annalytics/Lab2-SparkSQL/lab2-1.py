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

# Filter for the period 1950-2014
filtered_df = df.filter((df.year >= 1950) & (df.year <= 2014))

# Calculate the highest and lowest temperatures measured each year
max_temperatures = filtered_df.groupBy('year').agg(F.max('value').alias('value'))
min_temperatures = filtered_df.groupBy('year').agg(F.min('value').alias('value'))

# Join back with original dataframe to get the station
max_temperatures = max_temperatures.join(filtered_df, on=['year', 'value']) \
    .withColumnRenamed('value', 'MaxValue') \
    .select('year', 'station', 'MaxValue') \
    .orderBy('maxValue', ascending=False)

min_temperatures = min_temperatures.join(filtered_df, on=['year', 'value']) \
    .withColumnRenamed('value', 'minValue') \
    .select('year', 'station', 'minValue') \
    .orderBy('minValue', ascending=False)

# Ouput the results
max_temperatures.rdd.saveAsTextFile("BDA/output/lab2-1/max")
min_temperatures.rdd.saveAsTextFile("BDA/output/lab2-1/min")

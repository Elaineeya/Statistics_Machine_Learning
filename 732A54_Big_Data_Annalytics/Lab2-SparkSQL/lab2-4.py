from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext()
sqlContext = SQLContext(sc)

# Load temperature-readings.csv and convert each line to a tuple.
rdd_temp = sc.textFile("BDA/input/temperature-readings.csv")
parts_temp = rdd_temp.map(lambda l: l.split(";"))
temp_readings_row = parts_temp.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))

temp_readings_string = ["station", "date", "year", "month", "time", "temp", "quality"]
# Apply the schema to the RDD.
temp_df = sqlContext.createDataFrame(temp_readings_row, temp_readings_string)
# Register the DataFrame as a table.
temp_df.registerTempTable("tempReadingsTable")

# Filter for temperatures between 25 and 30 degrees
filtered_temp_df = temp_df.filter((temp_df.temp >= 25) & (temp_df.temp <= 30))

# Load precipitation-readings.csv and convert each line to a tuple.
rdd_prec = sc.textFile("BDA/input/precipitation-readings.csv")
parts_prec = rdd_prec.map(lambda l: l.split(";"))
prec_readings_row = parts_prec.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))

prec_readings_string = ["station", "date", "year", "month", "time", "prec", "quality"]
# Apply the schema to the RDD.
prec_df = sqlContext.createDataFrame(prec_readings_row, prec_readings_string)
# Register the DataFrame as a table.
prec_df.registerTempTable("precReadingsTable")

# Filter for precipitation between 100 and 200 mm
filtered_prec_df = prec_df.filter((prec_df.prec >= 100) & (prec_df.prec <= 200))

# Calculate maximum temperature for each station
max_temperature = filtered_temp_df.groupBy('station').agg(F.max('temp').alias('maxTemp'))

# Calculate maximum daily precipitation for each station
max_precipitation = filtered_prec_df.groupBy('station').agg(F.max('prec').alias('maxPrec'))

# Join the two dataframes
joined_df = max_temperature.join(max_precipitation, 'station').orderBy('station', ascending=False)

# Ouput the results
joined_df.rdd.saveAsTextFile("BDA/output/lab2-4/")

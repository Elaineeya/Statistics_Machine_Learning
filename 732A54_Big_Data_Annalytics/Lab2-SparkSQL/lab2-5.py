from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext()
sqlContext = SQLContext(sc)

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

# Load stations-Ostergotland.csv and convert each line to a tuple.
rdd_stations = sc.textFile("BDA/input/stations-Ostergotland.csv")
parts_stations = rdd_stations.map(lambda l: l.split(";"))
stations_row = parts_stations.map(lambda p: (p[0], p[1], float(p[2]), float(p[3]), float(p[4]), p[5], p[6], float(p[7])))

stations_string = ["station", "name", "height", "latitude", "longitude", "from_date", "to_date", "elevation"]
# Apply the schema to the RDD.
stat_df = sqlContext.createDataFrame(stations_row, stations_string)
# Register the DataFrame as a table.
stat_df.registerTempTable("stationsTable")

# Broadcast the station numbers
stat_num = sc.broadcast(stat_df.select("Station").rdd.flatMap(lambda x: x).collect())

# Filter for the period 1993-2016 and for the stations in Östergotland
filtered_prec_df = prec_df.filter((prec_df.year >= 1993) & (prec_df.year <= 2016) & prec_df.station.isin(stat_num.value))

# Calculate total monthly precipitation for each station
total_monthly_precipitation = filtered_prec_df.groupBy('year', 'month', 'station').agg(F.sum('prec').alias('totalMonthlyPrec'))

# Calculate average monthly precipitation
monthly_avg_precipitation = total_monthly_precipitation.groupBy('year', 'month').agg(F.avg('totalMonthlyPrec').alias('avgMonthlyPrecipitation')).orderBy(['year', 'month'], ascending=[False, False])

# Ouput the results
monthly_avg_precipitation.rdd.saveAsTextFile("BDA/output/lab2-5/")


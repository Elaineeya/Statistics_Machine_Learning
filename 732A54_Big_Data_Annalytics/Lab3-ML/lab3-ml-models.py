import sys
import numpy as np
from pyspark import SparkContext
from pyspark.mllib.regression import LabeledPoint
from pyspark.mllib.regression import LinearRegressionWithSGD
from pyspark.mllib.regression import RidgeRegressionWithSGD
from datetime import datetime
from math import exp


sc = SparkContext(appName="lab3-3")

# Function to normalize date field to a floating format ranging from 0 to 100
def normalize_date(date):
    date_object = datetime.strptime(date, "%Y-%m-%d")
    start_of_year = datetime(date_object.year, 1, 1).timestamp()
    end_of_year = datetime(date_object.year, 12, 31).timestamp()
    normalized_date = (date_object.timestamp() - start_of_year) / (end_of_year - start_of_year) * 100
    return normalized_date

# Function to normalize time field to a floating format ranging from 0 to 100
def normalize_time(time):
    time_object = datetime.strptime(time, "%H:%M:%S")
    normalized_time = (time_object.hour * 3600 + time_object.minute * 60 + time_object.second) / 86400 * 100
    return normalized_time


a = 58.4274 # Up to you
b = 14.826 # Up to you
date = "2014-05-19" # Up to you

stations = sc.textFile("BDA/input/stations.csv")
temps = sc.textFile("BDA/input/temperature-readings-small.csv")

# Parse the stations and temperatures data
stations = stations.map(lambda line: line.split(";"))
temps = temps.map(lambda line: line.split(";"))

# Broadcast the stations data
stations = stations.map(lambda x : (x[0], (float(x[4]), float(x[3]))))
broadcast_stations = sc.broadcast(stations.collectAsMap())

# Filter out temperature measurements that are posterior to the day and hour of your forecast
filtered_temps = temps.filter(lambda x: datetime.strptime(x[1], "%Y-%m-%d") < datetime.strptime(date, "%Y-%m-%d"))

# Prepare data structure before building models
# (station, normalized_date, normalized_time, temp, lon, lat)
filtered_temps = filtered_temps.map(lambda x : x[0], 
                                               normalize_date(x[1]), 
                                               normalize_time(x[2]), 
                                               float(x[3]),
                                               broadcast_stations.value[x[0]])

# (temp, normalized_date, normalized_time, lon, lat)
labeled_data = filtered_temps.map(lambda x : LabeledPoint(x[3], [x[1], x[2],x[4][0], x[4][5]]))


linear_model = LinearRegressionWithSGD.train(labeled_data)

linear_predictions = {}
for time in ["00:00:00", "22:00:00", "20:00:00", "18:00:00", "16:00:00", "14:00:00",
"12:00:00", "10:00:00", "08:00:00", "06:00:00", "04:00:00"]:
    # Compute the forecast for each time
    linear_prediction = linear_model.predict([normalize_date(date), normalize_time(time), b, a,])
    linear_predictions[time] = linear_prediction

ridge_model = RidgeRegressionWithSGD.train(labeled_data)

ridge_predictions = {}
for time in ["00:00:00", "22:00:00", "20:00:00", "18:00:00", "16:00:00", "14:00:00",
"12:00:00", "10:00:00", "08:00:00", "06:00:00", "04:00:00"]:
    # Compute the forecast for each time
    ridge_forecast = ridge_model.predict([normalize_date(date), normalize_time(time), b, a,])
    ridge_predictions[time] = ridge_prediction

print ("Linear regression model prediction results:")
for k,v in linear_predictions.items():
    print ("{0}: {1}" .format(k,v))
#print (sum_predictions)
print ("Ridge regression model prediction results:")
#print (prod_predictions)
for k,v in ridge_predictions.items():
    print ("{0}: {1}" .format(k,v))

linear_output = sc.parallelize(list(linear_predictions.items()))
ridge_output = sc.parallelize(list(ridge_predictions.items()))

linear_output.saveAsTextFile("BDA/output/lab3/linear")
ridge_output.saveAsTextFile("BDA/output/lab3/ridge")


from __future__ import division
from math import radians, cos, sin, asin, sqrt, exp
from datetime import datetime
from pyspark import SparkContext
#from pyspark.mllib.regression import LabeledPoint, LinearRegressionWithSGD, DecisionTree

sc = SparkContext(appName="lab_kernel")

def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a))
    km = 6367 * c
    return km

def date_diff(date1, date2):
    return abs(datetime.strptime(date1, "%Y-%m-%d") - datetime.strptime(date2, "%Y-%m-%d")).days

def time_diff(time1, time2):
    return abs((datetime.strptime(time1, "%H:%M:%S") - datetime.strptime(time2, "%H:%M:%S"))).total_seconds() / 3600

def gau_kernel(dist_h, h):
    return exp(-(dist_h**2)/(2 * h**2))

h_distance = 300000 # Up to you
h_date = 6000 # Up to you
h_time = 4 # Up to you

a = 58.4274 # Up to you
b = 14.826 # Up to you
date = "2014-05-19" # Up to you

stations = sc.textFile("BDA/input/stations.csv")
temps = sc.textFile("BDA/input/temperature-readings-small.csv")

# Parse the stations and temperatures data
stations = stations.map(lambda line: line.split(";"))
temps = temps.map(lambda line: line.split(";"))

# Broadcast the stations data
station_dist = stations.map(lambda x : (x[0], haversine(b, a, float(x[4]), float(x[3]))))
broadcast_station_dist = sc.broadcast(station_dist.collectAsMap())

# Filter out temperature measurements that are posterior to the day and hour of your forecast
filtered_temps = temps.filter(lambda x: datetime.strptime(x[1], "%Y-%m-%d") < datetime.strptime(date, "%Y-%m-%d"))


# Cache the reuse data (temperature, distance_kernal, date_kernal, time)
cached_temps = filtered_temps.map(lambda x: (float(x[3]),
                                                gau_kernel(broadcast_station_dist.value[x[0]], h_distance),
                                                gau_kernel(date_diff(x[1], date), h_date),
                                                x[2])).cache()

sum_predictions = {}
prod_predictions = {}
for time in ["00:00:00", "22:00:00", "20:00:00", "18:00:00", "16:00:00", "14:00:00",
"12:00:00", "10:00:00", "08:00:00", "06:00:00", "04:00:00"]:
    # Compute the forecast for each time
    forecast = cached_temps.map(lambda x: (x[0], x[1], x[2], 
                                     gau_kernel(time_diff(x[3],time), h_time)))

    sum_forecast = forecast.map(lambda x: (x[1]+x[2]+x[3], x[0]*(x[1]+x[2]+x[3])))
    sum_forecast = sum_forecast.reduce(lambda a, b: (a[0] + b[0], a[1] + b[1]))

    prod_forecast = forecast.map(lambda x: (x[1]*x[2]*x[3], x[0]*(x[1]*x[2]*x[3])))
    prod_forecast = prod_forecast.reduce(lambda a, b: (a[0] + b[0], a[1] + b[1]))

    sum_predictions[time] = sum_forecast[1]/sum_forecast[0]
    prod_predictions[time] = prod_forecast[1]/prod_forecast[0]


print ("SUM kernal prediction results:")
for k,v in sum_predictions.items():
    print ("{0}: {1}" .format(k,v))
#print (sum_predictions)
print ("PROD kernal prediction results:")
#print (prod_predictions)
for k,v in prod_predictions.items():
    print ("{0}: {1}" .format(k,v))

sum_output = sc.parallelize(list(sum_predictions.items()))
prod_output = sc.parallelize(list(prod_predictions.items()))

sum_output.saveAsTextFile("BDA/output/lab3/sum")
prod_output.saveAsTextFile("BDA/output/lab3/prod")



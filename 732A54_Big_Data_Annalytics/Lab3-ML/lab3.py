from __future__ import division
from math import radians, cos, sin, asin, sqrt, exp
from datetime import datetime
from pyspark import SparkContext

sc = SparkContext(appName="lab_kernel")



def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2- lon1
    dlat = lat2- lat1
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a))
    km = 6367 * c
    return km

def dist_date(date_1, date_2):
    """
    Calculate the distance in days between two datetime objects
    """
    return abs(date_1 - date_2).days

def dist_hour(hour_1, hour_2):
    """
    Calculate the distance in hours 
    """
    return abs(hour_1 - hour_2)

def kernel_function(dist, h):
    """
    Calculate the kernel value based on distance and h
    """
    return exp(-(dist/h)**2)

def kernel_value(Dist_station, Dist_date, Dist_hour, method):
    """
    Calculate the kernel value using sum or prod
    """
    kernel_1 = kernel_function(Dist_station, h_distance)
    kernel_2 = kernel_function(Dist_date, h_date)
    kernel_3 = kernel_function(Dist_hour, h_time)

    if method == "sum":
        return  kernel_1 + kernel_2 + kernel_3
    if method == "prod":
        return  kernel_1 * kernel_2 * kernel_3

h_distance = 500
h_date = 10 
h_time = 2 

a = 58.4274 # lat
b = 14.826 # lon
interest_date = "2000-05-19"
interest_date = datetime.strptime(interest_date, "%Y-%m-%d")
interest_time = ["24:00:00", "22:00:00", "20:00:00", "18:00:00", "16:00:00", "14:00:00",
"12:00:00", "10:00:00", "08:00:00", "06:00:00", "04:00:00"]

stations = sc.textFile("BDA/input/stations.csv")
stations=stations.map(lambda line: line.split(";"))
# map to {No.station: dist_station}
stations=stations.map(lambda x:(x[0],haversine(b,a,float(x[4]),float(x[3])))) 
stations=sc.parallelize(stations.collect()).collectAsMap()   
stations_data = sc.broadcast(stations)  

temps = sc.textFile("BDA/input/temperature-readings.csv")
temps=temps.map(lambda line: line.split(";"))
# map to {No.station: (date, time, temp)}
temps=temps.map(lambda x:(x[0],(datetime.strptime(x[1],"%Y-%m-%d"),
                        int(x[2].split(":")[0]),
                        float(x[3]))))
# filter the date after interest_date
temps=temps.filter(lambda x: (x[1][0] < interest_date))

# join two datasets 
# map to {No.station: (dist_station, dist_date, time, temp)}
processed_data = temps.map(lambda x: (x[0],(stations_data.value[x[0]],
                            dist_date(x[1][0],interest_date),
                            x[1][1],
                            x[1][2]))).cache()

def calculate_temp(time, data, mode):    
    # extract hour only as int
    t = int(time.split(":")[0])
    # map to {kernel_value: temp}
    data1=data.map(lambda x:(kernel_value(x[1][0],x[1][1],dist_hour(t,x[1][2]), method=mode),x[1][3]))
    # map to {kernel_value: kernel_value*temp}
    data2=data1.map(lambda x:(x[0], x[0]*x[1]))
    # map to {sum of kernel_value: sum of kernel_value*temp}
    data3=data2.reduce(lambda x,y:(x[0]+y[0], x[1]+y[1]))
    
    return round(data3[1]/data3[0],1)


temp_by_sum = [calculate_temp(time, processed_data, 'sum') for time in interest_time]
sum_output = list(zip(interest_time, temp_by_sum))
print ("SUM kernel prediction results:")
print(sum_output)

temp_by_prod = [calculate_temp(time, processed_data, 'prod') for time in interest_time]
prod_output = list(zip(interest_time, temp_by_prod))
print ("PROD kernel prediction results:")
print(prod_output)

#sum_output.saveAsTextFile("BDA/output/lab3/sum")



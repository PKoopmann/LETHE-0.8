#! /usr/bin/python

import re, datetime

sizeFile = open("statisticSize.txt", 'w')
clausesFile = open("statisticClauses.txt", 'w')
durationsFile = open("statisticDuration.txt", 'w')

reInputSize = re.compile('Size: (\d+)')
reOutputSize = re.compile('Size of the output: (\d+)')
                             
reInputClauses = re.compile('Number of clauses to be processed: (\d+)')
reOutputClauses = re.compile('Number of clauses after forgetting: (\d+)')

reStartTime = re.compile('Started at (.+)')
reFinishTime = re.compile('Finished at (.+)')

reNotFinished = re.compile('Execution has not been completed.')

        # Thu May 09 17:22:18 BST 2013
timeFormat = "%a %b %d %H:%M:%S %Z %Y"

lastSize = ""
lastClauses = ""
lastStart = None

durations = [] # list of size-duration tuples, for later cumulative distribution file

finished = False

for line in open("results.txt"):

    if(reNotFinished.match(line) != None):
        finished=False

    match = reInputSize.match(line)
    if(match!=None):
        lastSize = match.group(1)

    match = reInputClauses.match(line)
    if(match!=None):
        lastClauses = match.group(1)

    match = reOutputSize.match(line)
    if(match!=None and finished):
        newSize = match.group(1)
        sizeFile.write(lastSize + " " + newSize + "\n")

    match = reOutputClauses.match(line)
    if(match!=None and finished):
        newClauses = match.group(1)
        clausesFile.write(lastClauses + " " + newClauses + "\n")

    match = reStartTime.match(line)
    if(match!=None):
        timeString = match.group(1) 

        lastStart = datetime.datetime.strptime(timeString, timeFormat)  


    match = reFinishTime.match(line)
    if(match!=None):
        timeString = match.group(1) 

        finishTime = datetime.datetime.strptime(timeString, timeFormat)  
        duration = (finishTime-lastStart).total_seconds()

        if(finished):
            durationsFile.write(lastSize + " " + str(duration) + "\n")
            durations.append(duration)
        else:
            finished = True # reset


sizeFile.close()
clausesFile.close()
durationsFile.close()


# Now create cumulative distribution of durations

durations = sorted(durations)

map = {}
count = 0

for duration in durations:
    count+=1

    map[duration]=count

cumulativeDurationsFile = open("statisticCumulativeDurations.txt",'w')

for (duration, count) in sorted(map.items(), lambda x,y: cmp(x[0], y[0])):
    cumulativeDurationsFile.write(str(duration)+" "+str(count)+"\n")

cumulativeDurationsFile.close()

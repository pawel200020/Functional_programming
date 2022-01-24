f:: [Double]->Double
f[] = 1
f list = (((+).(/) 1).(f.tail)) list $ head list


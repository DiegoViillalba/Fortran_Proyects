import random

# ---- Family List

# 1 -> González 
# 2 -> Díaz
# 3 -> Romero
# 4 -> Villalba 

names = ['Rosy','Antonio','Sofia', 'Eduardo','Diego', 'Yanira', 'Lupe','Gioviss', 'Ivanna', 'Danna', 'Teo', 'Emanuel', 'Sara', 'Yerine', 'Fatima', 'Jany', 'Pablo','Fernanda']


# Structure of the array
# [name,family,past]

persons = [[0,1,7],[1,1,8],[2,1,100],[3,1,17],[4,1,14],[5,2,4],[6,2,11],[7,2,12],[8,2,100],[9,2,15],[10,3,3],[11,3,2],[12,3,16],[13,3,1],[14,3,9],[15,4,6],[16,4,10],[17,4,13]]


# Algorithm
n = []
for i in range(0,len(names)):
  n.append(i)
  
print(n)

for i in range(0,len(names)):
  r = random.choice(n)
  while r == persons[i][2] or persons[i][1] == persons[r][1]:
    r = random.choice(n)
    if r != persons[i][2] and persons[i][1] != persons[r][1]:
      break
  n.remove(r)
  print(i,names[persons[i][0]], r,names[persons[r][0]])



  


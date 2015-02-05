'''
Created on 30 jan 2015

@author: grandmother
'''

import Image
import gc

def color_thing(bob):
    #print bob
    #bob = 1-bob
    if (bob < 1/3.0 ):
        return (int(bob*255*3),0,0)
    elif (bob > 1/3.0 and bob < 2/3.0):
        return (255,int((bob-(1/3.0))*255*3),0)
    else:
        return (255,255,int((bob-(2/3.0))*255*3))
    

def lol():
    big_string = open("lol.txt","r").read()
    splitted = big_string.split("\n")
    max_thing = int(splitted[0])
    width = int(splitted[1])
    splitted = splitted[2:-1]
    #print width
    #print splitted
    data = [[0]*width]*width
    print len(splitted)
    i  = 0
    for line in splitted:
        #print line.split(",")
        data_line = map(int,line.split(","))
        data[data_line[0]] = data_line[1:]
        #print i 
        i = i+ 1 
    
    print "done with splittin and such"
    gc.collect()
    #print data
    img = Image.new("RGB", (width,width))
    dudes =  [[(0,0,0)]*width]*width
    pixels = img.load()
    
    print "building file"
    index = 0
    for i in range(img.size[0]):   
        #print i
        for j in range(img.size[1]):
            if(data[i][j] > max_thing):
                color = 0
            else:
                color = (float(data[i][j])/float(max_thing))
                
            pixels[i,j] = color_thing(color)
            index = index + 1  
    print "done"
    
    #img.show()
    img.save("mandel.png")
if __name__ == '__main__':
    lol()
    
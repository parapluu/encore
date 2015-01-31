'''
Created on 30 jan 2015

@author: grandmother
'''

import Image

def lol():
    big_string = open("lol.txt","r").read()
    splitted = big_string.split(",")
    width = int(splitted[0])
    data = map(int, splitted[1:])
    
    img = Image.new("RGB", (width,width))
    pixels = img.load()
    
    index = 0
    for i in range(img.size[0]):   
        for j in range(img.size[1]):
            pixels[i,j] = (data[index]*255, data[index]*255, data[index]*255)
            index = index + 1  
    
    
    img.save("mandel.png")
if __name__ == '__main__':
    lol()
    
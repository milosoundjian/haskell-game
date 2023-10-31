#CONVERTS maps that are in image form into easy to use lists of coordinates
#allows for designing maps on other software and easily exporting them to game
from PIL import Image
import sys
import json
import pprint

if __name__ == "__main__" : 
    filename = sys.argv[1]
    output_filename = "positions.txt"

    pos_dict = dict()

    image = Image.open(filename)
    pixels = image.load()
    w, h = image.size


    for x in range(w):
        for y in range(h):
            cur_rgb = image.getpixel((x, y))
            if str(cur_rgb) in pos_dict.keys() : 
                pos_dict[str(cur_rgb)].append((x, h - y - 1))
            else:
                pos_dict[str(cur_rgb)] = [(x, h - y - 1)]

    with open(output_filename, "w") as file : 
        file.write(pprint.pformat(pos_dict, compact=True) )
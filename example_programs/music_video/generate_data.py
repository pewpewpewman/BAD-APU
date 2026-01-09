import os
import yt_dlp
import cv2 as cv
import sys
import time
import numpy as np

file_name = "bad_apple.mp4"

if not os.path.exists(file_name):
    ydl_opts = {
        "format" : "134",
        "outtmpl" : file_name
    }

    with yt_dlp.YoutubeDL(ydl_opts) as ydl:
        ydl.download("https://www.youtube.com/watch?v=FtutLA63Cp8")

cap = cv.VideoCapture(file_name)

#Uncomment to play bad apple in your terminal (run mpg123 with the bad apple
#audio as a background task for a better experience)
#
#while True:
#
#    start_time = time.time()
#
#    ret, color_frame = cap.read()
#
#    if not ret:
#        print("ITS OVER!")
#        sys.exit()
#
#    frame = cv.cvtColor(color_frame, cv.COLOR_BGR2GRAY)
#
#    os.system('clear')
#    for row in range(0, 64):
#        for col in range(0, 48):
#            pix = frame[int(row * frame.shape[0] / 64)][int(col * frame.shape[1] / 48)] 
#            print( "#" if pix < 255 else " ", end = "")
#        print()
#
#    time.sleep(max(0, 1/30 - (time.time() - start_time)))


#progrees to a random frame
#for _ in range(2199):
#    cap.read()

ret, prev_color_frame = cap.read()
prev_frame = cv.cvtColor(prev_color_frame, cv.COLOR_BGR2GRAY)
#cv.imshow("PREV", prev_frame)
#cv.waitKey(0)

pixel_color = False #False is black, white is True
color_run_length = 0
color_rle_encode = []

inverting = False
difference_run_length = 0
difference_rle_encode = []

start_time = time.time()

while True:


    for _ in range(12):
        cap.read()
    ret, color_frame = cap.read()

    if not ret:

        break

    frame = cv.cvtColor(color_frame, cv.COLOR_BGR2GRAY)

    frame_rle = []

    for i in range(0, 48):
        for j in range(0, 64):
            row = int(i * frame.shape[0] / 48)
            col = int(j * frame.shape[1] / 64)
            
            prev = round(prev_frame[row][col] / 255)
            curr = round(frame[row][col] / 255)

            #print(0 if prev == curr else 1, end = "")

            #difference run length encode
            if prev == curr and not inverting:
                difference_run_length += 1
            elif prev != curr and inverting:
                difference_run_length += 1
            elif prev != curr and not inverting:
                #print(difference_run_length)
                difference_rle_encode.append(difference_run_length)
                difference_run_length = 1
                inverting = True
            elif prev == curr and inverting:
                #print(difference_run_length)
                difference_rle_encode.append(difference_run_length)
                difference_run_length = 1
                inverting = False

            #print(" " if curr != 0 else "#", end = "")

            #regular old run length encoding
            if curr == pixel_color:
                color_run_length += 1
            else:
                pixel_color = curr
                color_rle_encode.append(color_run_length)
                frame_rle.append(color_run_length)
                color_run_length = 1

        #print("")



    #print(frame_rle)
    #start_time = time.time()
    #os.system("clear")
    prev_frame = frame


#print("Color RLE length:", len(color_rle_encode))
#
#max = 0
#for i in range(len(color_rle_encode)):
#    if color_rle_encode[i] > max:
#        max = color_rle_encode[i]
#
#print("Color rle encode max value:", max)
#
#print("Difference RLE len:", len(difference_rle_encode))
#
#max = 0
#for i in range(0, len(difference_rle_encode), 1):
#    if difference_rle_encode[i] > max:
#        max = difference_rle_encode[i]
#
#print("Don't invert max value:", max)
#
#max = 0
#for i in range(1, len(difference_rle_encode), 1):
#    if difference_rle_encode[i] > max:
#        max = difference_rle_encode[i]
#
#print("Invert max value:", max)
#
#print("ITS OVER!")

cap.release()
cv.destroyAllWindows()

#file = open("data.txt", "w")
#file.write(str(color_rle_encode) + "\n")
#file.write(str(len(color_rle_encode)) + "\n")
#file.close

rle = color_rle_encode

rle_idx = 0

color = False

start = time.time()

while rle_idx < len(rle):

    for r in range(0, 48):
        for c in range(0, 64):

            #Catch end of video out of bound err
            if rle_idx >= len(rle):
                continue

            if rle[rle_idx] == 0:
                rle_idx += 1
                if rle_idx >= len(rle):
                    continue
                color = not color

            print(" " if color else "#", end = "")

            rle[rle_idx] -= 1

        print("")

    time.sleep((13 / 30) - (time.time() - start))
    start = time.time()
    os.system("clear")


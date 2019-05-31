import serial
import numpy as np
import socket
import threading
import queue
import time
import sys

SAMPLE_WIDTH=2
CHANNEL_NUM=4
BUFFER_SIZE=5000
HOST='127.0.0.1'
PORT=1234

BUFFER_CNT=10

def sendHeader(client, time):
    client.send(bytearray([time&255, (time>>8)&255, (time>>16)&255, (time>>24)&255]))

def sendData(client, data):
    client.send(bytearray(data))

class TCPThread(threading.Thread):
    def __init__(self, client):
        threading.Thread.__init__(self,None, 'TCPThread', 1)
        self.client=client
        self.queue=queue.Queue(BUFFER_CNT)
    def push(self,data):
        self.queue.put(data)
    def run(self):
        self.client.send(bytearray([0]))
        while(True):
            data=self.queue.get()
            if data==[]:
                break
            t=(int(time.time()*1000)&0xffffffff)
            sendHeader(self.client, t)
            sendData(self.client, data)
        self.client.close()

def openTCP():
    client=socket.socket()
    client.connect((HOST, PORT))
    thread=TCPThread(client)
    thread.start()
    return thread

def sendTCP(thread, data):
    thread.push(data)

def open():
    ser = serial.Serial('/dev/cu.usbmodem1411')
    return ser


def run():
    thread=openTCP()
    time.sleep(1)
    ser=open()
    try:
        while(True):
            data=ser.read(SAMPLE_WIDTH*CHANNEL_NUM*BUFFER_SIZE)
            sendTCP(thread, data)
            #print(";")
    except KeyboardInterrupt:
        sendTCP(thread, [])
        thread.join()
        sys.exit()

run()
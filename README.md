# MilliSonic

The companion code for paper *MilliSonic: Pushing the Limit of Acoustic Motion Tracking*, appearing in *CHI 2019*.
The server code is written in Scala and developed in IntellijIdea.

For the microphone array hardware, please find the arduino code in *Arduino* folder, and connect the four microphones to A0-A3 of an Arduino Due board.

File *SerialTCP.py* is used to read the serial input from the microphone array and send it to the server via TCP.

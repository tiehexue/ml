#!/bin/bash

git pull

activator assembly

java -Xms1024m -jar target/scala-2.11/DeepLearning-assembly-1.0.jar com.wangyz.Main &



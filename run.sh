#!/bin/bash

git pull

activator assembly

java -Xms2048m -Xmx4096m -XX:+CMSClassUnloadingEnabled -jar target/scala-2.11/DeepLearning-assembly-1.0.jar com.wangyz.Main > out &



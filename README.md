# itechart-homework

## Starting application using Docker
At first, we should create Docker Image that we will run. We can make it entering the following command to IDE terminal:
### sbt docker:publishLocal
Then we open cmd and enter the following command to start the container:
### docker run --rm -p8080:8080 homeworkkk2:1.0
Starting GuessClient from IDE we can make sure that connection with server is established
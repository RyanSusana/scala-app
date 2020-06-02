https://medium.com/@ryansusana/learning-functional-programming-with-scala-ccc7bf68214f

# Description
In my last week picking up Scala, I decided to build an application that runs on the cloud. I decided to do so in the form of a cloud function, considering that this is functional programming 😉. I wanted to create a service that analyses the contents of Word, PDF and Text files and extracts key information about those files. The program would essentially perform sentiment analysis on the text content of the file and respond with the following contents:
- The language of the file
- The sentence with the highest magnitude
- The most positive sentence
- The most negative sentence
- The general tone of the document

This service is hosted on Google Cloud, feel free to check out my pipeline in the `cloudbuild.yaml` file

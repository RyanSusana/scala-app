package com.ryansusana.scala

import java.util.stream.Collectors

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}
import com.google.cloud.language.v1beta2.Document.Type
import com.google.cloud.language.v1beta2.{Document, LanguageServiceClient, Sentence}

import scala.io.Source
import scala.jdk.CollectionConverters._

class Main extends HttpFunction {
  val language: LanguageServiceClient = LanguageServiceClient.create

  def service(request: HttpRequest, response: HttpResponse): Unit = {

    // Request.getParts is a Map<String, Part>

    val contentType = request.getContentType.orElse("application/json")

    val writer = response.getWriter

    val filenames =
      if (contentType contains "multipart")
        request.getParts.entrySet().stream()
          .map { e => e.getKey } collect Collectors.joining(",")
      else
        "none"


    if (!(contentType contains "multipart") || request.getParts.isEmpty) {
      writer.write("No files provided")
    } else {

      // Perform sentiment analysis
      val details = request.getParts.asScala.values.map(partToDetail).mkString("\n---\n")
      writer.write(details)

    }
  }

  def partToDetail(part: HttpRequest.HttpPart): String = {

    val source = Source.fromInputStream(part.getInputStream).mkString


    detailText(part.getFileName.orElse("unknown"), source)
  }


  def detailText(fileName: String, input: String): String = {
    val doc = Document.newBuilder.setContent(input).setType(Type.PLAIN_TEXT).build

    val sentiment = language.analyzeSentiment(doc)

    val mostImpactfulSentence = sentiment.getSentencesList
      .asScala
      .max { (s1: Sentence, s2: Sentence) => s1.getSentiment.getMagnitude compareTo s2.getSentiment.getMagnitude }
      .getText.getContent

    s"""
       |File Name: $fileName
       |Most impactful sentence: $mostImpactfulSentence
    """.stripMargin

  }
}

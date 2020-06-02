package com.ryansusana.scala

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}
import com.google.cloud.language.v1beta2.Document.Type
import com.google.cloud.language.v1beta2.{Document, LanguageServiceClient, Sentence}
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor

import scala.io.Source
import scala.jdk.CollectionConverters._

class Main extends HttpFunction {


  def service(request: HttpRequest, response: HttpResponse): Unit = {
    val contentType = request.getContentType.orElse("application/json")

    val writer = response.getWriter
    if (!(contentType contains "multipart") || request.getParts.isEmpty) {
      writer.write("No files provided")
    } else {
      // Perform sentiment analysis
      try {
        val details = request.getParts.asScala.values
          .map(toContentDetails)
          .mkString("\n---\n");
        writer.write(details);
      } catch {
        case e: Exception => {

          e.printStackTrace();
          response.setStatusCode(500);
          writer.write(e.getMessage)
        };
      }
    }
  }

  def toContentDetails(p: HttpRequest.HttpPart): String = partToDetail(fileType(p))(p)

  def fileType(part: HttpRequest.HttpPart): HttpRequest.HttpPart => String = {
    val contentType = "([A-Za-z]+)/([A-Za-z]+)".r
    part.getContentType.orElse("none/none") match {
      case contentType(_, "pdf") => pdf
      case contentType("text", _) => textFile
      case contentType(_, "text") => textFile
      case _ => throw new IllegalArgumentException(s"${part.getContentType} not allowed")
    }
  }

  def pdf(part: HttpRequest.HttpPart): String = {
    val reader = new PdfReader(part.getInputStream)

    (1 to reader.getNumberOfPages)
      .map { e => PdfTextExtractor.getTextFromPage(reader, e) }
      .mkString
  }

  def textFile(part: HttpRequest.HttpPart): String =
    Source.fromInputStream(part.getInputStream).mkString


  def partToDetail(getString: HttpRequest.HttpPart => String)(part: HttpRequest.HttpPart): String =
    detailText(part.getFileName.orElse("unknown"), getString(part))


  def detailText(fileName: String, input: String): String = {
    def happy(s1: Sentence, s2: Sentence) =
      s1.getSentiment.getScore compareTo s2.getSentiment.getScore

    def sad(s1: Sentence, s2: Sentence) =
      s2.getSentiment.getScore compareTo s1.getSentiment.getScore

    def magnitude(s1: Sentence, s2: Sentence) =
      s1.getSentiment.getMagnitude compareTo s2.getSentiment.getMagnitude

    def most(f: (Sentence, Sentence) => Int)(sentences: Iterable[Sentence]): String =
      sentences.max { (s1, s2) => f(s1, s2) }.getText.getContent

    val language: LanguageServiceClient = LanguageServiceClient.create

    val doc = Document.newBuilder.setContent(input).setType(Type.PLAIN_TEXT).build
    val sentiment = language.analyzeSentiment(doc)

    val sentences = sentiment.getSentencesList
      .asScala

    val mostImpactfulSentence = most(magnitude)(sentences)
    val mostHappySentence = most(happy)(sentences)
    val mostSadSentence = most(sad)(sentences)

    val score = sentiment.getDocumentSentiment.getScore

    val toneOfText = if (score > 0.2) "positive" else if (score <= 0.2) "negative" else "neutral"

    s"""
       |File Name: $fileName
       |Most impactful sentence: $mostImpactfulSentence
       |Most happy sentence: $mostHappySentence
       |Most sad sentence: $mostSadSentence
       |The general tone of the text is $toneOfText.
       |Language: ${sentiment.getLanguage}
    """.stripMargin

  }
}
